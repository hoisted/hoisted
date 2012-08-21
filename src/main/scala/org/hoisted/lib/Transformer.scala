package org.hoisted.lib

import net.liftweb.common._
import org.joda.time.DateTime


/**
 * Created with IntelliJ IDEA.
 * User: dpp
 * Date: 8/1/12
 * Time: 11:01 AM
 * To change this template use File | Settings | File Templates.
 */


object TransformTest extends LazyLoggableWithImplicitLogger {
  def fromMetadata(md: MetadataValue): TransformTest = {
    listFromMetadata(md) match {
      case x :: Nil => x
      case Nil => BooleanTest(true)
      case xs => AndTransformTest(xs :_*)
    }
  }

  def listFromMetadata(md: MetadataValue): List[TransformTest] = {
    md match {
      case BooleanMetadataValue(b) => List(BooleanTest(b))
      case ListMetadataValue(lst) => lst.flatMap(listFromMetadata(_))
      case KeyedMetadataValue(pairs) => pairs.toList.flatMap{
        case (NotTestKey, md) => listFromMetadata(md).map(test => NotTransformTest(test))
        case (AndTestKey, md) => List(AndTransformTest(listFromMetadata(md) :_*))
        case (OrTestKey, md) => List(OrTransformTest(listFromMetadata(md) :_*))
        case (HasTagTestKey, md) => md.asString.toList.map(HasTagTest(_))
        case (PathPrefixTestKey, md) => md.asString.toList.map(TestPathStartsWith(_))
        case (FileSuffixTestKey, md) => md.asString.toList.map(TestFileSuffix(_))
        case (HasMetadataTestKey, md) => md.asString.toList.map(s => TestHasMetadata(MetadataKey(s)))
        case (EqStrTestKey, ListMetadataValue(k :: v :: _)) =>
          for {
            kstr <- k.asString.toList
            vstr <- v.asString
          } yield TestEqStringMetadata(MetadataKey(kstr), vstr)

        case (ContainsStrTestKey,  ListMetadataValue(k :: v :: _)) =>
          for {
            kstr <- k.asString.toList
            vstr <- v.asString
          } yield TestContainsStringMetadata(MetadataKey(kstr), vstr)
        case other =>
          Nil
      }
      case other =>
        Nil
    }
  }
}

case class BooleanTest(result: Boolean) extends TransformTest {
  def test(pf: ParsedFile): Boolean = result

  override def toString = "Bool "+result
}

trait TransformTest extends Function1[ParsedFile, Boolean] {
  def apply(in: ParsedFile): Boolean = test(in)
  def test(pf: ParsedFile): Boolean
}


case class NotTransformTest(other: TransformTest) extends TransformTest {
  def test(pf: ParsedFile): Boolean = !other.test(pf)
  override def toString = "Not("+other+")"
}

case class OrTransformTest(others: TransformTest*) extends TransformTest {
  private lazy val lst: List[TransformTest] = others.toList
  def test(pf: ParsedFile): Boolean = {
    @scala.annotation.tailrec
    def test(cur: Boolean, rest: List[TransformTest]): Boolean = {
      if (cur) true
      else rest match {
        case Nil => false
        case x :: rest => test(x.test(pf), rest)
      }
    }

    test(false, lst)
  }
  override def toString = "Or("+others.map(_.toString()).mkString(", ")+")"
}

case class AndTransformTest(others: TransformTest*) extends TransformTest {
  private lazy val lst: List[TransformTest] = others.toList
  def test(pf: ParsedFile): Boolean = {
    @scala.annotation.tailrec
    def test(cur: Boolean, rest: List[TransformTest]): Boolean = {
      if (!cur) false
      else rest match {
        case Nil => true
        case x :: rest => test(x.test(pf), rest)
      }
    }

    test(true, lst)
  }
  override def toString = "And("+others.map(_.toString()).mkString(", ")+")"
}

case class HasTagTest(tag: String) extends TransformTest {
  def test(pf: ParsedFile): Boolean = {
    pf.findData(TagsKey).
      map(_.forceListString.map(_.toLowerCase.trim).
      contains(tag.toLowerCase.trim)) openOr false
  }

  override def toString = "HasTag("+tag+")"
}

case class TestHasMetadata(key: MetadataKey) extends TransformTest {
  def test(pf: ParsedFile): Boolean =
    pf.findData(key).isDefined
  override def toString = "HasMetadata("+key+")"

}

case class TestEqStringMetadata(key: MetadataKey, str: String) extends TransformTest {
  def test(pf: ParsedFile): Boolean =
    pf.findData(key).map(_.forceString.trim.toLowerCase) == Some(str.trim.toLowerCase)

  override def toString = key.toString+" == "+str

}

case class TestContainsStringMetadata(key: MetadataKey, str: String) extends TransformTest {
  def test(pf: ParsedFile): Boolean =
    pf.findData(key).map(_.forceListString.map(_.trim.toLowerCase).contains(str)).openOr(false)

  override def toString = key.toString+" contains "+str
}


case class TestEqMetadata(key: MetadataKey, value: MetadataValue) extends TransformTest {
  def test(pf: ParsedFile): Boolean =
    pf.findData(key).map(v => v.testEq(value)).openOr(false)

  override def toString = key.toString+" == "+value
}


case class TestPathStartsWith(str: String) extends TransformTest
{
  def test(pf: ParsedFile): Boolean = pf.fileInfo.pathAndSuffix.path match {
    case v :: _ if v.trim.toLowerCase == str.trim.toLowerCase => true
    case _ => false
  }

  override def toString = "Path Starts with "+str
}

case class TestFileSuffix(str: String) extends TransformTest
{
  def test(pf: ParsedFile): Boolean = {
    val res = pf.fileInfo.pathAndSuffix.suffix.map(_.toLowerCase.trim) == Some(str.toLowerCase.trim)
    res
  }
  override def toString = "File suffix "+str
}


object Transformer {
  def fromMetadata(md: MetadataValue): Transformer = {
    listFromMetadata(md) match {
      case x :: Nil => x
      case Nil => PassThruTransform
      case xs => CondTransform(xs)
    }
  }


  def listFromMetadata(md: MetadataValue): List[Transformer] = {
    def wrap(lst: List[Transformer], md: MetadataValue): List[Transformer] = {
      TransformTest.listFromMetadata(md) match {
        case Nil => lst
        case test :: Nil => List(TestAndTransform(test, lst))
        case tests => List(TestAndTransform(AndTransformTest(tests :_*), lst))
      }
    }

    md match {
      case StringMetadataValue(UpdatePathRootXFormKey(_)) => List(UpdatePathRootTransform)
      case StringMetadataValue(DateFromPathXFormKey(_)) => List(DateFromPathTransform)
      case ListMetadataValue(lst) => lst.flatMap(listFromMetadata(_))
      case KeyedMetadataValue(pairs) =>
        pairs.toList.flatMap{
        case (UpdatePathRootXFormKey, md) => wrap(List(UpdatePathRootTransform), md)
        case (DateFromPathXFormKey, md) => wrap(List(DateFromPathTransform), md)
        case (RemovePathPrefixXFormKey, md) => md.forceListString match {
          case Nil => Nil
          case xs => wrap(List(RemovePathPrefixPathTransform(xs)), md)
        }
        case (PrependPathXFormKey, md) => md.forceListString match {
          case Nil => Nil
          case xs => wrap(List(PrependPathTransform(xs)), md)
        }
        case (SetValueXFormKey, ListMetadataValue(key :: value :: rest)) =>
        wrap(key.asString.toList.map(k =>
            SetValueOnTestTransform(MetadataKey(k), value, BooleanTest(true) )), ListMetadataValue(rest))

        case other =>
          Nil
      }
      case other =>
        Nil
    }
  }

}

trait Transformer extends Function1[ParsedFile, ParsedFile] {
  def apply(in: ParsedFile): ParsedFile = transform(in)
  def transform(in: ParsedFile): ParsedFile
  def transformsKey(toTest: MetadataKey): Boolean
}


case class TestAndTransform(test: TransformTest, transforms: Seq[Transformer]) extends Transformer {
  def transformsKey(toTest: MetadataKey): Boolean = transforms.find(_.transformsKey(toTest)).isDefined
  def transform(in: ParsedFile): ParsedFile =
    if (!test.test(in)) in else transforms.foldLeft(in)((pf, t) => t.transform(pf))

  override def toString = "TestAndTransform("+test+", "+transforms.map(_.toString).mkString(", ")+")"
}

case object UpdatePathRootTransform extends Transformer {
  private def env = HoistedEnvironmentManager.value
  def transformsKey(toTest: MetadataKey): Boolean = toTest == OutputPathKey

  def transform(in: ParsedFile): ParsedFile = {

    def runTest(first: ParsedFile => Boolean,
                key: MetadataKey): Box[ParsedFile] = {
      Full(in).
        filter(first).
        flatMap(pf =>
        env.findMetadata(key).flatMap(_.asString).map(root => (pf, root))).
      filter{case (pf, root) => !env.computeOutputFileName(pf).startsWith(root)}.
      map{case (pf, root) =>
        val newPath = root + env.computeOutputFileName(pf)
        pf.updateMetadata(OutputPathKey, newPath)
      }
    }

    runTest(env.isBlogPost, BlogRootKey) or
    runTest(env.isArticle, ArticleRootKey) or
    runTest(env.isEvent, EventRootKey) openOr in
  }

  override def toString = "UpdatePathRoot"
}

object CondTransform {
  def apply(maybeTrans: => Iterable[Transformer]): CondTransform = new CondTransform(maybeTrans)
}
class CondTransform(maybeTrans: => Iterable[Transformer]) extends Transformer {
  def transformsKey(toTest: MetadataKey): Boolean = maybeTrans.find(_.transformsKey(toTest)).isDefined
  def transform(in: ParsedFile): ParsedFile = maybeTrans.foldLeft(in)((pf, t) => t.transform(pf))

  override def toString = "CondTransform("+maybeTrans.map(_.toString()).mkString(", ")+")"
}

case object PassThruTransform extends CondTransform(Nil)

case class RemovePathPrefixPathTransform(prefix: List[String]) extends Transformer {
  def transformsKey(toTest: MetadataKey): Boolean = false
  def transform(in: ParsedFile): ParsedFile =
    if (in.fileInfo.pathAndSuffix.path.startsWith(prefix)) {
      in.morphPath(_.drop(prefix.length))
    } else in
}

case class PrependPathTransform(prefix: List[String]) extends Transformer {
  def transformsKey(toTest: MetadataKey): Boolean = false
  def transform(in: ParsedFile): ParsedFile =
  in.morphPath(f => prefix ::: f)
}

case object DateFromPathTransform extends Transformer {
  private lazy val rx = """^([0-9]{2,4}-[0-9]{1,2}-[0-9]{1,2})-+(.*)""".r

  def transformsKey(toTest: MetadataKey): Boolean = toTest == DateKey

  def transform(in: ParsedFile): ParsedFile = {
    if (in.findData(DateKey).isDefined) in else {
      in.findData(ValidFromKey).flatMap(_.asDate) match {
        case Full(d) => in.updateMetadata(DateKey, d)
        case _ =>
          val dated = in.fileInfo.pathAndSuffix.path.
            zipWithIndex.map(str => (str._2, str._1, DateUtils.uglyParseDate(str._1)))

          dated.filter(_._3.isDefined).reverse match {
            case (pos, cur, Full(date)) :: _ =>
              val nfi = in.fileInfo.pathAndSuffix.copy(path =
                dated.map{
                  case (p2, name, Full(_)) if p2 == pos =>
                     rx.findFirstMatchIn(name).map(_.group(2).trim match {
                       case "" => "date"
                       case x => x
                     }) getOrElse name
                  case x => x._2
                })
              in.updateMetadata(DateKey, date).updateFileInfo(nfi.toFileInfo(in.fileInfo.file))
            case _ =>
              in.updateMetadata(DateKey,
              in.fileInfo.file.map(ff => new DateTime(ff.lastModified())).getOrElse(new DateTime()): DateTime)
          }
      }
    }
  }
}

case class SetValueOnTestTransform(key: MetadataKey,
                                   newVal: MetadataValue,
                                   test: TransformTest,
                                   ignoreIfKeyExists: Boolean = true) extends Transformer {
  def env = HoistedEnvironmentManager.value

  def transformsKey(toTest: MetadataKey): Boolean = toTest == key

  def transform(in: ParsedFile): ParsedFile =
  if (ignoreIfKeyExists && in.findData(key).isDefined) in else {
    in match {
      case toChange: ParsedFile =>
        if (test.test(toChange)) {
          toChange.updateMetadata(toChange.metaData.addKey(key, newVal))
        } else {
          in
        }
      case _ => in
    }
  }

  override def toString = "SetValueOnTest("+key+", "+newVal+", "+test+")"
}


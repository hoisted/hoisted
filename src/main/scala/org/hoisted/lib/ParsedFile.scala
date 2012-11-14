package org.hoisted.lib

import net.liftweb._
import common._
import common.Full
import util._
import Helpers._
import scala.xml._
import java.io._
import org.joda.time.{ DateTime}
import collection.mutable.ListBuffer
import org.apache.tika.parser.AutoDetectParser
import org.apache.tika.sax.ToHTMLContentHandler
import org.apache.tika.metadata.Metadata
import javax.swing.text.html.StyleSheet.BoxPainter

/**
 * Created with IntelliJ IDEA.
 * User: dpp
 * Date: 6/8/12
 * Time: 12:02 PM
 * To change this template use File | Settings | File Templates.
 */

object ParsedFile extends LazyLoggableWithImplicitLogger {



  def findBody(in: NodeSeq): NodeSeq =
    (in \ "content").headOption.map(_.child) getOrElse in


  def apply(fi: FileInfo): Box[ParsedFile] = {
    fi.suffix.map(_.toLowerCase) match {
      case Some("yaml") =>
        for {
          realFile <- fi.file
          fis <- HoistedUtil.logFailure("Trying to open file "+realFile)(new FileInputStream(realFile))
          yaml <- HoistedUtil.logFailure("Trying to parse "+fi.pathAndSuffix.display)(new String(Helpers.readWholeStream(fis), "UTF-8"))
          _ <- HoistedUtil.logFailure("Trying to close stream for file "+realFile)(fis.close())
          metaData <- HoistedUtil.reportFailure("Parsing YAML file "+fi.pathAndSuffix.display)( YamlUtil.parse(yaml))
        } yield YamlFile(fi, metaData)

      case Some("xhtml") | Some("cms.xml") =>
        for {
          realFile <- fi.file
          fis <- HoistedUtil.logFailure("Trying to open file "+realFile)(new FileInputStream(realFile))
          xml <- PCDataXmlParser(fis)
          _ <- HoistedUtil.logFailure("Trying to close stream for file "+realFile)(fis.close())
          metaData = findXmlMetaData(xml)
        } yield XmlFile(fi, findBody(xml), xml, metaData)

      case Some("html") | Some("htm") =>
        for {
          realFile <- fi.file
          fis <- HoistedUtil.logFailure("Trying to open file "+realFile)(new FileInputStream(realFile))
          bytes <- HoistedUtil.logFailure("Reading "+realFile)(Helpers.readWholeStream(fis))
          str = new String(bytes, "UTF-8")
          (str2, info) = MarkdownParser.readTopMetadata(str, false)
          html <- parseHtml5File(str2)
          _ <- HoistedUtil.logFailure("Closing "+realFile)(fis.close())
        } yield HtmlFile(fi, html, info)

      case Some("md") | Some("mkd") =>
        println("Reading "+fi.pathAndSuffix.display)
        try {
        for {
          realFile <- fi.file
          whole <- HoistedUtil.logFailure("Reading "+realFile)(Helpers.readWholeFile(realFile))
          str = new String(whole, "UTF-8")
          (elems, rawMeta) <- MarkdownParser.parse(str)

        } yield MarkdownFile(fi, elems, rawMeta)
        } finally {
          println("Done with "+fi.pathAndSuffix.display)
        }
      case Some("doc") | Some("docx") | Some("rtf") | Some("pages") =>
        (for {
          realFile <- fi.file
          inputStream <- HoistedUtil.logFailure("Opening "+realFile)(new FileInputStream(realFile))
          out = new ByteArrayOutputStream()
          handler = new ToHTMLContentHandler(out, "UTF-8")
          metadata = new Metadata()
          myParser = new AutoDetectParser()
          thing <- HoistedUtil.logFailure("Trying to read word, rtf, whatever "+realFile)(try {myParser.parse(inputStream, handler, metadata)} finally {tryo(inputStream.close)})
          str = new String(out.toByteArray, "UTF-8")
          html <- parseHtml5File(str).map(MarkdownParser.childrenOfBody(_))

        } yield {


          val md: MetadataValue =
            KeyedMetadataValue(metadata.names().foldLeft[MetadataMeta.Metadata](Map.empty){(map, key) =>
              val k2 = MetadataKey(key)
              metadata.getValues(key).toList match {
                case Nil => map
                case x::Nil => map + (k2 -> MetadataValue(x))
                case xs => map + (k2 -> ListMetadataValue(xs.map(MetadataValue(_))))
              }}.toList)

          HtmlFile(fi, html ,md)}) or Full(OtherFile(fi))

      case _ => Full(OtherFile(fi))
    }
  }

  private object GetHeader {
    def unapply(in: Node): Option[(Int, Elem, NodeSeq)] = in match {
      case e: Elem =>
        val lc = e.label.toLowerCase
        if ((null eq e.prefix) && lc.length == 2 && lc.charAt(0) == 'h' && lc.charAt(1) >= '1' && lc.charAt(1) <= '6') {
          Some(((lc.charAt(1) - '0').toInt, e, e.child))
        } else None
      case _ => None
    }
  }

  private def buildMetadataValue(in: (Int, String, NodeSeq)): MetadataValue =
  KeyedMetadataValue(List(HTagLevelKey -> StringMetadataValue(in._1.toString),
    HTagIdKey -> StringMetadataValue(in._2), HTagBodyKey -> NodeSeqMetadataValue(in._3)))

  // find and record the H* depth... plus add an id to each h* tag
  def findHeaders(in: NodeSeq): (NodeSeq, MetadataValue) = {
    def morphIt(in: NodeSeq): (NodeSeq, MetadataValue) = {
      val lb = new ListBuffer[(Int, String, NodeSeq)]

      var ids: Set[String] = Set()

      def slugify(in: String): String = {
      val safe = """[^\w]""".r
      val r1 = safe.replaceAllIn(in.trim.toLowerCase, "-")
        val noLeadingDash = """^(\-)+""".r
        val notrailingDash = """(\-)+$""".r
        val r2 = noLeadingDash.replaceAllIn(r1, "")
        notrailingDash.replaceAllIn(r2, "") match {
          case "" => "x"
          case s => s
        }
      }

      def computeIdFor(in: NodeSeq, seq: Int = 1): String = {
        val toTest = slugify(in.text)
        if (!ids.contains(toTest)) toTest
        else if (!ids.contains(toTest+"_"+seq)) toTest+"_"+seq
        else computeIdFor(in, seq + 1)
      }

      val res = in.map {
        case GetHeader(i, e, body) =>
          val id = e.attribute("id")
          if (id.isDefined) {
            ids += id.get.text
            lb.append((i ,id.get.text, body))
            e
          } else {
            import Helpers._
            val i2 = computeIdFor(body)
            ids += i2
            lb.append((i, i2, body))
            e % ("id" -> i2)
          }
        case x => x
      }

      (res, KeyedMetadataValue(List(HTagsKey -> ListMetadataValue(lb.toList.map(buildMetadataValue)))))
    }
  if ((in \ "body").length > 0) {
    var info: MetadataValue = NullMetadataValue
    val nodes = ("body *" #> ((ns: NodeSeq) => {
      val (a, b) = morphIt(ns)
      info = b
      a
    })).apply(in)

    (nodes, info)
  } else {
    morphIt(in)
  }
  }

  private object ThinHtml {
    def unapply(in: NodeSeq): Option[NodeSeq] = {
      def findBody(): Option[NodeSeq] = (in \ "body").toList match {
        case (e: Elem) :: Nil => Some(e.child)
        case _ => None
      }

      (in \ "head").toList match {
        case Nil => findBody()
        case (e: Elem) :: Nil =>
          if(e.child.length == 0) findBody()
          else {
            val cnt = (e \ "link").length + (e \ "meta").length + (e \ "script").length
            if (cnt == 0) findBody()
            else None
          }
        case _ => None
      }
    }
  }

  def parseHtml5File(in: String): Box[NodeSeq] = {
    val i1 = in.indexOf("<html")
    val i2 = in.indexOf("<body")
    val i3 = in.indexOf("</body")
    val i4 = in.indexOf("</html")
    if (i1 >= -1 && i2 >= -2 && i3 >= 0 && i4 >= 0 &&
    i1 < i2 && i2 < i3 && i3 < i4) {
      Html5.parse(in.trim) match {
        case Full(ThinHtml(html)) => Full(html)
        case x => x
      }
    } else {
      val res = Html5.parse("<html><head><title>I eat yaks</title></head><body>"+in+"</body></html>")

      res.map{
        res => (res \ "body").collect{case e: Elem => e}.flatMap(_.child)
      }
    }
  }

  def findXmlMetaData(in: NodeSeq): MetadataValue = {
    val somePairs: List[(String, String)] = (in \\ "cms").toList.flatMap {
      case e: Elem => e.attributes.toList.flatMap {
        case up: UnprefixedAttribute if up.value ne null => List((up.key, up.value.text))
        case pe: PrefixedAttribute if pe.value ne null => List((pe.pre + "." + pe.key, pe.value.text))
        case _ => Nil
      }
      case _ => Nil
    }
    val otherPairs: List[(String, (Option[String], Option[NodeSeq]))] = (in \\ "tag").toList.flatMap {
      case e: Elem =>
        for {
          name <- e.attribute("name").toList.map(_.text)
          value = e.attribute("value").map(_.text)
          body = Some(e.child).filter(!_.isEmpty).map(a => (a: NodeSeq))
        } yield (name, (value, body))
      case _ => Nil
    }
    KeyedMetadataValue.build(somePairs) +&+
    KeyedMetadataValue(otherPairs.flatMap{
      case (name, (Some(str), Some(ns))) => List(MetadataKey(name) ->
        ListMetadataValue(List(StringMetadataValue(str), NodeSeqMetadataValue(ns))))
      case (name, (_, Some(ns))) => List(MetadataKey(name) -> NodeSeqMetadataValue(ns))
      case (name, (Some(str), _)) => List(MetadataKey(name) -> StringMetadataValue(str))
      case _ => Nil
    })
  }

  def filterNames[T](in: List[(String, T)]): List[(MetadataKey, T)] =
    in flatMap {
      case (str, other) if str.trim.length > 0 => List(MetadataKey(str.toLowerCase.trim) -> other)
      case _ => Nil
    }
}


sealed trait ParsedFile {
  type MyType <: ParsedFile

  def matchPath(path: List[String]): Boolean = fileInfo.pathAndSuffix.display == path.mkString("/", "/", "")

  def updateFileInfo(newFileInfo: FileInfo): MyType

  def metaData: MetadataValue

  def findData(in: MetadataKey): Box[MetadataValue] = metaData.map.get(in)

  def findString(in: MetadataKey): Box[String] = findData(in).flatMap(_.asString)

  def findBoolean(in: MetadataKey): Box[Boolean] = findData(in).flatMap(_.asBoolean)
  def findDate(in: MetadataKey): Box[DateTime] = findData(in).flatMap(_.asDate)

  def updateMetadata(newMd: MetadataValue): MyType

  def updateMetadata(key: MetadataKey, value: MetadataValue): MyType = {
    updateMetadata(metaData.removeKey(key).addKey(key, value))
  }

  def pageUrl: String = fileInfo.pathAndSuffix.display

  def neverWrite: Boolean = false

  def morphPath(f: List[String] => List[String]): MyType = {
    val np = f(pathAndSuffix.path)
    updateFileInfo(pathAndSuffix.copy(path = np).toFileInfo(fileInfo.file))
  }

  def fileInfo: FileInfo

  def pathAndSuffix: PathAndSuffix = fileInfo.pathAndSuffix

  // def hidden: Boolean = !fileInfo.pathAndSuffix.path.filter(_.startsWith("_")).isEmpty

  def uniqueId: String

  def writeTo(out: OutputStream): Unit

  lazy val bytes:Box[Array[Byte]] = {
    Helpers.tryo{
      val fos = new ByteArrayOutputStream()
      writeTo(fos)
      fos.toByteArray()
    }
  }
}

sealed trait HasHtml extends ParsedFile {
  type MyType <: HasHtml

  def html: NodeSeq

  def updateHtml(newHtml: NodeSeq): MyType

  def writeTo(out: OutputStream): Unit = {
   if (HoistedEnvironmentManager.value.isHtml(this))  {
        val or = new PrintWriter(out)
        or.write("<!DOCTYPE html>\n")
        try {
          Html5.write(this.html.collect {
            case e: Elem => e
          }.headOption getOrElse <html/>, or, false, false)
        } finally {
          or.close()
        }
  } else {
     val or = new PrintWriter(out)
        try {
          or.write(this.html.text)
        } finally {
          or.close()
        }
      }
  }
}

trait MetadataBuilder[T] {
  def build(in: T): MetadataValue
}

object MetadataBuilder {
  implicit def buildFromString: MetadataBuilder[String] = new MetadataBuilder[String] {
    def build(in: String): MetadataValue = MetadataValue(in)
  }

  implicit def buildFromPair: MetadataBuilder[(Option[String], Option[NodeSeq])] =
    new MetadataBuilder[(Option[String], Option[NodeSeq])] {
      def build(in: (Option[String], Option[NodeSeq])): MetadataValue = MetadataValue(in)
    }
}

final case class SyntheticFile(computeFileInfo: () => FileInfo,
                               computeMetaData: () => MetadataValue,
                               writer: OutputStream => Unit,
                               uniqueId: String = Helpers.nextFuncName) extends ParsedFile {
  type MyType = SyntheticFile

  def updateFileInfo(newFileInfo: FileInfo): SyntheticFile = this

  lazy val fileInfo = computeFileInfo()
  def writeTo(out: OutputStream): Unit = writer(out)

  override lazy val metaData: MetadataValue = computeMetaData()

  def updateMetadata(newMd: MetadataValue): SyntheticFile = this
}

final case class XmlFile(fileInfo: FileInfo,
                         html: NodeSeq,
                         raw: NodeSeq,
                         metaData: MetadataValue,
                         uniqueId: String = Helpers.nextFuncName) extends HasHtml {
  type MyType = XmlFile

  def updateFileInfo(newFileInfo: FileInfo): XmlFile = copy(fileInfo = newFileInfo)



  def updateMetadata(newMd: MetadataValue): XmlFile = copy(metaData =  newMd)
  def updateHtml(newHtml: NodeSeq): XmlFile = copy(html = newHtml)
}

final case class YamlFile(fileInfo: FileInfo,
                         metaData: MetadataValue,
                         uniqueId: String = Helpers.nextFuncName) extends ParsedFile {
  type MyType = YamlFile

  def updateFileInfo(newFileInfo: FileInfo): YamlFile = copy(fileInfo = newFileInfo)

  /**
   * Never write out
   * @param out
   */
  def writeTo(out: OutputStream) {
    sys.error("Trying to write a YAML file")
  }

  override def neverWrite: Boolean = true

  def updateMetadata(newMd: MetadataValue): YamlFile = copy(metaData =  newMd)
}

final case class HtmlFile(fileInfo: FileInfo,
                          html: NodeSeq,
                          metaData: MetadataValue,
                          uniqueId: String = Helpers.nextFuncName) extends HasHtml {
  type MyType = HtmlFile

  def updateFileInfo(newFileInfo: FileInfo): HtmlFile = copy(fileInfo = newFileInfo)

  def updateMetadata(newMd: MetadataValue): HtmlFile = copy(metaData =  newMd)
  def updateHtml(newHtml: NodeSeq): HtmlFile = copy(html = newHtml)
}

final case class MarkdownFile(fileInfo: FileInfo,
                              html: NodeSeq,
                              metaData: MetadataValue,
                              uniqueId: String = Helpers.nextFuncName) extends HasHtml {
  type MyType = MarkdownFile

  def updateFileInfo(newFileInfo: FileInfo): MarkdownFile = copy(fileInfo = newFileInfo)

  def updateMetadata(newMd: MetadataValue): MarkdownFile = copy(metaData =  newMd)
  def updateHtml(newHtml: NodeSeq): MarkdownFile = copy(html = newHtml)
}

final case class OtherFile(fileInfo: FileInfo,
                           metaData: MetadataValue = NullMetadataValue,
                           uniqueId: String = Helpers.nextFuncName) extends ParsedFile {
  type MyType = OtherFile

  def updateFileInfo(newFileInfo: FileInfo): OtherFile = copy(fileInfo = newFileInfo)
  def updateMetadata(newMd: MetadataValue): OtherFile = copy(metaData =  newMd)

  def writeTo(out: OutputStream) {
    val bufLen = 4096
    val buffer = new Array[Byte](bufLen)

    def copy(from: File) {
      val in = new FileInputStream(from)
      try {
          var len = 0
          while ( {
            len = in.read(buffer, 0, bufLen)
            len >= 0
          }) {
            if (len > 0) out.write(buffer, 0, len)
          }
      } finally {
        in.close()
      }
    }

    fileInfo.file.foreach(copy(_))
  }
}

/**
 * The alias of a URL to another file
 * @param from the original place
 * @param to the resulting file
 */
case class Alias(from: String, to: String)

final case class FileInfo(file: Box[File], relPath: String, name: String, pureName: String, suffix: Option[String]) {
  lazy val pathAndSuffix: PathAndSuffix =
    PathAndSuffix(relPath.toLowerCase.roboSplit("/").dropRight(1) ::: List(name.toLowerCase), suffix.map(_.toLowerCase))
}


final case class PathAndSuffix(path: List[String], suffix: Option[String]) {
  def display: String = path.mkString("/", "/", "") + (suffix.map(s => "." + s) getOrElse "")
  def toFileInfo(file: Box[File]): FileInfo = FileInfo(file,
    display, path.takeRight(1).head,
    (suffix.map(s => "." + s) getOrElse "") +
    (suffix.map(s => "." + s) getOrElse ""), suffix)
}



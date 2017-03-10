package org.hoisted.lib

import java.util

import xml.{NodeSeq,Text}
import net.liftweb._
import common._
import net.liftweb.util.Helpers
import Helpers._
import org.joda.time._
import format.ISODateTimeFormat

object MetadataValue {
  lazy val hasSquareBraces = """(?:\[|\{)((?:[^,;\]}]+(?:,|;)?)+)(?:\]|\})""".r

  lazy val tagEntry = """([^,;]+)(?:,|;)?""".r

  def apply(in: String): MetadataValue = hasSquareBraces.findAllIn(in).matchData.toList match {
    case md :: _ =>
      val str = md.group(1)
      ListMetadataValue(tagEntry.findAllIn(str).matchData.toList.map(_.group(1).trim).map(StringMetadataValue.apply))
    case _ => StringMetadataValue(in)
  }

  def apply(in: (Option[String], Option[NodeSeq])): MetadataValue = in match {
    case (Some(str), None) => StringMetadataValue(str)
    case (None, Some(ns)) => NodeSeqMetadataValue(ns)
    case (Some(str), Some(ns)) =>
      ListMetadataValue(StringMetadataValue(str) ::
        NodeSeqMetadataValue(ns) :: Nil)
    case _ => NullMetadataValue
  }

  implicit def boolToMDV(in: Boolean): MetadataValue = BooleanMetadataValue(in)
  implicit def strToMDV(in: String): MetadataValue = StringMetadataValue(in)
  implicit def dateToMDV(in: DateTime): MetadataValue = DateTimeMetadataValue(in)
  implicit def mdvToMDV(in: MetadataValue): MetadataValue = in
  implicit def nsToMDV(in: NodeSeq): MetadataValue = NodeSeqMetadataValue(in)
}

trait MetadataValue {
  def append(other: MetadataValue, key: MetadataKey): MetadataValue = other match {
    case ListMetadataValue(lst) =>
      if (key.prepend) ListMetadataValue(lst ::: List(this)) else ListMetadataValue(this :: lst)
    case x => if (key.prepend) ListMetadataValue(other :: this :: Nil) else ListMetadataValue(this :: other :: Nil)
  }

  def findInt(key: MetadataKey): Box[Int] = map.get(key).flatMap(_.asInt)

  def findBoolean(key: MetadataKey): Box[Boolean] = map.get(key).flatMap(_.asBoolean)

  def findString(key: MetadataKey): Box[String] = map.get(key).flatMap(_.asString)

  def findString(key: String): Box[String] = findString(new StringMetadataKey(key, false))

  def findDate(key: MetadataKey): Box[DateTime] = map.get(key).flatMap(_.asDate)

  def find(key: MetadataKey): Box[MetadataValue] = map.get(key)

  def addKey(key: MetadataKey, value: MetadataValue): MetadataValue = this +&+ KeyedMetadataValue(key, value)

  def removeKey(key: MetadataKey): MetadataValue = this

  def asString: Box[String]

  def asBoolean: Box[Boolean]

  def asInt: Box[Int]

  def forceString: String

  def asDate: Box[DateTime]

  def asNodeSeq: Box[NodeSeq] = Empty

  def asListString: Box[List[String]] = Empty

  def forceListString: List[String]

  def map: MetadataMeta.Metadata = Map.empty

  def toJs(): Any

  def testEq(other: MetadataValue): Boolean

  def prettyString: String

  lazy val flatten: List[(MetadataKey, MetadataValue)] = map.toList.flatMap(a => a :: a._2.flatten)

  def +&+(other: MetadataValue): MetadataValue = (this, other) match {
    case (NullMetadataValue, that) => that
    case (_, NullMetadataValue) => this
    case (KeyedMetadataValue(_), other: KeyedMetadataValue) =>
      KeyedMetadataValue(other.pairs.foldLeft(this.map){
        case (map, (key, value)) => map + (key -> (map.getOrElse(key, NullMetadataValue).append(value, key)))
      })

    case (ListMetadataValue(thisList), ListMetadataValue(thatList)) => ListMetadataValue(thisList ::: thatList)
    case (ListMetadataValue(thisLst), that) => ListMetadataValue(thisLst ::: List(that))
    case (_, ListMetadataValue(thatList)) => ListMetadataValue(this :: thatList)
    case (a, b) => ListMetadataValue(a :: b :: Nil)
  }
}

case object NullMetadataValue extends MetadataValue {
  override def append(other: MetadataValue, key: MetadataKey): MetadataValue = other
  def asString: Box[String] = Empty
  def asBoolean: Box[Boolean] = Empty
  def asDate: Box[DateTime] = Empty
  def asInt: Box[Int] = Empty

  def testEq(other: MetadataValue): Boolean = other eq this
  def forceString: String = ""
  def forceListString: List[String] = Nil

  def toJs(): Any = null

  def prettyString: String = "Null"
}

final case class StringMetadataValue(s: String) extends MetadataValue  {
  def asString: Box[String] = Full(s)
  override def asNodeSeq: Box[NodeSeq] = Full(Text(s))
  lazy val asBoolean: Box[Boolean] = Helpers.asBoolean(s)
  lazy val asDate: Box[DateTime] = DateUtils.parseDate(s.trim)
  lazy val asInt: Box[Int] = Helpers.asInt(s)
  def forceListString: List[String] = List(s)
  def forceString: String = s
  def testEq(other: MetadataValue): Boolean = other match {
    case StringMetadataValue(os) => os == s
    case x => x.forceString == s
  }
  def prettyString = s.encJs

  def toJs(): Any = s
}

final case class IntMetadataValue(i: Int) extends MetadataValue  {
  def asString: Box[String] = Empty
  def asBoolean: Box[Boolean] = Empty
  def asDate: Box[DateTime] = Empty
  def asInt: Box[Int] = Full(i)
  def forceListString: List[String] = List(i.toString)
  def forceString: String = i.toString
  def testEq(other: MetadataValue): Boolean = other match {
    case IntMetadataValue(os) => os == i
    case x => x.forceString == forceString
  }
  def prettyString = i.toString

  def toJs(): Any = i
}

final case class DoubleMetadataValue(d: Double) extends MetadataValue  {
  def asString: Box[String] = Empty
  def asBoolean: Box[Boolean] = Empty
  def asDate: Box[DateTime] = Empty
  def asInt: Box[Int] = Full(d.toInt)
  def forceListString: List[String] = List(d.toString)
  def forceString: String = d.toString
  def testEq(other: MetadataValue): Boolean = other match {
    case DoubleMetadataValue(os) => os == d
    case x => x.forceString == forceString
  }

  def prettyString = d.toString

  def toJs(): Any = d
}

object KeyedMetadataValue {
  def apply(key: MetadataKey, value: MetadataValue): KeyedMetadataValue  = KeyedMetadataValue(List(key -> value))
  def apply(map: MetadataMeta.Metadata): KeyedMetadataValue  = KeyedMetadataValue(map.toList)
  def build(lst: List[(String, String)]): KeyedMetadataValue  =
    new KeyedMetadataValue(lst.map{case (k, v) => MetadataKey(k) -> MetadataValue(v)})
}

final case class KeyedMetadataValue(pairs: Seq[(MetadataKey, MetadataValue)]) extends MetadataValue  {
  def asString: Box[String] = Empty
  def asBoolean: Box[Boolean] = Empty
  def asDate: Box[DateTime] = Empty
  def asInt: Box[Int] = Empty

  override def removeKey(key: MetadataKey): MetadataValue = KeyedMetadataValue(pairs.filterNot(_._1 == key))

  lazy val forceListString: List[String] = pairs.toList.flatMap(_._2.forceListString)

  override lazy val map: MetadataMeta.Metadata = Map(pairs :_*)

  lazy val forceString: String = forceListString.mkString(", ")
  def testEq(other: MetadataValue): Boolean = other match {
    case kmd: KeyedMetadataValue => pairs.foldLeft(kmd.map){
      case (m, (key, value)) if !m.contains(key) => m + (key -> BooleanMetadataValue(true))
      case (m, (key, value)) if value.testEq(m(key)) => m - key
      case (m, _) => m
    }.isEmpty
    case x => x.forceString == forceString
  }
  def prettyString = " { " + pairs.map{case (k, v) => k.key + ": "+ v.prettyString}.mkString(", ") + " } "

  def toJs(): Any = {
    val ret = new util.HashMap[String, Any]()
    pairs.foreach {
      case (k,v) => ret.put(k.key, v.toJs())
    }
    ret
  }
}

final case class ListMetadataValue(lst: List[MetadataValue]) extends MetadataValue {
  override def append(other: MetadataValue, key: MetadataKey): MetadataValue = other  match {
    case ListMetadataValue(l2) => if (key.prepend)  ListMetadataValue(l2 ::: this.lst) else ListMetadataValue(this.lst ::: l2)
    case x => if (key.prepend) ListMetadataValue(List(other) ::: lst) else ListMetadataValue(lst ::: List(other))
  }

  def prettyString = " [ " + lst.map{_.prettyString}.mkString(", ") + " ] "

  override def removeKey(key: MetadataKey): MetadataValue = ListMetadataValue(lst.map(_.removeKey(key)))


  override lazy val asListString: Box[List[String]] = Full(lst.flatMap{
    case lmd: ListMetadataValue => lmd.asListString.toList.flatMap(a => a)
    case x => x.asString.toList
  })

  lazy val forceListString: List[String] = lst.flatMap(_.forceListString)

  lazy val asString: Box[String] = lst.flatMap(_.asString).headOption
  lazy val asBoolean: Box[Boolean] = lst.flatMap(_.asBoolean).headOption
  lazy val asDate: Box[DateTime] = lst.flatMap(_.asDate).headOption
  lazy val asInt: Box[Int] = lst.flatMap(_.asInt).headOption
  override lazy val asNodeSeq: Box[NodeSeq] = lst.collect{case NodeSeqMetadataValue(v) => v}.headOption

  lazy val forceString: String = forceListString.mkString(", ")

  def testEq(other: MetadataValue): Boolean = other match {
    case ListMetadataValue(oLst) => if (lst.length == oLst.length) {
      lst.zip(oLst).filterNot(a => a._1.testEq(a._2)).isEmpty
    } else false
    case x => x.forceString == forceString
  }


  override lazy val map: MetadataMeta.Metadata = lst.foldLeft[MetadataMeta.Metadata](Map.empty)((m, md) => m ++ md.map)

  def toJs(): Any = {
    val ret = new util.ArrayList[Any]()
    lst.foreach {
      case v => ret.add(v)
    }
    ret
  }
}

final case class BooleanMetadataValue(b: Boolean) extends MetadataValue  {
  def prettyString = b.toString
  def asString: Box[String] = Empty
  def asBoolean: Box[Boolean] = Full(b)
  def asDate: Box[DateTime] = Empty
  def asInt: Box[Int] = Empty
  def forceListString: List[String] = List(b.toString)
  def forceString: String = b.toString

  def testEq(other: MetadataValue): Boolean = other match {
    case BooleanMetadataValue(ob) => ob == b
    case x => x.forceString == forceString
  }

  def toJs(): Any = b
}

final case class DateTimeMetadataValue(date: DateTime) extends MetadataValue  {
  def prettyString = date.toString
  def asString: Box[String] = Empty
  def asBoolean: Box[Boolean] = Empty
  def asDate: Box[DateTime] = Full(date)
  def asInt: Box[Int] = Empty
  def forceListString: List[String] = List(ISODateTimeFormat.basicDateTime().print(date))
  def forceString: String = forceListString.head

  def testEq(other: MetadataValue): Boolean = other match {
    case DateTimeMetadataValue(odt) => odt.getMillis == date.getMillis
    case x => x.forceString == forceString
  }

  def toJs(): Any = date
}

final case class NodeSeqMetadataValue(ns: NodeSeq) extends MetadataValue  {
  def prettyString = ns.toString
  def asString: Box[String] = Full(forceString)
  def asBoolean: Box[Boolean] = Empty
  def asDate: Box[DateTime] = Empty
  def asInt: Box[Int] = Empty
  override def asNodeSeq: Box[NodeSeq] = Full(ns)
  def forceListString: List[String] = List(forceString)
  lazy val forceString: String = ns.text
  def testEq(other: MetadataValue): Boolean = forceString == other.forceString

  def toJs(): Any = ns
}


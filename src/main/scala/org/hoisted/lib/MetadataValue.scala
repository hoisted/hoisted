package org.hoisted.lib

import xml.NodeSeq
import net.liftweb._
import common._
import org.joda.time._
import format.ISODateTimeFormat
import util.Helpers
import org.hoisted.lib.MetadataValue._

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
  def ++(other: MetadataValue): MetadataValue

  def asString: Box[String]

  def asBoolean: Box[Boolean]

  def asInt: Box[Int]

  def asDate: Box[DateTime]

  def asListString: List[String] = asString.toList
}

case object NullMetadataValue extends MetadataValue {
  def ++(other: MetadataValue): MetadataValue = other
  def asString: Box[String] = Empty
  def asBoolean: Box[Boolean] = Empty
  def asDate: Box[DateTime] = Empty
  def asInt: Box[Int] = Empty
}

final case class StringMetadataValue(s: String) extends MetadataValue  {
  def ++(other: MetadataValue): MetadataValue = other match {
    case ListMetadataValue(lst) => ListMetadataValue(this :: lst)
    case x => ListMetadataValue(this :: other :: Nil)
  }
  def asString: Box[String] = Full(s)
  lazy val asBoolean: Box[Boolean] = Helpers.asBoolean(s)
  lazy val asDate: Box[DateTime] = ParsedFile.parseDate(s.trim)
  lazy val asInt: Box[Int] = Helpers.asInt(s)

}

final case class ListMetadataValue(lst: List[MetadataValue]) extends MetadataValue {
  def ++(other: MetadataValue): MetadataValue = other  match {
    case ListMetadataValue(l2) => ListMetadataValue(this.lst ::: l2)
    case x => ListMetadataValue(lst ::: List(other))
  }

  override lazy val asListString: List[String] = lst.flatMap(_.asListString)
  lazy val asString: Box[String] = lst.flatMap(_.asString).headOption
  lazy val asBoolean: Box[Boolean] = lst.flatMap(_.asBoolean).headOption
  lazy val asDate: Box[DateTime] = lst.flatMap(_.asDate).headOption
  lazy val asInt: Box[Int] = lst.flatMap(_.asInt).headOption
}

final case class BooleanMetadataValue(b: Boolean) extends MetadataValue  {
  def ++(other: MetadataValue): MetadataValue = other match {
    case ListMetadataValue(lst) => ListMetadataValue(this :: lst)
    case x => ListMetadataValue(this :: other :: Nil)
  }

  def asString: Box[String] = Empty
  def asBoolean: Box[Boolean] = Full(b)
  def asDate: Box[DateTime] = Empty
  def asInt: Box[Int] = Empty
}

final case class DateTimeMetadataValue(date: DateTime) extends MetadataValue  {
  def ++(other: MetadataValue): MetadataValue = other  match {
    case ListMetadataValue(lst) => ListMetadataValue(this :: lst)
    case x => ListMetadataValue(this :: other :: Nil)
  }

  lazy val asString: Box[String] = Full(ISODateTimeFormat.basicDateTime().print(date))
  def asBoolean: Box[Boolean] = Empty
  def asDate: Box[DateTime] = Full(date)
  def asInt: Box[Int] = Empty
}

final case class NodeSeqMetadataValue(ns: NodeSeq) extends MetadataValue  {
  def ++(other: MetadataValue): MetadataValue = other  match {
    case ListMetadataValue(lst) => ListMetadataValue(this :: lst)
    case x => ListMetadataValue(this :: other :: Nil)
  }

  def asString: Box[String] = Full(ns.text)
  def asBoolean: Box[Boolean] = Empty
  def asDate: Box[DateTime] = Empty
  def asInt: Box[Int] = Empty
}


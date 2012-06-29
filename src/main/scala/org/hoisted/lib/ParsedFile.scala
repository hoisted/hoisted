package org.hoisted.lib

import net.liftweb._
import common._
import util._
import Helpers._
import xml.{UnprefixedAttribute, PrefixedAttribute, Elem, NodeSeq}
import java.io.FileInputStream
import org.joda.time.{DateTimeZone, DateTime}
import org.joda.time.format.{DateTimeFormatter, ISODateTimeFormat, DateTimeFormat}

/**
 * Created with IntelliJ IDEA.
 * User: dpp
 * Date: 6/8/12
 * Time: 12:02 PM
 * To change this template use File | Settings | File Templates.
 */

final case class PathAndSuffix(path: List[String], suffix: Option[String])

object ParsedFile {

  object CurrentTimeZone extends ThreadGlobal[DateTimeZone]

  lazy val dateFormats = List(DateTimeFormat.forPattern("EEE, dd MMM yyyy HH:mm:ss Z"),
    DateTimeFormat.forPattern("yyyy-MM-dd HH:mm:ss Z"),
    DateTimeFormat.forPattern("yyyy/MM/dd HH:mm:ss Z"),
    DateTimeFormat.forPattern("yyyy-MM-dd HH:mm:ss"),
    DateTimeFormat.forPattern("yyyy/MM/dd HH:mm:ss"),
    DateTimeFormat.forPattern("yyyy-MM-dd HH:mm Z"),
    DateTimeFormat.forPattern("yyyy/MM/dd HH:mm Z"),
    DateTimeFormat.forPattern("yyyy-MM-dd HH:mm"),
    DateTimeFormat.forPattern("yyyy/MM/dd HH:mm"),
    DateTimeFormat.forPattern("yyyy-MM-dd Z"),
    DateTimeFormat.forPattern("yyyy/MM/dd Z"),
    DateTimeFormat.forPattern("yyyy-MM-dd"),
    DateTimeFormat.forPattern("yyyy/MM/dd"),
    ISODateTimeFormat.basicDateTime(),
    ISODateTimeFormat.basicDateTime(),
    DateTimeFormat.longDateTime(),
    DateTimeFormat.fullDateTime(),
    DateTimeFormat.fullDate(),
    DateTimeFormat.longDate(),
    DateTimeFormat.mediumDateTime(),
    DateTimeFormat.mediumDate(),
    DateTimeFormat.shortDateTime(),
    DateTimeFormat.shortDate()).toStream

  def parseDate(str: String): Box[DateTime] = {
    val mod: DateTimeFormatter => DateTimeFormatter = CurrentTimeZone.box match {
      case Full(tz) => ((f: DateTimeFormatter) => f.withZone(tz))
      case _ => f => f
    }
    dateFormats.flatMap(f => tryo(mod(f).parseDateTime(str))).headOption
  }

  @scala.annotation.tailrec
  def uglyParseDate(str: String): Box[DateTime] = if (str.length < 8) Empty else {
    parseDate(str) match {
      case Full(d) => Full(d)
      case _ => uglyParseDate(str.dropRight(1))
    }
  }


  def findBody(in: NodeSeq): NodeSeq =
    (in \ "content").headOption.map(_.child) getOrElse in


  def apply(fi: FileInfo): Box[ParsedFile] = {
    fi.suffix.map(_.toLowerCase) match {
      case Some("xml") | Some("cms.xml") =>
        for {
          fis <- tryo(new FileInputStream(fi.file))
          xml <- PCDataXmlParser(fis)
          _ <- tryo(fis.close())
          metaData = findXmlMetaData(xml)
        } yield XmlFile(fi, findBody(xml), xml,
          HoistedEnvironmentManager.value.updateMetadata(metaData, fi))

      case Some("html") | Some("htm") =>
        for {
          fis <- tryo(new FileInputStream(fi.file))
          bytes <- tryo(Helpers.readWholeStream(fis))
          str = new String(bytes, "UTF-8")
          (str2, info) = MarkdownParser.readTopMetadata(str)
          html <- Html5.parse(str2.trim)
          _ <- tryo(fis.close())
          (_html, metaData) = findHtmlMetaData(html)
        } yield HtmlFile(fi, _html,
        HoistedEnvironmentManager.value.updateMetadata(
          mergeMetadata(pairsToMetadata(info), metaData), fi))

      case Some("md") =>
        for {
          whole <- tryo(Helpers.readWholeFile(fi.file))
          str = new String(whole, "UTF-8")
          (elems, rawMeta) <- MarkdownParser.parse(str)
        } yield MarkdownFile(fi, elems,
          HoistedEnvironmentManager.value.updateMetadata(pairsToMetadata(rawMeta), fi))
      case _ => Full(OtherFile(fi))
    }
  }

  def findHtmlMetaData(in: NodeSeq): (NodeSeq, MetadataMeta.Metadata) = {

    (in, Map.empty) // FIXME
  }

  def findXmlMetaData(in: NodeSeq): MetadataMeta.Metadata = {
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
          body: Option[NodeSeq] = Some(e.child).filter(!_.isEmpty).map(a => (a: NodeSeq))
        } yield (name, (value, body))
      case _ => Nil
    }
    mergeMetadata(pairsToMetadata(somePairs), pairsToMetadata(otherPairs))
  }

  def filterNames[T](in: List[(String, T)]): List[(MetadataKey, T)] =
    in flatMap {
      case (str, other) if str.trim.length > 0 => List(MetadataKey(str.toLowerCase.trim) -> other)
      case _ => Nil
    }

  def pairsToMetadata[T](in: List[(String, T)])(implicit buildMetadata: MetadataBuilder[T]): MetadataMeta.Metadata = {
    filterNames(in).foldLeft[MetadataMeta.Metadata](Map()) {
      case (m, (key, value)) if key.global =>
        HoistedEnvironmentManager.value.appendMetadata(key, buildMetadata.build(value))
        m
      case (m, (key, value)) =>
        append(m, key, buildMetadata.build(value))
    }
  }

  def mergeMetadata(m1: MetadataMeta.Metadata, m2: MetadataMeta.Metadata): MetadataMeta.Metadata = {
    m2.foldLeft(m1) {
      case (map, (key, value)) => append(map, key, value)
    }
  }

  def append(m1: MetadataMeta.Metadata, key: MetadataKey, value: MetadataValue): MetadataMeta.Metadata =
    m1 + (key -> (m1.getOrElse(key, NullMetadataValue) ++ value))


}


sealed trait ParsedFile {
  def fileInfo: FileInfo

  def pathAndSuffix: PathAndSuffix = fileInfo.pathAndSuffix

  // def hidden: Boolean = !fileInfo.pathAndSuffix.path.filter(_.startsWith("_")).isEmpty

  def findData(in: MetadataKey): Box[MetadataValue] = Empty

  def uniqueId: String
}

sealed trait HasHtml {
  def html: NodeSeq
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


sealed trait HasMetaData {
  self: ParsedFile =>
  def metaData: MetadataMeta.Metadata

  override def findData(in: MetadataKey): Box[MetadataValue] = metaData.get(in)

}

final case class XmlFile(fileInfo: FileInfo,
                         html: NodeSeq,
                         raw: NodeSeq,
                         metaData: MetadataMeta.Metadata,
                         uniqueId: String = Helpers.nextFuncName) extends ParsedFile with HasHtml with HasMetaData

final case class HtmlFile(fileInfo: FileInfo,
                          html: NodeSeq,
                          metaData: MetadataMeta.Metadata,
                          uniqueId: String = Helpers.nextFuncName) extends ParsedFile with HasHtml with HasMetaData

final case class MarkdownFile(fileInfo: FileInfo,
                              html: NodeSeq,
                              metaData: MetadataMeta.Metadata,
                              uniqueId: String = Helpers.nextFuncName) extends ParsedFile with HasHtml with HasMetaData

final case class OtherFile(fileInfo: FileInfo,
                           uniqueId: String = Helpers.nextFuncName) extends ParsedFile



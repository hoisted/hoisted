package org.hoisted.lib

import scala.collection.JavaConverters._
import scala.xml.{Elem, NodeSeq}

import net.liftweb._
  import common._
  import util.{Helpers}

import org.asciidoctor._

/**
 * Created with IntelliJ IDEA.
 * User: dpp
 * Date: 6/8/12
 * Time: 11:56 AM
 * To change this template use File | Settings | File Templates.
 */


object AsciidocParser extends Loggable {
  lazy val matchMetadata = """(?m)\A(:?[ \t]*\n)?(?:-{3,}+\n)?(^([a-zA-Z0-9 _\-]+)[=:]([^\n]*)\n+(:?[ \t]*\n)?)+(:?-{3,}+\n)?""".r

  lazy val topMetadata = """(?m)^([^:]+):[ \t]*(.*)$""".r

  lazy val topYamlStuff = """(?m)(?:^\w$)*^-{3,}$((?:.|\n)*)^-{3,}$""".r

  lazy val lineSplit = """(?m)^(.*)$""".r

  lazy val linkDefs = """(?m)^\p{Space}{0,3}\[([^:]+)[=:](?:[ ]*)(.+)\]:""".r

  lazy val hasYaml = """(?s)(?m)^[yY][aA][mM][lL][ \t]*\{[ \t]*$(.*?)^\}[ \t]*[yY][Aa][mM][Ll][ \t]*$""".r
  lazy val htmlHasYaml = """(?s)(?m)\A(:?[ \t]*\n)*^[yY][aA][mM][lL][ \t]*\{[ \t]*$(.*?)^\}[ \t]*[yY][Aa][mM][Ll][ \t]*$""".r

  def childrenOfBody(in: NodeSeq): NodeSeq = {
    (in \ "body").toList match {
      case Nil => in
      case xs => xs.collect {
        case e: Elem => e
      }.flatMap(_.child)
    }
  }

  def readTopMetadata(in: String, markdownFormat: Boolean): (String, MetadataValue, Any) = {
    topYamlStuff.findFirstMatchIn(in) match {
      case Some(matcher) =>
        val yaml = matcher.group(1)
        YamlUtil.parse(yaml).map {
          case (meta, json) =>
            (topYamlStuff.replaceFirstIn(in, ""), meta, json)
        }.openOr(in, NullMetadataValue, null)

      case _ =>
        val yamlRegex = if (markdownFormat) hasYaml else htmlHasYaml

        val pairs: List[(MetadataValue, Any)] =
          for {
            thing <- yamlRegex.findAllIn(in).matchData.toList
            yamlStr = thing.group(1)
            yaml <- YamlUtil.parse(yamlStr)
          } yield yaml

        val jsMeta = pairs match {
          case (_, ret) :: Nil => ret
          case _ => null
        }

        val (_in, p2) =
          pairs.map(_._1) match {
            case Nil =>
              matchMetadata.findFirstIn(in) match {
                case Some(data) =>
                  (matchMetadata.replaceAllIn(in, ""),
                    List(KeyedMetadataValue.build(lineSplit.findAllIn(data).toList.flatMap(s =>
                      topMetadata.findAllIn(s).matchData.toList.map(md => (md.group(1).trim, md.group(2).trim))))))
                case None => (in, List(NullMetadataValue))
              }
            case x => (yamlRegex.replaceAllIn(in, ""), x)
          }

        val pairs2: MetadataValue = if (markdownFormat)
          KeyedMetadataValue.build(linkDefs.findAllIn(_in).
            matchData.toList.map(md => (md.group(1).trim, md.group(2).trim)))
        else NullMetadataValue

        (_in, p2.foldLeft(pairs2)(_ +&+ _), jsMeta)
    }
  }

  private lazy val asciidoctor = Asciidoctor.Factory.create()
  private lazy val asciidoctorAttributes =
    AttributesBuilder.attributes()
      .unsetStyleSheet()
  private lazy val asciidoctorOptions =
    OptionsBuilder.options()
      .safe(SafeMode.SAFE)
      .attributes(asciidoctorAttributes)

  def parse(in: String): Box[(NodeSeq, MetadataValue, Any)] = {
    asciidoctor.synchronized {
      for {
        documentAttributes <- Helpers.tryo(asciidoctor.readDocumentHeader(in).getAttributes)
        stringMetadata =
          documentAttributes.asScala.toList.flatMap {
            case (key, value: String) => Seq((key, value))
            case (key, other) =>
              logger.info(s"Found non-string asciidoc metadata value for $key: $other")
              Seq()
          }
        metadata = KeyedMetadataValue.build(stringMetadata)
        htmlString <- Helpers.tryo(asciidoctor.convert(in, asciidoctorOptions))
        res = HoistedHtml5.parse(s"<html><head><title>I eat yaks</title></head><body>$htmlString</body></html>")
        documentBody <- res.map {
          res => (res \ "body").collect {
            case e: Elem => e
          }.flatMap(_.child)
        }
      } yield {
        (documentBody, metadata, documentAttributes)
      }
    }
  }
}

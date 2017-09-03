package org.hoisted.lib

import net.liftweb._
import common._
import util.{Helpers}
import xml.{Elem, NodeSeq}

/**
 * Created with IntelliJ IDEA.
 * User: dpp
 * Date: 6/8/12
 * Time: 11:56 AM
 * To change this template use File | Settings | File Templates.
 */


object MarkdownParser {
  private val matchMetadata = """(?m)\A(:?[ \t]*\n)?(?:-{3,}+\n)?(^([a-zA-Z0-9 _\-]+)[=:]([^\n]*)\n+(:?[ \t]*\n)?)+(:?-{3,}+\n)?""".r

  private val topMetadata = """(?m)^([^:]+):[ \t]*(.*)$""".r

  private val topYamlStuff = """(?m)(?:^\w$)*^-{3,}$((?:.|\n)*)^-{3,}$""".r

  private val lineSplit = """(?m)^(.*)$""".r

  private val linkDefs = """(?m)^\p{Space}{0,3}\[([^:]+)[=:](?:[ ]*)(.+)\]:""".r

  private val hasYaml = """(?s)(?m)^[yY][aA][mM][lL][ \t]*\{[ \t]*$(.*?)^\}[ \t]*[yY][Aa][mM][Ll][ \t]*$""".r
  private val htmlHasYaml = """(?s)(?m)\A(:?[ \t]*\n)*^[yY][aA][mM][lL][ \t]*\{[ \t]*$(.*?)^\}[ \t]*[yY][Aa][mM][Ll][ \t]*$""".r

  def childrenOfBody(in: NodeSeq): NodeSeq = {
    (in \ "body").toList match {
      case Nil => in
      case xs => xs.collect {
        case e: Elem => e
      }.flatMap(_.child)
    }
  }

  def readTopMetadata(in: String, markdownFormat: Boolean): (String, MetadataValue, Any) = {

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

  private lazy val markdownTransformer = new net.liftweb.markdown.SingleThreadedTransformer

  def parse(in: String): Box[(NodeSeq, MetadataValue, Any)] = {
    val (_in, retPairs, rawJson) = readTopMetadata(in.replace("\r\n", "\n").replace("\r", "\n"), true)

    markdownTransformer.synchronized {
      for {
        str <- Helpers.tryo(markdownTransformer.apply(_in))
        res = HoistedHtml5.parse("<html><head><title>I eat yaks</title></head><body>" + str + "</body></html>")
        info <- res.map {
          res => (res \ "body").collect {
            case e: Elem => e
          }.flatMap(_.child)
        }

        titleFromHtml = res.map(_ \\ "h1").flatMap(_.headOption).map(_.text)
      } yield {
        // Use title from HTML if no other title has been specified.
        val finalMetadata =
          (retPairs.map.get(MetadataKey("title")), titleFromHtml) match {
            case (Some(_), _) =>
              retPairs
            case (_, Full(title)) =>
              retPairs +&+ KeyedMetadataValue(MetadataKey("title"), MetadataValue(title))
            case _ =>
              retPairs
          }

        (info, finalMetadata, rawJson)
      }
    }

  }
}

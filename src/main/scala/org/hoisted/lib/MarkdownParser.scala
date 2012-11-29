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
  lazy val matchMetadata = """(?m)\A(:?[ \t]*\n)?(?:-{3,}+\n)?(^([a-zA-Z0-9 _\-]+)[=:]([^\n]*)\n+(:?[ \t]*\n)?)+(:?-{3,}+\n)?""".r

  lazy val topMetadata = """(?m)^([^:]+):[ \t]*(.*)$""".r

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

  def readTopMetadata(in: String, markdownFormat: Boolean): (String, MetadataValue) = {
    val yamlRegex = if (markdownFormat) hasYaml else htmlHasYaml

    val pairs: List[MetadataValue] =
      for {
        thing <- yamlRegex.findAllIn(in).matchData.toList
        yamlStr = thing.group(1)
        yaml <- YamlUtil.parse(yamlStr)
      } yield yaml

    val (_in, p2) =
      pairs match {
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

    (_in, p2.foldLeft(pairs2)(_ +&+ _))
  }

  def parse(in: String): Box[(NodeSeq, MetadataValue)] = {
    val (_in, retPairs) = readTopMetadata(in.replace("\r\n", "\n").replace("\r", "\n"), true)

    import eu.henkelmann.actuarius._


    ActuariusApp.synchronized{
    for {
      str <- Helpers.tryo(ActuariusApp.apply(_in))
      res = HoistedHtml5.parse("<html><head><title>I eat yaks</title></head><body>" + str + "</body></html>")
      info <- res.map {
        res => (res \ "body").collect {
          case e: Elem => e
        }.flatMap(_.child)
      }
    } yield info -> retPairs
    }

  }
}

package org.hoisted.lib

import scala.collection.JavaConverters._
import scala.xml.{Elem, NodeSeq}

import net.liftweb._
  import common._
  import java.io.{File, FileOutputStream, FileInputStream}
  import util.{Helpers}
import scala.sys.process._

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

  private def writeToTmpFile(in: String): Box[String] = {
    Helpers.tryo{
    val file = File.createTempFile("hoisted_", ".adoc")
    val fos = new FileOutputStream(file)
    fos.write(in.getBytes("UTF-8"))
    fos.flush()
    fos.close()
    file.getAbsolutePath()

    }
  }

  private def execAsciiDoctor(filename: String): Box[Any] = {
    Helpers.tryo {
     Seq("asciidoctor", "-r", "asciidoctor-diagram", "-d", "book", filename).!
    }
  }

  private def readGeneratedHtml(baseFilename: String): Box[String] = {
    Helpers.tryo{
    val htmlFile = new File(baseFilename.replace(".adoc", ".html"))
    val oldFile = new File(baseFilename)
    val bytes = Helpers.readWholeFile(htmlFile)
    oldFile.delete()
    htmlFile.delete()
    new String(bytes, "UTF-8")
    }
  }

  def parse(in: String): Box[(NodeSeq, MetadataValue, Any)] = {
  //    val metadata = Map[String, String]()

    val (realBody, metadata, _) = readTopMetadata(in, true)
    AsciidocParser.synchronized {
      for {
        filename <- writeToTmpFile(realBody)
        _ <- execAsciiDoctor(filename)
        htmlString <- readGeneratedHtml(filename)
        res = HoistedHtml5.parse(htmlString)
        documentBody <- res.map {
          res => (res \ "body").collect {
            case e: Elem => e
          }.flatMap(_.child)
        }

        titleFromDocument =     
          Box(metadata.map.get(MetadataKey("doctitle")).flatMap(_.asString)) or 
            res.map(_ \\ "h1").flatMap(_.headOption).map(_.text) 
      } yield {
        // Use title from HTML if no other title has been specified.
        val finalMetadata =
          (metadata.map.get(MetadataKey("title")), titleFromDocument) match {
            case (Some(_), _) =>
              metadata
            case (_, Full(title)) =>
              metadata +&+ KeyedMetadataValue(MetadataKey("title"), MetadataValue(title))
            case _ =>
              metadata
          }

        (documentBody, finalMetadata, None)
      }
    }
  }
}

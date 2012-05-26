package org.hoisted
package lib

import net.liftweb._
import common._
import http.{S, LiftSession, LiftRules}
import util._
import Helpers._
import xml.NodeSeq
import java.io.{FileInputStream, File}
import java.util.Locale

/**
 * This singleton will take a directory, find all the files in the directory
 * and then generate a static site in the output directory and return metadata about
 * the transformation
 */

object RunHoisted extends HoistedRenderer

trait HoistedRenderer {
  def apply(inDir: File, outDir: File): Box[HoistedTransformMetaData] = {
    for {
      deleteAll <- tryo(deleteAll(outDir))
      theDir <- tryo(outDir.mkdirs())
      allFiles <- tryo(allFiles(inDir).filter(_.exists()))
      fileInfo <- tryo(allFiles.map(fileInfo(inDir)))
      parsedFiles = (fileInfo: List[FileInfo]).flatMap(ParsedFile.apply _)
      fileMap = byName(parsedFiles)
      templates = createTemplateLookup(parsedFiles)
      transformedFiles = parsedFiles.map(f => runTemplater(f, templates))

    } yield HoistedTransformMetaData()
  }

  type TemplateLookup = PartialFunction[(List[String], String), ParsedFile]

  def createTemplateLookup(in: Seq[ParsedFile]): TemplateLookup = Map.empty // FIXME

  def snippets: PartialFunction[(String, String), Box[NodeSeq => NodeSeq]] = Map.empty // FIXME

  def runTemplater(f: ParsedFile, templates: TemplateLookup): ParsedFile = {
    val lu = new PartialFunction[(Locale, List[String]), Box[NodeSeq]] {
      def isDefinedAt(in: (Locale, List[String])): Boolean = true

      def apply(in: (Locale, List[String])): Box[NodeSeq] = Empty
    }
    val localSnippets = snippets
    val session = new LiftSession("", Helpers.nextFuncName, Empty)
    S.initIfUninitted(session) {
      LiftRules.allowParallelSnippets.doWith(() => false) {
        LiftRules.allowAttributeSnippets.doWith(() => false) {
          LiftRules.snippetWhiteList.doWith(() => localSnippets) {
            LiftRules.externalTemplateResolver.doWith(() => () => lu) {
              f // FIXME run the template and then perhaps fix it up
            }
          }
        }
      }
    }
  }

  def fileInfo(inDir: File)(f: File): FileInfo = {
    val cp: String = f.getAbsolutePath()
    val hidden = (cp.indexOf("/_") != -1)
    val pureName = f.getName
    val dp = pureName.lastIndexOf(".")
    val (name, suf) = if (dp <= 0) (pureName, None)
    else (pureName.substring(0, dp),
      Some(pureName.substring(dp + 1)))
    FileInfo(f, name, pureName, suf, hidden)
  }

  def byName(in: Seq[ParsedFile]): Map[String, List[ParsedFile]] = {
    in.foldLeft[Map[String, List[ParsedFile]]](Map.empty) {
      (m, f) =>
        val name = f.fileInfo.name

        m + (name -> (f :: m.getOrElse(name, Nil)))
    }
  }

  def byPureName(in: Seq[ParsedFile]): Map[String, List[ParsedFile]] = {
    in.foldLeft[Map[String, List[ParsedFile]]](Map.empty) {
      (m, f) =>
        val name = f.fileInfo.pureName

        m + (name -> (f :: m.getOrElse(name, Nil)))
    }
  }

  def deleteAll(f: File) {
    if (f.isDirectory()) {
      f.listFiles().foreach(deleteAll)
      f.delete()
    } else f.delete()
  }

  def allFiles(dir: File): List[File] = {
    if (dir.isDirectory()) {
      dir.listFiles().toList.flatMap(allFiles)
    } else if (dir.isFile()) List(dir)
    else Nil
  }
}

final case class HoistedTransformMetaData()

final case class FileInfo(file: File, name: String, pureName: String, suffix: Option[String], hidden: Boolean)

object ParsedFile {
  def findBody(in: NodeSeq): NodeSeq =
    (in \ "content").headOption getOrElse in


  def apply(fi: FileInfo): Box[ParsedFile] = {
    fi.suffix.map(_.toLowerCase) match {
      case Some("xml") =>
        for {
          fis <- tryo(new FileInputStream(fi.file))
          xml <- PCDataXmlParser(fis)
          _ <- tryo(fis.close())
          metaData = findXmlMetaData(xml)
        } yield XmlFile(fixFileInfo(fi, metaData), findBody(xml), xml, metaData)

      case Some("html") | Some("htm") =>
        for {
          fis <- tryo(new FileInputStream(fi.file))
          html <- Html5.parse(fis)
          _ <- tryo(fis.close())
          metaData = findHtmlMetaData(html)
        } yield HtmlFile(fixFileInfo(fi, metaData), html, metaData)

      case Some("md") => Full(OtherFile(fi)) // FIXME deal with Markdown
      case _ => Full(OtherFile(fi))
    }
  }

  type MetaData = Map[String, List[String]]

  def findHtmlMetaData(in: NodeSeq): MetaData = Map.empty

  // FIXME
  def findXmlMetaData(in: NodeSeq): Map[String, List[String]] = Map.empty

  // FIXME
  def fixFileInfo(fi: FileInfo, metaData: MetaData): FileInfo = fi // FIXME
}

sealed trait ParsedFile {
  def fileInfo: FileInfo

  def hidden: Boolean = fileInfo.hidden
}

sealed trait HasHtml {
  def html: NodeSeq
}

sealed trait HasMetaData {
  def metaData: Map[String, List[String]]
}

final case class XmlFile(fileInfo: FileInfo,
                         html: NodeSeq,
                         raw: NodeSeq,
                         metaData: Map[String, List[String]]) extends ParsedFile with HasHtml with HasMetaData

final case class HtmlFile(fileInfo: FileInfo,
                          html: NodeSeq,
                          metaData: Map[String, List[String]]) extends ParsedFile with HasHtml with HasMetaData

final case class MarkdownFile(fileInfo: FileInfo,
                              html: NodeSeq,
                              metaData: Map[String, List[String]]) extends ParsedFile with HasHtml with HasMetaData

final case class OtherFile(fileInfo: FileInfo) extends ParsedFile
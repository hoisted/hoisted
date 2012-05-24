package com.liftweb.lib

import net.liftweb._
import common._
import util._
import Helpers._
import org.apache.derby.iapi.sql.dictionary.FileInfoDescriptor
import xml.NodeSeq
import java.io.{FileInputStream, File}

/**
 * This singleton will take a directory, find all the files in the directory
 * and then generate a static site in the output directory and return metadata about
 * the transformation
 */


object RunHoisted {
  def apply(inDir: File, outDir: File): Box[HoistedTransformMetaData] = {
    for {
      deleteAll <- tryo(deleteAll(outDir))
      theDir <- tryo(outDir.mkdirs())
      allFiles <- tryo(allFiles(inDir).filter(_.exists()))
      fileInfo <- tryo(allFiles.map(fileInfo(inDir)))
      fileMap = byName(fileInfo)

    } yield HoistedTransformMetaData()
  }

  def fileInfo(inDir: File)(f: File): FileInfo = {
    val cp: String = f.getAbsolutePath()
    val hidden = (cp.indexOf("/_") != -1)
    val pureName = f.getName
    val dp = pureName.lastIndexOf(".")
    val (name, suf) = if (dp <= 0) (pureName, None) else (pureName.substring(0, dp),
      Some(pureName.substring(dp + 1)))
    FileInfo(f, name, pureName, suf, hidden)
  }

  def byName(in: Seq[ParsedFile]): Map[String, List[ParsedFile]] = {
    in.foldLeft[Map[String, List[ParsedFile]]](Map.empty){(m, f) =>
      val name = f.fileInfo.name

      m + (name -> (f :: m.getOrElse(name, Nil)))}
  }

  def byPureName(in: Seq[ParsedFile]): Map[String, List[ParsedFile]] = {
    in.foldLeft[Map[String, List[ParsedFile]]](Map.empty){(m, f) =>
      val name = f.fileInfo.pureName

      m + (name -> (f :: m.getOrElse(name, Nil)))}
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
        } yield XmlFile(fi, findBody(xml), xml)

      case Some("html") | Some("htm") =>
        for {
          fis <- tryo(new FileInputStream(fi.file))
          html <- Html5.parse(fis)
          _ <- tryo(fis.close())
        } yield HtmlFile(fi, html)

      case Some("md") => Full(OtherFile(fi)) // FIXME deal with Markdown
      case _ => Full(OtherFile(fi))
    }
  }
}

sealed trait ParsedFile {
  def fileInfo: FileInfo
  def hidden: Boolean = fileInfo.hidden
}

sealed trait HasHtml {
  def html: NodeSeq
}

final case class XmlFile(fileInfo: FileInfo, html: NodeSeq, raw: NodeSeq) extends ParsedFile with HasHtml
final case class HtmlFile(fileInfo: FileInfo, html: NodeSeq) extends ParsedFile with HasHtml
final case class MarkdownFile(fileInfo: FileInfo, html: NodeSeq) extends ParsedFile with HasHtml
final case class OtherFile(fileInfo: FileInfo) extends ParsedFile
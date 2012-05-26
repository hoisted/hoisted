package org.hoisted
package lib

import net.liftweb._
import builtin.snippet._
import common._
import http._
import util._
import Helpers._
import java.util.Locale
import java.io.{FileWriter, FileOutputStream, FileInputStream, File}
import xml.{Elem, NodeSeq}

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
      done <- tryo(writeFiles(transformedFiles, inDir, outDir))
    } yield HoistedTransformMetaData()
  }

  def writeFiles(toWrite: Seq[ParsedFile], inDir: File, outDir: File): Unit = {
    val bufLen = 4096
    val buffer = new Array[Byte](bufLen)

    def translate(source: File): File = {
      new File(outDir.getAbsolutePath + (source.getAbsolutePath.substring(inDir.getAbsolutePath.length)))
    }

    def calcFile(pf: ParsedFile with HasMetaData): File = translate(pf.fileInfo.file) // FIXME -- where do we put the file?

    def copy(from: File, to: File) {
      val in = new FileInputStream(from)
      try {
        to.getParentFile().mkdirs()
        val out = new FileOutputStream(to)
        try {
          var len = 0
          while ( {
            len = in.read(buffer, 0, bufLen); len >= 0
          }) {
            if (len > 0) out.write(buffer, 0, bufLen)
          }
        } finally {
          out.close()
        }
      } finally {
        in.close()
      }
    }

    toWrite.foreach {
      case pf: ParsedFile with HasHtml with HasMetaData => if (shouldEmitFile(pf)) {
        val where = calcFile(pf)
        where.getParentFile.mkdirs()
        val out = new FileWriter(where)
        try {
          Html5.write(pf.html.collect {
            case e: Elem => e
          }.headOption getOrElse <html/>, out, true, true)
        } finally {
          out.close()
        }
      }
      case f => copy(f.fileInfo.file, translate(f.fileInfo.file))
    }
  }

  def shouldEmitFile(pf: ParsedFile with HasMetaData): Boolean = true // FIXME look at metadata about file

  type TemplateLookup = PartialFunction[(List[String], String), ParsedFile]

  def createTemplateLookup(in: Seq[ParsedFile]): TemplateLookup = Map.empty // FIXME

  def snippets: PartialFunction[(String, String), Box[NodeSeq => NodeSeq]] = {
    val m = Map(("surround", "render") -> Full(Surround.render _),
      ("ignore", "render") -> Full(Ignore.render _),
      ("tail", "render") -> Full(Tail.render _),
      ("head", "render") -> Full(Head.render _),
      ("withparam", "render") -> Full(WithParam.render _),
      ("embed", "render") -> Full(Embed.render _),
      ("if", "render") -> Full(testAttr _)
    ).withDefaultValue(Empty)

    new PartialFunction[(String, String), Box[NodeSeq => NodeSeq]] {
      def isDefinedAt(in: (String, String)) = true

      def apply(in: (String, String)) = {
        m.apply(in._1.toLowerCase -> in._2.toLowerCase)
      }
    }
  }

  // Map.empty // FIXME

  def testAttr(in: NodeSeq): NodeSeq = {
    for {
      toTest <- S.attr("toTest").toList
      attr <- S.attr(toTest).toList if attr == "true"
      node <- in
    } yield node
  }

  /*
  S.mapSnippet("choose", chooseNode _)
  S.mapSnippet("a", link _)
  S.mapSnippet("xmenu", menu _)
  S.mapSnippet("subs", subs _)
*/

  def runTemplater(f: ParsedFile, templates: TemplateLookup): ParsedFile = {
    val lu = new PartialFunction[(Locale, List[String]), Box[NodeSeq]] {
      def isDefinedAt(in: (Locale, List[String])): Boolean = true

      def apply(in: (Locale, List[String])): Box[NodeSeq] = Empty
    }
    val localSnippets = snippets
    val session = new LiftSession("", Helpers.nextFuncName, Empty) with StatelessSession

    def insureChrome(todo: ParsedFile with HasMetaData, node: NodeSeq): NodeSeq =
      session.merge(if ((node \ "html" \ "body").length > 0) node
      else
        session.processSurroundAndInclude("Chrome", <lift:surround with="default" at="content">
          {node}
        </lift:surround>), Req.nil)

    S.initIfUninitted(session) {
      LiftRules.allowParallelSnippets.doWith(() => false) {
        LiftRules.allowAttributeSnippets.doWith(() => false) {
          LiftRules.snippetWhiteList.doWith(() => localSnippets) {
            LiftRules.externalTemplateResolver.doWith(() => () => lu) {
              f match {
                case todo: ParsedFile with HasHtml with HasMetaData =>
                  HtmlFile(todo.fileInfo,
                    insureChrome(todo,
                      session.processSurroundAndInclude(todo.fileInfo.pureName, todo.html)),
                    todo.metaData)
                case d => d
              }
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
    (in \ "content").headOption.map(_.child) getOrElse in


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
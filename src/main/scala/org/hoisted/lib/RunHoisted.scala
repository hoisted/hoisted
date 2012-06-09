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
import org.eclipse.jgit.api.Git
import xml._
import org.joda.time.DateTime
import MetadataMeta._

/**
 * This singleton will take a directory, find all the files in the directory
 * and then generate a static site in the output directory and return metadata about
 * the transformation
 */

object RunHoisted extends HoistedRenderer

trait EnvironmentManager {
  private var metadata: MetadataMeta.Metadata = Map()
  var menuEntries: List[MenuEntry] = Nil

  def menuTitle: NodeSeq => NodeSeq =
    "* *" #> (
      CurrentFile.box.map(computeTitle) openOr "Telegram Site")

  def siteName: NodeSeq => NodeSeq = {
    "* *" #> (findString(SiteNameKey, metadata) openOr "Telegram")
  }

  def menuItems: NodeSeq => NodeSeq =
  "li" #> menuEntries.map{
    case MenuEntry(pf, _) => "a *" #> computeLinkText(pf) &
    "a [href]" #> computeLink(pf) &
      "li [class+]" #> Full("active").filter(a => (pf eq CurrentFile.value))
  }

  def computeDestinationPathFunc: ParsedFile => PathAndSuffix = pf => pf.fileInfo.pathAndSuffix
  def findDefaultTemplate: List[ParsedFile] => Box[ParsedFile] = lpf => lpf.find{p =>
    p.pathAndSuffix.path == List("templates-hidden", "default" ) ||
      (p.findData(DefaultTemplateKey).flatMap(_.asBoolean) openOr false)
  }
  def needsTemplates: List[ParsedFile] => Boolean = lpf => findDefaultTemplate(lpf).isEmpty

  def computeMenuItems: List[ParsedFile] => List[MenuEntry] = pf =>
    pf.filter(shouldWriteHtmlFile).filter(isHtmlFile).filter(a => !isBlogPost(a)).
      filter(a => !isEvent(a)).map(pf => MenuEntry(pf, Nil))


  def isBlogPost: ParsedFile => Boolean = _.findData(PostKey).flatMap(_.asBoolean) openOr false

  def isEvent: ParsedFile => Boolean = _.findData(EventKey).flatMap(_.asBoolean) openOr false


  def isHtmlFile: ParsedFile => Boolean = {
    case x: HasHtml => true
    case _ => false
  }

  def shouldWriteHtmlFile: ParsedFile => Boolean =
  pf => {
    pf.findData(ServeKey).flatMap(_.asBoolean) openOr (pf.pathAndSuffix.path match {
    case "templates-hidden" :: _ => false
    case x => x.filter(_.startsWith("_")).isEmpty
  })
  }

  /**
  * Are there any blog posts
*/
  def hasBlogPosts: List[ParsedFile] => Boolean =
    pf =>
      (findBoolean(HasBlogKey, metadata) or findString(BlogRootKey, metadata).map(_ => true)) openOr
  pf.toStream.flatMap(pf => pf.findData(PostKey).flatMap(_.asBoolean).filter(a => a)).headOption.isDefined

  def computeBlogRoot: () => String = () => findString(BlogRootKey, metadata) openOr "/blog"

  /**
   * compute the URL of the Git repo with the template in it
 */
  def computeTemplateURL: () => String =
    () => (findMetadata(TemplateURLKey).flatMap(_.asString).filter(
    s => s.startsWith("http") && s.endsWith(".git")
    )) openOr "https://github.com/telegr-am/template-base.git"

  def loadTemplates: (HoistedRenderer, String, List[ParsedFile]) => List[ParsedFile] = (render, url, cur) =>
  {
    val cloner = Git.cloneRepository()
    cloner.setURI(url)
    val dir = File.createTempFile("fog", "dog")
    dir.delete()
    dir.mkdirs()
    cloner.setDirectory(dir)
    cloner.call()
    val allFiles = render.allFiles(dir, f => f.exists() && !f.getName.startsWith(".") && f.getName.toLowerCase != "readme" &&
    f.getName.toLowerCase != "readme.md")
    val fileInfo = allFiles.map(render.fileInfo(dir))
    val parsedFiles = fileInfo.flatMap(ParsedFile.apply _)

    val curSet = Set(cur.map(_.fileInfo.pathAndSuffix) :_*)

    parsedFiles.filter(pf => !curSet.contains(pf.fileInfo.pathAndSuffix)) ::: cur
  }

  def appendMetadata(key: MetadataKey, value: MetadataValue) {
    metadata = set(metadata, key, value)
  }

  /**
  * Find a metadata entry
  */
  def findMetadata(key: MetadataKey): Box[MetadataValue] = metadata.get(key)


  /**
  * Compute the title from the filename
*/
  def computeTitleFromFileName: (ParsedFile) => String =
  pf => capify(pf.pathAndSuffix.path.takeRight(1).head)

  /**
  * Replace the '_' with ' ' and then make the first character
  * of each word upper case
*/
  def capify: String => String = str =>
  str.replace('_', ' ').roboSplit(" ") match {
    case List("index") => "Home"
    case xs => xs.map(_.toList match {
      case c :: rest => (c.toUpper :: rest).mkString
      case _ => ""
    }).mkString(" ")
  }

  /**
  * Compute the title of the file
  */
  def computeTitle: (ParsedFile) => String = pf => {
    pf match {
      case md: ParsedFile with HasMetaData => md.findData(TitleKey).flatMap(_.asString) or
        pf.findData(LinkKey).flatMap(_.asString) openOr computeTitleFromFileName(pf)
      case _ => computeTitleFromFileName(pf)
    }
  }

  /**
  * Compute the link text for the file
*/
  def computeLinkText: (ParsedFile) => String =
  pf => pf.findData(LinkKey).flatMap(_.asString) openOr computeTitle(pf)

  /**
  * Collect the HTML files
*/
  def collectHtml: List[ParsedFile] => List[ParsedFile with HasHtml] =
  in => in.collect {
    case x: ParsedFile with HasHtml => x
  }

  /**
  * Collect the files to be rendered as Html and written to the filesystem
*/
  def collectRendered: List[ParsedFile] => List[ParsedFile with HasHtml] =
    in => in.collect {
      case x: ParsedFile with HasHtml with HasMetaData if shouldWriteHtmlFile(x) => x
    }

  def updateMetadata: (MetadataMeta.Metadata, FileInfo) => MetadataMeta.Metadata =
  (md, fi) => {
    var fixedMd = md
    // test to see if it's a post
    findBoolean(PostKey, fixedMd) match {
      case Full(_) => // do nothing
      case _ => findString(LayoutKey, fixedMd).map(_.toLowerCase) match {
        case Full("post") => fixedMd = set(fixedMd, PostKey, true)
        case Full(_) => // do nothing
        case _ => fi.pathAndSuffix.path match {
          case "_post" :: _ => fixedMd = set(fixedMd, PostKey, true)
          case _ => // do nothing
        }
      }
    }

    // test to see if it's an event
    findBoolean(EventKey, fixedMd) match {
      case Full(_) => // do nothing
      case _ => findString(LayoutKey, fixedMd).map(_.toLowerCase) match {
        case Full("event") => fixedMd = set(fixedMd, EventKey, true)
        case Full(_) => // do nothing
        case _ => fi.pathAndSuffix.path match {
          case "_event" :: _ => fixedMd = set(fixedMd, EventKey, true)
          case _ => // do nothing
        }
      }
    }

    // deal with the date
    findDate(DateKey, fixedMd) match {
      case Full(_) =>
      case _ => findDate(ValidFromKey, fixedMd) match {
        case Full(date) => fixedMd = set(fixedMd, DateKey, date)
        case _ =>
          ParsedFile.uglyParseDate(fi.name) match {
            case Full(date) => fixedMd = set(fixedMd, DateKey, date)
            case _ => fixedMd = set(fixedMd, DateKey, new DateTime(fi.file.lastModified()))
          }
      }
    }

    fixedMd
  }

  def insureHtmlSuffix: String => String = str => (str.trim.toLowerCase match {
    case s if s.endsWith(".html")  => s
    case s => s+".html"
  }) match {
    case s if s.startsWith("/") => s
    case s => "/"+s
  }

  def striptHtmlSuffix: String => String = str => if (str.endsWith(".html")) str.dropRight(5) else str

  def computeOutputFileName: ParsedFile => String = m => {
    val ret =
    m.findData(OutputPathKey).flatMap(_.asString).map(insureHtmlSuffix) openOr m.pathAndSuffix.path.mkString("/", "/", ".html")

    (m.findData(PostKey).flatMap(_.asBoolean).filter(a => a).map(a => computeBlogRoot()) openOr  "") + ret
  }

  def computeLink: ParsedFile => String = pf => striptHtmlSuffix(computeOutputFileName(pf)) match {
    case s if s.endsWith("/index") => s.dropRight(5)
    case s => s
  }
}

/**
* An instance of the EnvironmentManager that just inherits the default behavior
*/
class DefaultEnvironmentManager extends EnvironmentManager

/**
* The current environment manager, so it doesn't have to be passed around
*/
object HoistedEnvironmentManager extends ThreadGlobal[EnvironmentManager]

object CurrentFile extends ThreadGlobal[ParsedFile]

trait HoistedRenderer {
  def apply(inDir: File, outDir: File, environment: EnvironmentManager = new DefaultEnvironmentManager): Box[HoistedTransformMetaData] = {
    HoistedEnvironmentManager.doWith(environment) {
      for {
        deleteAll <- tryo(deleteAll(outDir))
        theDir <- tryo(outDir.mkdirs())
        allFiles <- tryo(allFiles(inDir,f => f.exists() && !f.getName.startsWith(".") && f.getName.toLowerCase != "readme" &&
          f.getName.toLowerCase != "readme.md"))
        fileInfo <- tryo(allFiles.map(fileInfo(inDir)))
        _parsedFiles = (fileInfo: List[FileInfo]).flatMap(ParsedFile.apply _)
        parsedFiles = ensureTemplates(_parsedFiles)

        fileMap = byName(parsedFiles)
        templates = createTemplateLookup(parsedFiles)
        menu = HoistedEnvironmentManager.value.computeMenuItems(parsedFiles)
        _ = HoistedEnvironmentManager.value.menuEntries = menu

        transformedFiles = parsedFiles.map(f => runTemplater(f, templates))
        done <- tryo(writeFiles(transformedFiles, inDir, outDir))
      } yield HoistedTransformMetaData()
    }
  }

  def ensureTemplates(in: List[ParsedFile]): List[ParsedFile] =
  if (HoistedEnvironmentManager.value.needsTemplates(in)) {
    val name = HoistedEnvironmentManager.value.computeTemplateURL()
    HoistedEnvironmentManager.value.loadTemplates(this, name, in)
  } else in

  def dropSuffix(in: String): String = {
    in.lastIndexOf(".") match {
      case x if x < 0 => in
      case x => in.substring(0, x)
    }
  }

  def captureSuffix(in: String): String = {
    in.lastIndexOf(".") match {
      case x if x < 0 => ""
      case x => in.substring(x + 1)
    }
  }



  def writeFiles(toWrite: Seq[ParsedFile], inDir: File, outDir: File): Unit = {
    val bufLen = 4096
    val buffer = new Array[Byte](bufLen)

    def translate(source: String): File = {
      new File(outDir.getAbsolutePath + source)
    }

    def outputFile(m: ParsedFile): String = HoistedEnvironmentManager.value.computeOutputFileName(m)


    def calcFile(pf: ParsedFile with HasMetaData): File = translate(pf match {
      case XmlFile(f, _, _, _, _) => outputFile(pf)
      case HtmlFile(f, _, _, _) => outputFile(pf)
      case MarkdownFile(f, _, _, _) => outputFile(pf)
      case f => f.fileInfo.relPath
    })

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
          }.headOption getOrElse <html/>, out, false, true)
        } finally {
          out.close()
        }
      }
      case f if (shouldEmitFile(f)) => copy(f.fileInfo.file, translate(f.fileInfo.relPath))
      case _ =>
    }
  }

  def shouldEmitFile(pf: ParsedFile): Boolean = HoistedEnvironmentManager.value.shouldWriteHtmlFile(pf)

  type TemplateLookup = PartialFunction[(List[String], String), ParsedFile]

  def createTemplateLookup(in: Seq[ParsedFile]): TemplateLookup = {
    def makeName(f: ParsedFile): (List[String], String) = {
      f match {
        case h: HasHtml => (dropSuffix(f.fileInfo.relPath).roboSplit("/"),"html")
        case f => (dropSuffix(f.fileInfo.relPath).roboSplit("/"), captureSuffix(f.fileInfo.relPath))
      }
    }
    Map(in.map(f => (makeName(f), f)) :_*)
  }



  def doTitle(ns: NodeSeq): NodeSeq = <head><title>{ns.text}</title></head>

  def snippets: PartialFunction[(String, String), Box[NodeSeq => NodeSeq]] = {
    val m = Map(("surround", "render") -> Full(Surround.render _),
      ("ignore", "render") -> Full(Ignore.render _),
      ("tail", "render") -> Full(Tail.render _),
      ("head", "render") -> Full(Head.render _),
      ("menu", "title") -> Full(HoistedEnvironmentManager.value.menuTitle),
      ("menu", "items") -> Full(HoistedEnvironmentManager.value.menuItems),
      ("site", "name") -> Full(HoistedEnvironmentManager.value.siteName),
      ("title", "render") -> Full(doTitle _),
      ("withparam", "render") -> Full(WithParam.render _),
      ("embed", "render") -> Full(Embed.render _),
      ("if", "render") -> Full(testAttr _),
      ("bootstraputil", "headcomment") -> Full((ignore: NodeSeq) => BootstrapUtil.headComment),
      ("bootstraputil", "bodycomment") -> Full((ignore: NodeSeq) => BootstrapUtil.bodyComment)
    ).withDefaultValue(Empty)

    new PartialFunction[(String, String), Box[NodeSeq => NodeSeq]] {
      def isDefinedAt(in: (String, String)) = true

      def apply(in: (String, String)) = {
        m.apply(in._1.toLowerCase -> in._2.toLowerCase)
      }
    }
  }

  def testAttr(in: NodeSeq): NodeSeq = {
    for {
      toTest <- S.attr("toTest").toList
      attr <- S.attr(toTest).toList if attr == "true"
      node <- in
    } yield node
  }

  object BootstrapUtil {
    def headComment = Unparsed("""    <!-- Le HTML5 shim, for IE6-8 support of HTML elements -->
                                   <!--[if lt IE 9]>
                                     <script src="https://html5shim.googlecode.com/svn/trunk/html5.js"></script>
                                   <![endif]-->""")
    def bodyComment = Unparsed("""  <!--[if IE]>
                        <link rel="stylesheet" type="text/css" href="/css/custom-theme/jquery.ui.1.8.16.ie.css"/>
                        <![endif]-->""")
  }

  def runTemplater(f: ParsedFile, templates: TemplateLookup): ParsedFile = {
    val lu = new PartialFunction[(Locale, List[String]), Box[NodeSeq]] {
      def isDefinedAt(in: (Locale, List[String])): Boolean = {

      true
      }

      def apply(in: (Locale, List[String])): Box[NodeSeq] =
      if (templates.isDefinedAt((in._2, "html"))) {
        val ret = templates((in._2, "html"))
        ret match {
          case h: HasHtml => Full(h.html)
          case _ => Empty
        }
      } else {
        Empty
      }
    }
    def localSnippets = snippets
    val session = new LiftSession("", Helpers.nextFuncName, Empty) with StatelessSession {
      override def stateful_? = false
    }

    def insureChrome(todo: ParsedFile with HasMetaData, node: NodeSeq): NodeSeq = {
      session.merge(if ((node \\ "html" \\ "body").length > 0) node
      else {
        session.processSurroundAndInclude("Chrome", <lift:surround with="default" at="content">
          {node}
        </lift:surround>)
      }, Req.nil)
    }

    S.initIfUninitted(session) {
      LiftRules.autoIncludeAjaxCalc.doWith(() => ignore => false) {
      LiftRules.allowParallelSnippets.doWith(() => false) {
        LiftRules.allowAttributeSnippets.doWith(() => false) {
          LiftRules.snippetWhiteList.doWith(() => localSnippets) {
            LiftRules.externalTemplateResolver.doWith(() => () => lu) {
              CurrentFile.doWith(f) {
              f match {
                case todo: ParsedFile with HasHtml with HasMetaData =>
                  HtmlFile(todo.fileInfo,
                    insureChrome(todo,
                      session.processSurroundAndInclude(todo.fileInfo.pureName, todo.html)),
                    todo.metaData, todo.uniqueId)
                case d => d
              }
            }
            }
          }
        }
      }
      }
    }
  }

  def fileInfo(inDir: File)(f: File): FileInfo = {
    val cp: String = f.getAbsolutePath().substring(inDir.getAbsolutePath.length)
    val pureName = f.getName
    val dp = pureName.lastIndexOf(".")
    val (name, suf) = if (dp <= 0) (pureName, None)
    else (pureName.substring(0, dp),
      Some(pureName.substring(dp + 1)))
    FileInfo(f, cp, name, pureName, suf)
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

  def allFiles(dir: File, filter: File => Boolean): List[File] = {
    if (!filter(dir)) Nil else if (dir.isDirectory()) {
      dir.listFiles().toList.flatMap(allFiles(_, filter))
    } else if (dir.isFile() && !dir.getName.startsWith(".")) List(dir)
    else Nil
  }


}

final case class HoistedTransformMetaData()

final case class FileInfo(file: File, relPath: String, name: String, pureName: String, suffix: Option[String]) {
  lazy val pathAndSuffix: PathAndSuffix =
    PathAndSuffix(relPath.toLowerCase.roboSplit("/").dropRight(1) ::: List(name.toLowerCase), suffix.map(_.toLowerCase))
}


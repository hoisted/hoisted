package org.hoisted.lib

import net.liftweb._
import common._
import http.Templates
import util._
import Helpers._
import MetadataMeta._
import org.joda.time.DateTime
import org.eclipse.jgit.api.Git
import java.io.File
import xml.{Node, NodeSeq}

/**
 * Created with IntelliJ IDEA.
 * User: dpp
 * Date: 6/13/12
 * Time: 1:07 PM
 * To change this template use File | Settings | File Templates.
 */

trait EnvironmentManager {
  private var metadata: MetadataMeta.Metadata = Map()
  var menuEntries: List[MenuEntry] = Nil
  var blogPosts: List[ParsedFile] = Nil
  var pages: List[ParsedFile] = Nil

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

  /**
   * Finds all the valid pages if they have a tag
   * @return
   */
  def findByTag: (String, Box[String]) => List[ParsedFile] = (tag, cmp) => {
    val key = MetadataKey(tag)

    pages.filter(_.findData(key) match {
      case fv@ Full(v) if cmp.isEmpty || fv.flatMap(_.asString) == cmp => true
      case _ => false
    }).filter(isValid).sortWith{
      case (a, b) =>
        val d1 = computeDate(a)
        val d2 = computeDate(b)
        d1.getMillis > d2.getMillis
    }
  }

  def computeDestinationPathFunc: ParsedFile => PathAndSuffix = pf => pf.fileInfo.pathAndSuffix

  private def findDefaultTemplate: List[ParsedFile] => Box[ParsedFile] = lpf => lpf.find{p =>
    p.pathAndSuffix.path == List("templates-hidden", "default" ) ||
      (p.findData(DefaultTemplateKey).flatMap(_.asBoolean) openOr false)
  }

  def needsTemplates: List[ParsedFile] => Boolean = lpf => findDefaultTemplate(lpf).isEmpty

  def computePosts: List[ParsedFile] => List[ParsedFile] = f => {
    val ret = f.filter(isBlogPost).sortWith(compareBlogPosts)
    ret
  }

  private def testTemplateName(name: String): Box[String] = {
    val ns = name.roboSplit("/")
    (Templates("templates-hidden" :: ns) or Templates(ns)).map(ignore => name)
  }

  def chooseTemplateName: ParsedFile => String = pf => {
    (pf.findData(TemplateKey).flatMap(_.asString).flatMap(testTemplateName(_)) or
    pf.findData(PostKey).flatMap(_.asBoolean).filter(a => a).flatMap(ignore => testTemplateName("post")) or
    pages.find(_.findData(DefaultTemplateKey).flatMap(_.asBoolean) openOr false).headOption.map(_.pathAndSuffix.path.mkString("/", "/", ""))) openOr
    "default"
  }


  def computeMenuItems: List[ParsedFile] => List[MenuEntry] = pf =>
    pf.filter(shouldWriteHtmlFile).filter(isHtmlFile).filter(a => !isBlogPost(a)).
      filter(a => !isEvent(a)).map(pf => MenuEntry(pf, Nil)).
      sortWith(compareMenuEntries)

  def compareMenuEntries: (MenuEntry, MenuEntry) => Boolean = {
    case (MenuEntry(m1, _), MenuEntry(m2, _)) =>
      (menuOrder(m1), menuOrder(m2)) match {
        case (Full(i1), Full(i2)) if i1 != i2 => i1 < i2
        case (Full(i1), e: EmptyBox) if i1 != 1000 => i1 < 1000
        case (e: EmptyBox, Full(i2)) if i2 != 1000 => 1000 < i2
        case _ => computeLinkText(m1).toLowerCase < computeLinkText(m2).toLowerCase
      }
  }

  @scala.annotation.tailrec
  final def makeShortHtml(in: List[Node], changed: Boolean): (NodeSeq, Boolean) = {
    if (in.length == 1) (in, changed) else {
    val ns: NodeSeq = in
    val tl = ns.text.length
    if (tl < 700) (ns, changed) else makeShortHtml(in.dropRight(1), true)
    }
  }

  def computeContent: ParsedFile => NodeSeq = {
    case h: HasHtml => h.html
    case _ => NodeSeq.Empty
  }

  def computeShortContent: ParsedFile => (NodeSeq, Boolean) = {
    case h: HasHtml => h.html.toList match {
      case x :: Nil => makeShortHtml(x.child.toList, false)
      case xs => makeShortHtml(xs, false)
    }
    case _ => (NodeSeq.Empty, false)
  }

  def compareBlogPosts: (ParsedFile, ParsedFile) => Boolean = {
    case (p1, p2) =>
      val d1 = computeDate(p1)
      val d2 = computeDate(p2)
      (d1.getMillis, d2.getMillis) match {
        case (dl1, dl2) if dl1 == dl2 => computeTitle(p1).toLowerCase < computeTitle(p2).toLowerCase
        case (dl1, dl2) => dl1 > dl2
      }
  }

  def menuOrder: ParsedFile => Box[Int] = pf =>
    pf.findData(OrderKey).flatMap(_.asInt) or (pf.fileInfo.pathAndSuffix.path match {
      case List("index") => Full(Integer.MIN_VALUE)
      case List("home") => Full(Integer.MIN_VALUE)
      case _ => Empty
    })

  def isBlogPost: ParsedFile => Boolean = _.findData(PostKey).flatMap(_.asBoolean) openOr false

  def isEvent: ParsedFile => Boolean = _.findData(EventKey).flatMap(_.asBoolean) openOr false

  def computeDate: ParsedFile => DateTime = pf => pf.findData(DateKey).flatMap(_.asDate) openOr
    new DateTime(pf.fileInfo.file.lastModified())

  def isHtmlFile: ParsedFile => Boolean = {
    case x: HasHtml => true
    case _ => false
  }

  def shouldWriteHtmlFile: ParsedFile => Boolean =
    pf => {
      pf.findData(ServeKey).flatMap(_.asBoolean) openOr (pf.pathAndSuffix.path match {
        case "templates-hidden" :: _ => false
        case x => x.filter(_.startsWith("_")).isEmpty ||
          (pf.findData(PostKey).flatMap(_.asBoolean).openOr( pf.findData(EventKey).flatMap(_.asBoolean) openOr false))
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
  def capify: String => String = str => {
    val ret = str.replace('_', ' ').roboSplit(" ") match {
      case List("index") => "Home"
      case xs => xs.map(_.toList match {
        case c :: rest => (c.toUpper :: rest).mkString
        case _ => ""
      }).mkString(" ")
    }
    ret
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
    pf => pf.findData(LinkKey).flatMap(_.asString). openOr(
      computeTitle(pf)
    )

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
      var pathling = fi.name

      // test to see if it's a post
      findBoolean(PostKey, fixedMd) match {
        case Full(_) => // do nothing
        case _ => findString(LayoutKey, fixedMd).map(_.toLowerCase) match {
          case Full("post") => fixedMd = set(fixedMd, PostKey, true)
          case Full(_) => // do nothing
          case _ => fi.pathAndSuffix.path match {
            case "_post" :: _ => fixedMd = set(fixedMd, PostKey, true)
            case "_posts" :: _ => fixedMd = set(fixedMd, PostKey, true)
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
                val rx = """^([0-9]{2,4}-[0-9]{1,2}-[0-9]{1,2})-+(.*)""".r
                rx.findFirstMatchIn(fi.name).foreach(m => pathling = m.group(2))
              case _ => fixedMd = set(fixedMd, DateKey, new DateTime(fi.file.lastModified()))
            }
        }
      }

      if (findBoolean(PostKey, fixedMd) or findBoolean(EventKey, fixedMd) openOr false) {
        fixedMd = set(fixedMd, OutputPathKey, "/"+pathling)
      }

      fixedMd
    }

  def insureHtmlSuffix: String => String = str => multiSlash.replaceAllIn( (str.trim.toLowerCase match {
    case s if s.endsWith(".html")  => s
    case s => s+".html"
  }) match {
    case s if s.startsWith("/") => s
    case s => "/"+s
  }, "/")

  lazy val multiSlash = """/{2,}""".r

  def striptHtmlSuffix: String => String = str => if (str.endsWith(".html")) str.dropRight(5) else str

  def computeOutputFileName: ParsedFile => String = m => {
    if (isHtml(m)) {
    val ret =
      m.findData(OutputPathKey).flatMap(_.asString).map(insureHtmlSuffix) openOr m.pathAndSuffix.path.mkString("/", "/", ".html")

    multiSlash.replaceAllIn((m.findData(PostKey).flatMap(_.asBoolean).filter(a => a).map(a => computeBlogRoot()) openOr  "") + ret, "/")
    } else {
        m.findData(OutputPathKey).flatMap(_.asString) openOr m.pathAndSuffix.path.mkString("/", "/", "")
    }
  }

  def computeLink: ParsedFile => String = pf =>
    pf.findData("redirect").flatMap(_.asString) openOr
    striptHtmlSuffix(computeOutputFileName(pf)) match {
    case s if s.endsWith("/index") => s.dropRight(5)
    case s => s
  }

  def isHtml: ParsedFile => Boolean = pf => {
    pf match {
      case hh: HasHtml =>
        pf.findData(TypeKey).flatMap(_.asString) match {
          case Full("html") => true
          case Full(_) => false
          case _ => true
        }
      case _ => false
    }
  }


  def isValid: ParsedFile => Boolean = pf => {
    def computeValidFrom: Boolean =
      pf.findData(ValidFromKey).flatMap(_.asDate).map(_.getMillis < Helpers.millis) openOr
        (pf.findData(DateKey).flatMap(_.asDate).map(_.getMillis < Helpers.millis) openOr
          pf.fileInfo.file.lastModified() < Helpers.millis)

    pf.findData(ValidToKey).flatMap(_.asDate).map(_.getMillis > Helpers.millis) match {
      case Full(true) => computeValidFrom
      case Full(false) => false
      case _ => computeValidFrom
    }
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

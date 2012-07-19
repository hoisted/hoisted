package org.hoisted.lib

import net.liftweb._
import builtin.snippet._
import common._
import common.Full
import http.{SessionMemoize, RequestVar, Templates}
import util._
import Helpers._
import MetadataMeta._
import org.joda.time.DateTime
import org.eclipse.jgit.api.Git
import java.io.{PrintWriter, OutputStream, File}
import xml.{Node, NodeSeq}
import org.joda.time.format.ISODateTimeFormat

/**
 * Created with IntelliJ IDEA.
 * User: dpp
 * Date: 6/13/12
 * Time: 1:07 PM
 * To change this template use File | Settings | File Templates.
 */

trait EnvironmentManager {
  private var _metadata: MetadataMeta.Metadata = Map()
  var menuEntries: List[MenuEntry] = Nil
  var blogPosts: List[ParsedFile] = Nil
  var pages: List[ParsedFile] = Nil
  def metadata: MetadataMeta.Metadata = _metadata

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

  def beginRendering: ParsedFile => Unit = pf => ()

  def endRendering: ParsedFile => Unit = pf => ()


  def snippets: PartialFunction[(String, String), Box[NodeSeq => NodeSeq]] = snippetCalculator.get

  var externalSnippets: List[PartialFunction[(String, String), Box[NodeSeq => NodeSeq]]] = Nil

  object snippetCalculator extends RequestVar[PartialFunction[(String, String), Box[NodeSeq => NodeSeq]]](
  externalSnippets.foldLeft[PartialFunction[(String, String),
    Box[NodeSeq => NodeSeq]]](Map.empty)(_ orElse _) orElse baseSnippets
  )

  def baseSnippets: PartialFunction[(String, String), Box[NodeSeq => NodeSeq]] = {
    val m: Map[(String, String), Box[NodeSeq => NodeSeq]] = Map(("surround", "render") -> Full(Surround.render _),
      ("ignore", "render") -> Full(Ignore.render _),
      ("tail", "render") -> Full(Tail.render _),
      ("head", "render") -> Full(Head.render _),
      ("a", "render") -> Full(BaseSnippets.doA),
      ("choose", "render") -> Full(BaseSnippets.doChoose _),
      ("subs", "render") -> Full(BaseSnippets.doSubs _),
      ("xmenu", "render") -> Full(BaseSnippets.doXmenu),
      ("bind", "render") -> Full(BaseSnippets.doBind),
      ("menu", "title") -> Full(menuTitle),
      ("menu", "items") -> Full(menuItems),
      ("site", "name") -> Full(siteName),
      ("group", "render") -> Full(BaseSnippets.group(this)),
      ("google-analytics", "render") -> Full(BaseSnippets.googleAnalytics),
      ("google_analytics", "render") -> Full(BaseSnippets.googleAnalytics),
      ("title", "render") -> Full(BaseSnippets.doTitle _),
      ("withparam", "render") -> Full(WithParam.render _),
      ("embed", "render") -> Full(Embed.render _),
      ("archived_posts", "render") -> Full(BaseSnippets.archivedPosts),
      ("if", "render") -> Full(BaseSnippets.testAttr _),
      ("xform", "render") -> Full(BaseSnippets.xform),
      ("page-info", "render") -> Full(BaseSnippets.pageInfo),
      ("page_info", "render") -> Full(BaseSnippets.pageInfo),
      ("pageinfo", "render") -> Full(BaseSnippets.pageInfo),
      ("blog", "posts") -> Full(BaseSnippets.blogPosts),
      ("move_top", "render") -> Full(BaseSnippets.moveTop),
      ("twitter", "render") -> Full(BaseSnippets.doTwitter),
      ("search", "render") -> Full(BaseSnippets.search),
      ("htag-list", "render") -> Full(BaseSnippets.hTags),
      ("htag_list", "render") -> Full(BaseSnippets.hTags),
      ("bootstraputil", "headcomment") -> Full((ignore: NodeSeq) => BootstrapUtil.headComment),
      ("bootstraputil", "bodycomment") -> Full((ignore: NodeSeq) => BootstrapUtil.bodyComment)
    ).withDefaultValue(Empty)

    new PartialFunction[(String, String), Box[NodeSeq => NodeSeq]] {
      def isDefinedAt(in: (String, String)) = true

      def apply(in: (String, String)): Box[NodeSeq => NodeSeq] = {
        m.apply(in._1.toLowerCase -> in._2.toLowerCase)
      }
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
    pf.filter(shouldWriteFile).filter(isHtmlFile).filter(a => !isBlogPost(a)).
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
  final def makeShortHtml(in: List[Node], len: Int = 700, changed: Boolean = false): (NodeSeq, Boolean) = {
    if (in.length == 1) (in, changed) else {
    val ns: NodeSeq = in
    val tl = ns.text.length
    if (tl < len) (ns, changed) else makeShortHtml(in.dropRight(1), len, true)
    }
  }

  def computeContent: ParsedFile => NodeSeq = {
    case h: HasHtml => h.html
    case _ => NodeSeq.Empty
  }

  def computeShortContent(pf: ParsedFile, len: Int = 700): (NodeSeq, Boolean) = pf match {
    case h: HasHtml => h.html.toList match {
      case x :: Nil => makeShortHtml(x.child.toList, len, false)
      case xs => makeShortHtml(xs, len, false)
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

  def isBlogPost: ParsedFile => Boolean = f =>  f.findData(PostKey).flatMap(_.asBoolean) openOr
  (f.findData(TypeKey).flatMap(_.asString).map(_.toLowerCase) == Full("post"))

  def isEvent: ParsedFile => Boolean = f =>  f.findData(EventKey).flatMap(_.asBoolean) openOr
    (f.findData(TypeKey).flatMap(_.asString).map(_.toLowerCase) == Full("event"))

  def computeDate: ParsedFile => DateTime = pf => pf.findData(DateKey).flatMap(_.asDate) or
    (pf.fileInfo.file.map(ff => new DateTime(ff.lastModified()))) openOr new DateTime()

  def isHtmlFile: ParsedFile => Boolean = {
    case x: HasHtml => true
    case _ => false
  }

  def shouldWriteFile: ParsedFile => Boolean =
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
    _metadata = set(metadata, key, value)
  }

  /**
   * Find a metadata entry
   */
  def findMetadata(key: MetadataKey): Box[MetadataValue] = metadata.get(key)


  private object titleFNMemo extends SessionMemoize[Hashly, String] {
    override lazy val __nameSalt = Helpers.nextFuncName
  }

  /**
   * Compute the title from the filename
   */
  def computeTitleFromFileName: (ParsedFile) => String =
    pf => titleFNMemo(pf, capify(pf.pathAndSuffix.path.takeRight(1).head))

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

  private object titleMemo extends SessionMemoize[Hashly, String] {
    override lazy val __nameSalt = Helpers.nextFuncName
  }

  /**
   * Compute the title of the file
   */
  def computeTitle: (ParsedFile) => String = pf => titleMemo(pf, {
    pf match {
      case md: ParsedFile with HasMetaData => md.findData(TitleKey).flatMap(_.asString) or
        pf.findData(LinkKey).flatMap(_.asString) openOr computeTitleFromFileName(pf)
      case _ => computeTitleFromFileName(pf)
    }
  })


  private case class Hashly(i: Int)

  private object Hashly {
    implicit def anyToHashly(in: Any): Hashly = Hashly(System.identityHashCode(in))
  }

  private object linkTextMemo extends SessionMemoize[Hashly, String] {
    override lazy val __nameSalt = Helpers.nextFuncName
  }

  /**
   * Compute the link text for the file
   */
  def computeLinkText: (ParsedFile) => String =
    pf => linkTextMemo(pf, pf.findData(LinkKey).flatMap(_.asString). openOr(
      computeTitle(pf)
    ))

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
      case x: ParsedFile with HasHtml with HasMetaData if shouldWriteFile(x) => x
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
              case _ => fixedMd = set(fixedMd, DateKey, fi.file.map(ff => new DateTime(ff.lastModified())).getOrElse(new DateTime()))
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

  private object outFileNameMemo extends SessionMemoize[Hashly, String] {
    override lazy val __nameSalt = Helpers.nextFuncName
  }

  def computeOutputFileName: ParsedFile => String = m =>  outFileNameMemo(m, {
    if (isHtml(m)) {
    val ret =
      m.findData(OutputPathKey).flatMap(_.asString).map(insureHtmlSuffix) openOr m.pathAndSuffix.path.mkString("/", "/", ".html")

    multiSlash.replaceAllIn((m.findData(PostKey).flatMap(_.asBoolean).filter(a => a).map(a => computeBlogRoot()) openOr  "") + ret, "/")
    } else {
        m.findData(OutputPathKey).flatMap(_.asString) openOr m.pathAndSuffix.path.mkString("/", "/", m.pathAndSuffix.suffix.map(s => "."+s).getOrElse(""))
    }
  })

  private object linkMemo extends SessionMemoize[Hashly, String] {
    override lazy val __nameSalt = Helpers.nextFuncName
  }

  def computeLink: ParsedFile => String = pf =>
    linkMemo(pf, pf.findData(RedirectKey).flatMap(_.asString) openOr
    striptHtmlSuffix(computeOutputFileName(pf)) match {
    case s if s.endsWith("/index") => s.dropRight(5)
    case s => s
  })

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

  def w3cFormattedDate: ParsedFile => String = pf => ParsedFile.w3cDateTimeFormat.print(computeDate(pf))

  def syntheticFiles: () => Seq[ParsedFile] = () => {
    val bpSynt: List[ParsedFile] =
     if (!(findMetadata(NoSyntheticRssFile).flatMap(_.asBoolean) openOr false)) {
      blogPosts.filter(isValid).sortWith{
      case (a, b) =>
        val d1 = computeDate(a)
        val d2 = computeDate(b)
        d1.getMillis > d2.getMillis
    }.collect{case hh: HasHtml => hh}.take(10) match {
      case Nil => Nil
      case bp =>
        postMergeTransforms = ("head *+" #> <link rel="alternate" type="application/rss+xml" href="/rss.xml"/>) :: postMergeTransforms

        val toShow = bp

        List(SyntheticFile(() => FileInfo(Empty, "/", "rss", "rss.xml", Some("xml")),
          () => Map(DateKey -> DateTimeMetadataValue(new DateTime())),
          out => {
            val pw = new PrintWriter(out)
            pw.println("<?xml version=\"1.0\" encoding=\"UTF-8\"?>")
            val xml =
              <feed xmlns="http://www.w3.org/2005/Atom">
                <title type="text">{siteTitle()}</title>
                  <link href={siteLink()}/>
                <updated>{w3cFormattedDate(toShow.head)}</updated>
                <author>{
                  val names: List[String] = (toShow.flatMap(_.findData(AuthorKey).toList).flatMap(_.asString) :::
                    findMetadata(SiteAuthorKey).toList.flatMap(_.asString.toList))

                  val ret = names.flatMap {
                  a =>
                    <name>{a}</name>
                }
                  ret
                  }</author>
                <id>{siteGuid()}</id>{toShow.map(post =>
                <entry>
                  <title type="text">{computeTitle(post)}</title>
                    <link href={siteLink() + computeLink(post)}/>
                  <id>{computeGuid(post)}</id>
                  <author>
                    {(post.findData(AuthorKey).flatMap(_.asString) or
                    findMetadata(SiteAuthorKey).flatMap(_.asString)).toList.flatMap(
                    a => <name>{a}</name>
                  )}
                  </author>
                  <updated>{w3cFormattedDate(post)}</updated>
                  <summary type="html">
                    {makeShortHtml(post.html.toList) match {
                    case (ns, false) => ns
                    case (ns, _) => ns.dropRight(1)
                  }
                    }
                  </summary>
                </entry>)}
              </feed>

            pw.print(xml.toString)
            pw.flush()
            pw.close()
          }))
    }} else Nil

    bpSynt ::: Nil
  }

  def computeGuid: ParsedFile => String = pf => siteGuid() +":"+Helpers.hashHex(computeLink(pf))

  def siteGuid: () => String = () => Helpers.hashHex(siteLink())

  def siteLink: () => String = () => findMetadata(SiteLinkKey).flatMap(_.asString) openOr "http://telegr.am"

  def siteTitle: () => String = () => findMetadata(SiteNameKey).flatMap(_.asString) openOr "Telegram"

  var globalTransforms: List[NodeSeq => NodeSeq] = Nil

  def computeTransforms: ParsedFile => Seq[NodeSeq => NodeSeq] = pf => globalTransforms

  var postMergeTransforms: List[NodeSeq => NodeSeq] = Nil

  def computePostMergeTransforms: ParsedFile => Seq[NodeSeq => NodeSeq] = pf => postMergeTransforms

  def computeFromToDates: ParsedFile => (Box[DateTime], Box[DateTime]) = pf =>
  (pf.findData(ValidFromKey).flatMap(_.asDate) or
    pf.findData(DateKey).flatMap(_.asDate), pf.findData(ValidToKey).flatMap(_.asDate))

  def isValid: ParsedFile => Boolean = pf => {
    def computeValidFrom: Boolean =
      pf.findData(ValidFromKey).flatMap(_.asDate).map(_.getMillis < Helpers.millis) or
        pf.findData(DateKey).flatMap(_.asDate).map(_.getMillis < Helpers.millis) or
          pf.fileInfo.file.map(_.lastModified() < Helpers.millis) openOr false

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

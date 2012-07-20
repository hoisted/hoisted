package org.hoisted.lib

import xml.{Unparsed, Elem, Text, NodeSeq}
import net.liftweb._
import common._
import util._
import Helpers._
import http.S
import http.js._
import JsCmds._
import builtin.snippet._
import org.joda.time.format.{DateTimeFormat}
import actors.remote.Node
import collection.mutable.ListBuffer
import scala.xml
import scala.xml

/**
 * Created with IntelliJ IDEA.
 * User: dpp
 * Date: 7/2/12
 * Time: 6:14 PM
 * To change this template use File | Settings | File Templates.
 */

object BaseSnippets {
  /*
  sortWith{
    case (a, b) =>
      val d1 = env.computeDate(a)
      val d2 = env.computeDate(b)
      d1.getMillis > d2.getMillis
  }.collect{case hh: HasHtml => hh}.
   */

  lazy val months = Array("January", "February", "March", "April",
  "May", "June", "July", "August", "September", "October",
  "November", "December")

  def archivedPosts: NodeSeq => NodeSeq = {
    val e2 = env
    val byYear = e2.blogPosts.filter(e2.isValid).groupBy(a => e2.computeDate(a).getYear)

    ClearClearable andThen "@year-block" #> byYear.keys.
      toList.sortWith(_ > _).
      map(year => "@year *" #> year & "@month-block" #> {
      val byMonth = byYear(year).groupBy(a => e2.computeDate(a).getMonthOfYear)
      byMonth.keys.toList.sortWith(_ > _).
      map(month => "@month *" #> months(month - 1) & "@post-block" #> {
        val posts = byMonth(month).sortWith{
          case (a, b) =>
            val d1 = env.computeDate(a)
            val d2 = env.computeDate(b)
            d1.getMillis > d2.getMillis
        }

        posts.map(post =>
        "@post-date *" #> DateTimeFormat.longDate().print(e2.computeDate(post)) &
        "@post-title *" #> e2.computeTitle(post) & "@post-title [href]" #> e2.computeLink(post))
      })
    })
  }

  def env = HoistedEnvironmentManager.value

  def doA: NodeSeq => NodeSeq = ns => {
    (for {
      name <- S.attr("name")
      it <- env.findByTag("name", Full(name)).headOption
    } yield <a href={env.computeLink(it)}>{ns}</a>) openOr ns
  }

  def doXmenu: NodeSeq => NodeSeq = ns => {
    val activeClass = S.attr("active_class") openOr "active"
    val menus: List[ParsedFile] = HoistedEnvironmentManager.value.findByTag("menu", Empty)
    val sorted = menus.sortWith((a, b) =>
      (a.findData(OrderKey).flatMap(_.asInt) openOr 0) <
        (b.findData(OrderKey).flatMap(_.asInt) openOr 0)
    )

    if ((ns \\ "item").filter {
      case e: Elem => e.prefix == "menu"
      case _ => false
    }.isDefined) {
      sorted.flatMap(fr =>
        bind("menu", ns, "item" -> env.computeLinkText(fr),
          FuncAttrOptionBindParam("class", (value: NodeSeq) =>
            if (fr eq CurrentFile.value) Some(value) else None, "class"),
          AttrBindParam("href", env.computeLink(fr), "href"))
      )
    } else {
      ("*" #> sorted.map(fr =>
        "a [href]" #> env.computeLink(fr) &
          "a *" #> env.computeLinkText(fr) andThen
          (if (fr eq CurrentFile.value)
            ("* [class+]" #> activeClass)
          else
            PassThru))).apply(ns)
    }
  }

  def doTitle(ns: NodeSeq): NodeSeq = <head>
    <title>
      {ns.text}
    </title>
  </head>

  def doBind: NodeSeq => NodeSeq = ignore => {
    S.attr("name").map(name => <div id={name}></div>) openOr NodeSeq.Empty
  }

  def hTags: NodeSeq => NodeSeq = {
    val depth = S.attr("depth").flatMap(Helpers.asInt(_)) openOr 2
    val info: List[MetadataValue] = CurrentFile.value.findData(HTagsKey) match {
      case Full(ListMetadataValue(lst)) => lst
      case _ => Nil
    }

    val depthClass = S.attr("class") openOr "depth"
    import Helpers._

    ("data-htag=root" #>
      (for {
        md <- info
          map <- md.map.toList
          rd <- map.get(HTagLevelKey).toList.flatMap(_.asInt) if rd <= depth
          body <- map.get(HTagBodyKey).flatMap(_.asNodeSeq)
          id <- map.get(HTagIdKey).flatMap(_.asString)
        } yield
        "data-htag=root [class+]" #> (depthClass + rd) & "a *" #> body & "a [href]" #> ("#"+id) andThen  "* [data-htag]" #> (Empty: Box[String])))
  }

  def doSubs(in: NodeSeq): NodeSeq = {
    val info = for {
      tpe <- S.attr("type").toList
      max = S.attr("max").flatMap(asInt).filter(_ > 0) openOr 10
      rec <- env.findByTag("sub", Full(tpe)).take(max)
      val title = rec.findData(MetadataKey("sub")).flatMap(_.asNodeSeq) openOr Text("Happening")
      val desc = rec.findData(MetadataKey("desc")).flatMap(_.asNodeSeq)
    } yield (rec, title, desc)

    if ((in \\ "title").filter{
      case e: Elem => e.prefix == "sub"
      case _ => false
    }.isDefined) {
      info.flatMap{ case (rec, title, desc) =>
        Helpers.bind("sub", in,
          "title" -> title, "desc" -> desc,
          AttrBindParam("href", Text(rec.findData(RedirectKey).flatMap(_.asString) openOr env.computeLink(rec)), "href"))
      }
    } else {
      ("*" #> info.map{
        case (rec, title, desc) => "@title *" #> title &
          "@desc *" #> desc &
          "a [href]" #> (rec.findData(RedirectKey).flatMap(_.asString) openOr env.computeLink(rec))
      }).apply(in)
    }
  }

  def search: NodeSeq => NodeSeq = ns => {
    val it: Elem= ns.toList.collect{case e: Elem => e}.headOption.getOrElse(<div/>)

    <form action="https://www.google.com/search">
      <input name="q"></input> <input type="hidden" name="as_sitesearch" value={env.siteLink()}></input>
      <input type="submit" value={S.?("Search")}/>
    </form> % it.attributes
  }

  def doTwitter: NodeSeq => NodeSeq = ns => {
    val user = S.attr("user") openOr "_telegram"
    Unparsed(
    """<script charset="utf-8" src="https://widgets.twimg.com/j/2/widget.js"></script>
      <script>
      // <![CDATA[
      new TWTR.Widget({
        version: 2,
        type: 'profile',
        rpp: """+S.attr("rpp").flatMap(Helpers.asInt).openOr(4)+""",
        interval: """+S.attr("interval").flatMap(Helpers.asInt).openOr(30000)+""",
        width: 'auto',
        height: """+S.attr("height").flatMap(Helpers.asInt).openOr(300)+""",
        theme: {
          shell: {
            background: """+S.attr("shell-background").openOr("#bfbfbf").encJs+""",
            color: """+S.attr("shell-color").openOr("#000000").encJs+"""
          },
          tweets: {
            background: """+S.attr("tweets-background").openOr("#ffffff").encJs+""",
            color: """+S.attr("tweets-color").openOr("#000000").encJs+""",
            links: """+S.attr("tweets-links").openOr("#292382").encJs+"""
          }
        },
        features: {
          scrollbar: true,
          loop: false,
          live: true,
          behavior: 'all'
        }
      }).render().setUser("""+user.encJs+""").start();
      // ]]>
      </script>"""

  )}

  def group(env: EnvironmentManager): NodeSeq => NodeSeq = {
    val by = (S.attr("by") openOr "post").toLowerCase
    val order = (S.attr("order") openOr "date").toLowerCase

    val descending = (S.attr("descending").flatMap(Helpers.asBoolean(_)) openOr false)

    val filterFunc: ParsedFile => Boolean = by match {
      case "post" => env.isBlogPost
      case "event" => env.isEvent
      case x => p => p.findData(TypeKey).flatMap(_.asString).map(_.toLowerCase) == Full(x)
    }

    val pages = env.pages.filter(filterFunc)

    val sortFunc1: (ParsedFile, ParsedFile) => Boolean = order match {
      case "date" => (a, b) => {
        val ad = env.computeDate(a)
        val bd = env.computeDate(b)
        ad.getMillis < bd.getMillis
      }
      case "name" => (a, b) => env.computeLinkText(a).toLowerCase < env.computeLinkText(b).toLowerCase
      case "title" => (a, b) => env.computeTitle(a).toLowerCase < env.computeTitle(b).toLowerCase
      case x =>
        val k = MetadataKey(x)
        (a, b) => a.findData(k).flatMap(_.asString).map(_.toLowerCase).openOr("") <
          b.findData(k).flatMap(_.asString).map(_.toLowerCase).openOr("")
    }


    val sortFunc: (ParsedFile, ParsedFile) => Boolean =
      if (!descending) sortFunc1 else (a, b) => !sortFunc1(a, b)

    val sorted = pages.sortWith(sortFunc)
    val f = DateTimeFormat.shortDate()

    ("data-post=item" #> sorted.map {
      p =>
        val (short, more) = env.computeShortContent(p)

        "data-post=link [href]" #> env.computeLink(p) &
          "data-post=link *" #> env.computeLinkText(p) &
          "data-post=shortcontent" #> short &
          (if (more) ("data-post=more" #> ("a [href]" #> env.computeLink(p))) else "data-post=more" #> (Empty: Box[String])) &
          "data-post=content" #> env.computeContent(p) &
          "data-post=date *" #> f.print(env.computeDate(p))
    }) andThen "* [data-post]" #> (Empty: Box[String])
  }

  def googleAnalytics: NodeSeq => NodeSeq = ns => {
    val xml = (S.attr("id") or S.attr("googleid"))map(id => """<script type="text/javascript">
      // <![CDATA[
  var _gaq = _gaq || [];
  _gaq.push(['_setAccount', """+id.encJs+"""]);
  _gaq.push(['_trackPageview']);

  (function() {
    var ga = document.createElement('script'); ga.type = 'text/javascript'; ga.async = true;
    ga.src = ('https:' == document.location.protocol ? 'https://ssl' : 'http://www') + '.google-analytics.com/ga.js';
    var s = document.getElementsByTagName('script')[0]; s.parentNode.insertBefore(ga, s);
  })();
// ]]>
    </script>""")

    xml.map(v => <lift:tail>{Unparsed(_)}</lift:tail>) openOr NodeSeq.Empty

  }

  def doChoose(in: NodeSeq): NodeSeq = {
    val num = S.attr("cnt").flatMap(Helpers.asInt) openOr 1


    val in2 =
      S.session.map(session => session.processSurroundAndInclude("stuff", in)) openOr in

    val lst: List[Elem] = in2.toList.collect {
      case e: Elem => e
    }

    val ids = lst.map(ignore => Helpers.nextFuncName)

    val refresh = S.attr("refresh").flatMap(Helpers.asInt(_)).map(i => if (i < 500) i * 1000 else i)

    val lst2 = lst.zip(ids).map{
      case (elem, id) => elem % ("id" -> id) % ("style" -> "display:none")
    }
    val varName = Helpers.nextFuncName

    lst2 ++ Script(JsCmds.Run(varName+" = ["+ids.map(_.encJs).mkString(",")+"];\n"+
      varName+"_func = function() {"+
      """

        for (var i = 0; i < """+(lst.length * 10)+"""; i++) {
          var tmp = """+varName+""";
          var len = tmp.length;
          var x = Math.floor(Math.random() * len);
          var y = Math.floor(Math.random() * len);
          var q = tmp[x];
          tmp[x] = tmp[y];
          tmp[y] = q;
        };

        for (var i = 0; i < """+varName+""".length; i++) {
          document.getElementById("""+varName+"""[i]).style.display = "none";
        }

        for (var i = 0; i < """+num+"""; i++) {
          document.getElementById("""+varName+"""[i]).style.display = "";
        };
                                              """+refresh.map(s => "setTimeout('"+varName+"_func()', "+s+");").openOr("")+"""
        };

        """ + varName + """_func();
        """) )
  }

  def moveTop: NodeSeq => NodeSeq = in => {
    val xf: Box[NodeSeq => NodeSeq] = for {
      from <- S.attr("from")
      to <- S.attr("to")
    } yield {

      var foundFrom: Box[NodeSeq] = Empty
      var elemCnt = 0;
      var removePageHeader = false
      var otherNodes: ListBuffer[xml.Node] = new ListBuffer[xml.Node]()
      (("#"+from+" *") #> ((ns: NodeSeq) => {
        ns.toList.foreach {
          case e: Elem => if (elemCnt == 0 && ((e.label.toLowerCase match {
            case "h1" | "h2" | "h3" | "h4" | "h5" | "h6" => true
            case _ => false
          }) || (e.attribute("class").map{s =>
            val s2 = s.text.toLowerCase
            s2.contains("hero-unit") || s2.contains("page-header")
          } getOrElse false))) {
            removePageHeader = e.attribute("class").map{s =>
              val s2 = s.text.toLowerCase
              s2.contains("hero-unit") || s2.contains("page-header")
            } getOrElse false

            foundFrom = Full(e)
          } else {
            otherNodes.append(e)
          }
            elemCnt += 1
          case other => otherNodes.append(other)
        }
        ns
      })).apply(in)

      if (foundFrom.isDefined) {
        val q = ("#"+to+" *") #> foundFrom & ("#"+from+" *") #> (otherNodes.toList: NodeSeq)

        if (removePageHeader) {
          q & ("#"+to+" [class]") #> (None: Option[String])
        } else q
      } else {
        ("#"+to) #> foundFrom
      }
    }

    xf.map(_.apply(in)) openOr in
  }

  def pageInfo: NodeSeq => NodeSeq = ns => {
    val name = S.attr("name") or S.attr("info")

    val data: Box[NodeSeq] = name.map(MetadataKey(_)).flatMap(CurrentFile.value.findData(_)).flatMap(data =>
      data.asListString.map(a => Text(a.mkString(", ")): NodeSeq) or
        data.asNodeSeq or data.asString.map(a => Text(a): NodeSeq) or
        data.asDate.map(d => Text(DateTimeFormat.longDate().print(d)): NodeSeq))

    ns match {
      case e: Elem => ("* *" #> data).apply(e)
      case x => data openOr NodeSeq.Empty
    }
  }


  def blogPosts: NodeSeq => NodeSeq = {
    val posts = HoistedEnvironmentManager.value.blogPosts.take(S.attr("max").flatMap(Helpers.asInt _) openOr Integer.MAX_VALUE)

    val m = HoistedEnvironmentManager.value

    val f = DateTimeFormat.shortDate()

    ("data-post=item" #> posts.map {
      p =>
        val (short, more) = m.computeShortContent(p)

        "data-post=link [href]" #> m.computeLink(p) &
          "data-post=link *" #> m.computeLinkText(p) &
          "data-post=shortcontent" #> short &
          (if (more) ("data-post=more" #> ("a [href]" #> m.computeLink(p))) else "data-post=more" #> (Empty: Box[String])) &
          "data-post=content" #> m.computeContent(p) &
          "data-post=date *" #> f.print(m.computeDate(p))
    } andThen "* [data-post]" #> (Empty: Box[String]))
  }

  def testAttr(in: NodeSeq): NodeSeq = {
    for {
      toTest <- S.attr("toTest").toList
      attr <- S.attr(toTest).toList if attr == "true"
      node <- in
    } yield node
  }


  def xform: NodeSeq => NodeSeq = ns => {
    for {
      css <- (ns.collect {
        case e: Elem => e
      }.flatMap(_.attribute("data-css")).headOption.map(_.text): Option[String])
      thing: NodeSeq = S.attr("kids").
        flatMap(Helpers.asBoolean).
        filter(a => a).map(ignore => ns.collect {
        case e: Elem => e
      }.flatMap(_.child): NodeSeq) openOr
        (("* [data-css]" #> (None: Option[String])).apply(ns))
      xf <- (Helpers.tryo(css #> thing): Box[NodeSeq => NodeSeq])
    } {
      PostPageTransforms.set(PostPageTransforms.get :+ xf)
    }

    NodeSeq.Empty
  }
}

object BootstrapUtil {
  def headComment = Unparsed( """    <!-- Le HTML5 shim, for IE6-8 support of HTML elements -->
                                   <!--[if lt IE 9]>
                                     <script src="https://html5shim.googlecode.com/svn/trunk/html5.js"></script>
                                   <![endif]-->""")

  def bodyComment = Unparsed( """  <!--[if IE]>
                        <link rel="stylesheet" type="text/css" href="/css/custom-theme/jquery.ui.1.8.16.ie.css"/>
                        <![endif]-->""")
}
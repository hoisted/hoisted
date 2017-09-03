package org.hoisted.lib

import xml.{Unparsed, Elem, Text, NodeSeq}
import net.liftweb._
import common._
import util._
import Helpers._
import http.S
import http.js._
import JsCmds._
import org.joda.time.format.{DateTimeFormat}
import collection.mutable.ListBuffer
import scala.xml
import java.net.URLEncoder


/**
  * Created with IntelliJ IDEA.
  * User: dpp
  * Date: 7/2/12
  * Time: 6:14 PM
  * To change this template use File | Settings | File Templates.
  */

object BaseSnippets extends LazyLoggableWithImplicitLogger {

  def sketchboard(in: NodeSeq): NodeSeq = {
    val acct = S.attr("acct")
    acct.map(a => {
      val url = Helpers.urlEncode(a)
      <a href={"http://sketchboard.me/" + url}
         target="_blank">
        <img
        src={"http://sketchboard.me/img/" + url}></img>
      </a>
    }) openOr in
  }


  def embedBy: NodeSeq => NodeSeq = ns => {
    val (by, pages) = selectAGroup

    val css = S.attr("css") openOr "*"

    ("*" #> pages.collect {
      case h: HasHtml => h
    }.map(p => css #> p.html)).apply(ns)
  }

  def archivedPosts: NodeSeq => NodeSeq = ns => {
    val e2 = env
    val byYear = e2.blogPosts.filter(e2.isValid).groupBy(a => e2.computeDate(a).getYear)

    val months: Array[String] = {
      val locale = DateUtils.CurrentLocale.box openOr java.util.Locale.getDefault
      val fmt = DateTimeFormat.forPattern("MMMM").withLocale(locale)
      val baseDate = (new org.joda.time.DateTime())

      (1 to 12).map(i => fmt.print(baseDate.withMonthOfYear(i))).toArray
    }

    val fmt = env.dateFormatter

    (ClearClearable andThen "@year-block" #> byYear.keys.
      toList.sortWith(_ > _).
      map(year => "@year *" #> year & "@month-block" #> {
        val byMonth = byYear(year).groupBy(a => e2.computeDate(a).getMonthOfYear)
        byMonth.keys.toList.sortWith(_ > _).
          map(month => "@month *" #> months(month - 1) & "@post-block" #> {
            val posts = byMonth(month).sortWith {
              case (a, b) =>
                val d1 = env.computeDate(a)
                val d2 = env.computeDate(b)
                d1.getMillis > d2.getMillis
            }

            posts.map(post =>
              "@post-date *" #> fmt.print(e2.computeDate(post)) &
                "@post-title [data-hoisted-type]" #> "post" &
                "@post-title [data-hoisted-slug]" #> env.computeSlug(post) &
                "@post-title *" #> e2.computeTitle(post) & "@post-title [href]" #> e2.computeLink(post))
          })
      })).apply(ns)
  }

  def env = HoistedEnvironmentManager.value

  def doA: NodeSeq => NodeSeq = ns => {
    (for {
      name <- S.attr("name")
      it <- env.findByTag("name", Full(name)).headOption
    } yield <a href={env.computeLink(it)}>
      {ns}
    </a>) openOr ns
  }

  def doXmenu: NodeSeq => NodeSeq = ns => {
    val activeClass = S.attr("active_class") or S.attr("active-class") openOr "active"
    val locgroup = S.attr("locgroup")

    val menus: List[ParsedFile] = HoistedEnvironmentManager.value.findByTag("menu", Empty).
      filter(p => p.findData(MenuLocGroupKey).flatMap(_.asString) == locgroup)

    val sorted = menus.sortWith((a, b) =>
      (a.findData(OrderKey).flatMap(_.asInt) openOr 0) <
        (b.findData(OrderKey).flatMap(_.asInt) openOr 0)
    )

    def buildLink(p: ParsedFile): NodeSeq = {
      def buildIconLink(ls: List[String]): NodeSeq = {
        def buildIconLeftLink: NodeSeq = <span>
          {icon}{env.computeLinkText(p)}
        </span>

        def buildIconRightLink: NodeSeq = <span>
          {env.computeLinkText(p)}{icon}
        </span>

        def icon = <i class={ls.mkString(" ")}></i>

        p.findData(MenuIconPlacementKey).flatMap(_.asString).map(_.toLowerCase()) match {
          case Full("right") => buildIconRightLink
          case _ => buildIconLeftLink
        }
      }

      def builTextdLink: NodeSeq = Text(env.computeLinkText(p))

      p.findData(MenuIconKey).map(_.forceListString) match {
        case Full(ls) => buildIconLink(ls)
        case _ => builTextdLink
      }
    }

    def buildMenuDivider(p: ParsedFile): NodeSeq = {
      def buildWithMenuDivider(ls: List[String]): NodeSeq = <li class={ls.mkString(" ")}></li>

      p.findData(MenuDividerKey).map(_.forceListString) match {
        case Full(ls) => buildWithMenuDivider(ls)
        case _ => NodeSeq.Empty
      }
    }

    if ((ns \\ "item").filter {
      case e: Elem => e.prefix == "menu"
      case _ => false
    }.isDefined) {
      sorted.flatMap(fr =>
        BindHelpers.bind("menu", ns,
          BindHelpers.TheBindParam("item", buildLink(fr)),
          BindHelpers.TheBindParam("divider", buildMenuDivider(fr)),
          BindHelpers.FuncAttrOptionBindParam("class", (value: NodeSeq) =>
            if (fr eq CurrentFile.value) Some(value) else None, "class"),
          BindHelpers.AttrBindParam("href", env.computeLink(fr), "href"))
      )
    } else {
      ("*" #> sorted.map(fr =>
        "@divider *" #> buildMenuDivider(fr) &
          "a [href]" #> env.computeLink(fr) &
          "a *" #> buildLink(fr) andThen
          (if (fr eq CurrentFile.value)
            ("* [class+]" #> activeClass)
          else
            PassThru))).apply(ns)
    }
  }

  def doTitle(ns: NodeSeq): NodeSeq = <head>
    <title>
      {val x: Box[NodeSeq] =
      for {
        ses <- S.session
      } yield ses.processSurroundAndInclude("Do Title", ns)

    x.openOr(NodeSeq.Empty).text}
    </title>
  </head>

  def doBind: NodeSeq => NodeSeq = ignore => {
    S.attr("name").map(name => <div id={name}></div>) openOr NodeSeq.Empty
  }

  def hTags: NodeSeq => NodeSeq = ns => {
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
        map = md.map
        rd <- map.get(HTagLevelKey).toList.flatMap(_.asInt) if rd <= depth
        body <- map.get(HTagBodyKey).flatMap(_.asNodeSeq)
        id <- map.get(HTagIdKey).flatMap(_.asString)
      } yield
        "data-htag=root [class+]" #> (depthClass + rd) & "a *" #> body & "a [href]" #> ("#" + id) andThen "* [data-htag]" #> (Empty: Box[String]))).apply(ns)
  }

  private[this] def titleFromParsedFile(file: ParsedFile): NodeSeq = {
    file.findData(MetadataKey("title")).flatMap(_.asNodeSeq) or
      file.findData(MetadataKey("sub")).flatMap(_.asNodeSeq) openOr
      Text(file.fileInfo.name)
  }

  def doSubs(in: NodeSeq): NodeSeq = {
    val info = for {
      tpe <- S.attr("type").toList
      max = S.attr("max").flatMap(asInt).filter(_ > 0) openOr 10
      rec <- env.findByTag("sub", Full(tpe)).take(max)
      title = titleFromParsedFile(rec)
      desc = rec.findData(MetadataKey("desc")).flatMap(_.asNodeSeq)
    } yield (rec, title, desc)

    if ((in \\ "title").filter {
      case e: Elem => e.prefix == "sub"
      case _ => false
    }.isDefined) {
      info.flatMap { case (rec, title, desc) =>
        BindHelpers.bind("sub", in,
          BindHelpers.TheBindParam("title", title),
          BindHelpers.BoxBindParam("desc", desc),
          BindHelpers.AttrBindParam("href", Text(rec.findData(RedirectKey).flatMap(_.asString) openOr env.computeLink(rec)), "href"))
      }
    } else {
      ("*" #> info.map {
        case (rec, title, desc) => "@title *" #> title &
          "@desc *" #> desc &
          "a [href]" #> (rec.findData(RedirectKey).flatMap(_.asString) openOr env.computeLink(rec))
      }).apply(in)
    }
  }

  def search: NodeSeq => NodeSeq = ns => {
    val it: Elem = ns.toList.collect { case e: Elem => e }.headOption.getOrElse(<div/>)

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
        rpp: """ + S.attr("rpp").flatMap(Helpers.asInt).openOr(4) +
        """,
        interval: """ + S.attr("interval").flatMap(Helpers.asInt).openOr(30000) +
        """,
        width: 'auto',
        height: """ + S.attr("height").flatMap(Helpers.asInt).openOr(300) +
        """,
        theme: {
          shell: {
            background: """ + S.attr("shell-background").openOr("#bfbfbf").encJs +
        """,
            color: """ + S.attr("shell-color").openOr("#000000").encJs +
        """
          },
          tweets: {
            background: """ + S.attr("tweets-background").openOr("#ffffff").encJs +
        """,
            color: """ + S.attr("tweets-color").openOr("#000000").encJs +
        """,
            links: """ + S.attr("tweets-links").openOr("#292382").encJs +
        """
          }
        },
        features: {
          scrollbar: true,
          loop: false,
          live: true,
          behavior: 'all'
        }
      }).render().setUser(""" + user.encJs +
        """).start();
      // ]]>
      </script>"""

    )
  }

  def selectAGroup: (String, List[ParsedFile]) = {
    val by = (S.attr("by") openOr "post").toLowerCase
    val order = (S.attr("order") openOr "date").toLowerCase

    val descending = (S.attr("descending").flatMap(Helpers.asBoolean(_)) openOr false)

    val filterFunc: ParsedFile => Boolean = by match {
      case "post" => env.isBlogPost
      case "event" => env.isEvent
      case "article" => env.isArticle
      case x => p => TypeKey.test(p, x)
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
        (a, b) =>
          a.findData(k).flatMap(_.asString).map(_.toLowerCase).openOr("") <
            b.findData(k).flatMap(_.asString).map(_.toLowerCase).openOr("")
    }


    val sortFunc: (ParsedFile, ParsedFile) => Boolean =
      if (!descending) sortFunc1 else (a, b) => !sortFunc1(a, b)

    (by, (pages.sortWith(sortFunc), S.attr("max").flatMap(Helpers.asInt)) match {
      case (ret, Full(max)) => ret.take(max)
      case (ret, _) => ret
    })
  }

  def group(env: EnvironmentManager): NodeSeq => NodeSeq = ns => {
    val (by, sorted) = selectAGroup

    val f = env.dateFormatter

    (("data-post=item" #> sorted.map {
      p =>
        val (short, more) = env.computeShortContent(p)

        "data-post=img [src]" #> env.computeLink(p) &
          "data-post=img [alt]" #> env.computeLinkText(p) &
          "data-post=link [href]" #> env.computeLink(p) &
          "data-post=link *" #> env.computeLinkText(p) &
          "data-post=link [data-hoisted-type]" #> by &
          "data-post=link [data-hoisted-slug]" #> env.computeSlug(p) &
          "data-post=shortcontent" #> short &
          (if (more) ("data-post=more" #> ("a [href]" #> env.computeLink(p))) else "data-post=more" #> (Empty: Box[String])) &
          "data-post=content" #> env.computeContent(p) &
          "data-post=date *" #> f.print(env.computeDate(p))
    }) andThen "* [data-post]" #> (Empty: Box[String])).apply(ns)
  }

  def googleAnalytics: NodeSeq => NodeSeq = ns => {
    val xml = (S.attr("id") or S.attr("googleid")) map (id =>
      """<script type="text/javascript">
      // <![CDATA[
  var _gaq = _gaq || [];
  _gaq.push(['_setAccount', """ + id.encJs +
        """]);
  _gaq.push(['_trackPageview']);

  (function() {
    var ga = document.createElement('script'); ga.type = 'text/javascript'; ga.async = true;
    ga.src = ('https:' == document.location.protocol ? 'https://ssl' : 'http://www') + '.google-analytics.com/ga.js';
    var s = document.getElementsByTagName('script')[0]; s.parentNode.insertBefore(ga, s);
  })();
// ]]>
    </script>""")

    xml.map(v => <lift:tail>
      {Unparsed(v)}
    </lift:tail>) openOr NodeSeq.Empty

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

    val lst2 = lst.zip(ids).map {
      case (elem, id) => elem % ("id" -> id) % ("style" -> "display:none")
    }
    val varName = Helpers.nextFuncName

    lst2 ++ Script(JsCmds.Run(varName + " = [" + ids.map(_.encJs).mkString(",") + "];\n" +
      varName + "_func = function() {" +
      """

        for (var i = 0; i < """ + (lst.length * 10) +
      """; i++) {
          var tmp = """ + varName +
      """;
          var len = tmp.length;
          var x = Math.floor(Math.random() * len);
          var y = Math.floor(Math.random() * len);
          var q = tmp[x];
          tmp[x] = tmp[y];
          tmp[y] = q;
        };

        for (var i = 0; i < """ + varName +
      """.length; i++) {
          document.getElementById(""" + varName +
      """[i]).style.display = "none";
        }

        for (var i = 0; i < """ + num +
      """; i++) {
          document.getElementById(""" + varName +
      """[i]).style.display = "";
        };
                                              """ + refresh.map(s => "setTimeout('" + varName + "_func()', " + s + ");").openOr("") +
      """
        };

        """ + varName +
      """_func();
      """))
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
      (("#" + from + " *") #> ((ns: NodeSeq) => {
        ns.toList.foreach {
          case e: Elem => if (elemCnt == 0 && ((e.label.toLowerCase match {
            case "h1" | "h2" | "h3" | "h4" | "h5" | "h6" => true
            case _ => false
          }) || (e.attribute("class").map { s =>
            val s2 = s.text.toLowerCase
            s2.contains("hero-unit") || s2.contains("page-header")
          } getOrElse false))) {
            removePageHeader = e.attribute("class").map { s =>
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
        val q = ("#" + to + " *") #> foundFrom & ("#" + from + " *") #> (otherNodes.toList: NodeSeq)

        if (removePageHeader) {
          q & ("#" + to + " [class]") #> (None: Option[String])
        } else q
      } else {
        ("#" + to) #> foundFrom
      }
    }

    xf.map(_.apply(in)) openOr in
  }

  def pageInfo: NodeSeq => NodeSeq = ns => {
    val name = (S.attr("name") or S.attr("info")).map(_.trim.toLowerCase)

    val nameDateThing: Box[NodeSeq] = name match {
      case Full("title") => Full(Text(env.computeTitle(CurrentFile.value)))
      case Full("date") => Full(Text(env.dateFormatter.print(env.computeDate(CurrentFile.value))))
      case _ => Empty
    }

    val data: Box[NodeSeq] = nameDateThing or name.map(MetadataKey(_)).flatMap(CurrentFile.value.findData(_)).flatMap(data =>
      data.asListString.map(a => Text(a.mkString(", ")): NodeSeq) or
        data.asNodeSeq or data.asString.map(a => Text(a): NodeSeq) or
        data.asDate.map(d => Text(env.dateFormatter.print(d)): NodeSeq))

    ns match {
      case e: Elem => ("* *" #> data).apply(e)
      case x => data openOr NodeSeq.Empty
    }
  }


  def simplyBlogPosts: NodeSeq => NodeSeq = ignore =>
    S.withAttrs("max" -> "15")(blogPosts(
      <ul class="posts" style="list-style: none">
        <li data-post="item">
          <h2>
            <a data-post="link" href="#">Blog Post</a>
          </h2>
          <h4 style="padding-left: 8px;">
            <span data-post="date">2012/12/14</span>
          </h4>
          <div style="padding-left: 15px;" data-post="shortcontent">
            Post Content goes here
          </div>
          <div data-post="more">
            <a href="#">Read More...</a>
          </div>
          <hr/>
        </li>
      </ul>))

  def blogPosts: NodeSeq => NodeSeq = ns => {
    val posts = HoistedEnvironmentManager.value.blogPosts.take(S.attr("max").flatMap(Helpers.asInt _) openOr Integer.MAX_VALUE)

    val m = HoistedEnvironmentManager.value

    val f = env.dateFormatter

    ("data-post=item" #> posts.map {
      p =>
        val (short, more) = m.computeShortContent(p)

        "data-post=link [href]" #> m.computeLink(p) &
          "data-post=link [data-hoisted-type]" #> "post" &
          "data-post=link [data-hoisted-slug]" #> env.computeSlug(p) &
          "data-post=link *" #> m.computeLinkText(p) &
          "data-post=shortcontent" #> short &
          (if (more) ("data-post=more" #> ("a [href]" #> m.computeLink(p))) else "data-post=more" #> (Empty: Box[String])) &
          "data-post=content" #> m.computeContent(p) &
          "data-post=date *" #> f.print(m.computeDate(p))
    } andThen "* [data-post]" #> (Empty: Box[String])).apply(ns)
  }

  def testAttr(in: NodeSeq): NodeSeq = {
    (S.attr("toTest") or S.attr("totest") or S.attr("to_test"), S.attr("extra"), S.attr("extra_true"), S.attr("extra_eq")) match {
      case (Full(toTest), _, _, _) =>
        for {
          attr <- S.attr(toTest).toList if attr == "true"
          node <- in
        } yield node

      case (_, Full(extra), _, _) =>
        val key = MetadataKey(extra.toLowerCase)
        (CurrentFile.value.findData(key) or env.findMetadata(key)).
          map(ignore => in) openOr Nil

      case (_, _, Full(extraInfo), _) =>
        val key = MetadataKey(extraInfo.toLowerCase)
        (CurrentFile.value.findData(key).flatMap(_.asBoolean) or env.findMetadata(key).flatMap(_.asBoolean)).
          filter(v => v).map(ignore => in) openOr Nil

      case (_, _, _, Full(extraEq)) =>
        val key = MetadataKey(extraEq.toLowerCase)
        (CurrentFile.value.findData(key).flatMap(_.forceListString.headOption) or
          env.findMetadata(key).flatMap(_.forceListString.headOption)).
          filter(v => Full(v.trim.toLowerCase) == S.attr("value").map(_.trim.toLowerCase)).map(ignore => in) openOr Nil

      case _ => Nil
    }
  }


  def xform: NodeSeq => NodeSeq = ns => {
    for {
      css <- (ns.collect {
        case e: Elem => e
      }.flatMap(_.attribute("data-css")).headOption.map(_.text): Option[String])

      thing: NodeSeq = (S.attr("kids").
        flatMap(Helpers.asBoolean).
        filter(a => a).map(ignore => ns.collect {
        case e: Elem => e
      }.flatMap(_.child): NodeSeq) openOr
        (("* [data-css]" #> (None: Option[String])).apply(ns)))

      xf <- (HoistedUtil.logFailure("Creating CSS xform for " + css + " with right side " + thing)(css #> thing): Box[NodeSeq => NodeSeq])
    } {
      PostPageTransforms.set(PostPageTransforms.get :+ xf)
    }

    NodeSeq.Empty
  }

  def googleMap: NodeSeq => NodeSeq = ns => {
    (S.attr("address") or S.attr("where") or ns.toList.collect { case e: Elem => e }.headOption.flatMap(_.attribute("data-address").map(_.text))) match {
      case Full(where) =>
        <iframe width={S.attr("width") openOr "640"} height={S.attr("height") openOr "480"} frameborder="0" scrolling="no" marginheight="0" marginwidth="0" src={"http://maps.google.com/maps?q=" + URLEncoder.encode(where, "UTF-8") + "&output=embed"}></iframe>

      case _ =>
        logger.warn("'google.map' snippet invoked, but no 'address' or 'where' parameter specified")
        ns
    }
  }

  def disqusCount: NodeSeq => NodeSeq = ns => {
    S.attr("shortname") match {
      case Full(acct) =>

        val xf = "data-hoisted-type=post" #> ((ns: NodeSeq) =>
          ns ++ <small>
            &nbsp;{<a href={ns.collect { case e: Elem => e }.headOption.flatMap(_.attribute("href").
              map(a => Text(a.text + "#disqus_thread")))}></a>}
          </small>
          )

        PostPageTransforms.set(PostPageTransforms.get :+ xf)


        <tail>
          <script type="text/javascript">
            {Unparsed(
            """
      // <![CDATA[
      /* * * CONFIGURATION VARIABLES: EDIT BEFORE PASTING INTO YOUR WEBPAGE * * */
      var disqus_shortname = """ + acct.encJs +
              """;

      /* * * DON'T EDIT BELOW THIS LINE * * */
      (function () {
      var s = document.createElement('script'); s.async = true;
    s.type = 'text/javascript';
    s.src = 'https://' + disqus_shortname + '.disqus.com/count.js';
    (document.getElementsByTagName('HEAD')[0] || document.getElementsByTagName('BODY')[0]).appendChild(s);
    }());
    // ]]>
    """)}
          </script>
        </tail>

      case _ =>
        logger.warn("'disqus.count' snippet invoked, but no 'shortname' parameter specified")
        ns
    }
  }

  def disqus: NodeSeq => NodeSeq = ns => {
    S.attr("shortname") match {
      case Full(acct) =>
        <xml:group>
          <div id="disqus_thread"></div>
          <script type="text/javascript">
            {Unparsed(
            """

        // <![CDATA[
        /* * * CONFIGURATION VARIABLES: EDIT BEFORE PASTING INTO YOUR WEBPAGE * * */
        var disqus_shortname = """ + acct.encJs +
              """;

        /* * * DON'T EDIT BELOW THIS LINE * * */
        (function() {
        var dsq = document.createElement('script'); dsq.type = 'text/javascript'; dsq.async = true;
    dsq.src = 'https://' + disqus_shortname + '.disqus.com/embed.js';
    (document.getElementsByTagName('head')[0] || document.getElementsByTagName('body')[0]).appendChild(dsq);
    })();
    // ]]>
    """)}
          </script>
          <noscript>Please enable JavaScript to view the
            <a href="http://disqus.com/?ref_noscript">comments powered by Disqus.</a>
          </noscript>
          <a href="http://disqus.com" class="dsq-brlink">comments powered by
            <span class="logo-disqus">Disqus</span>
          </a>
        </xml:group>

      case _ =>
        logger.warn("'disqus' snippet invoked, but no 'shortname' parameter specified")
        ns

    }
  }
}

object BootstrapUtil {
  def headComment = Unparsed(
    """    <!-- Le HTML5 shim, for IE6-8 support of HTML elements -->
                                   <!--[if lt IE 9]>
                                     <script src="https://html5shim.googlecode.com/svn/trunk/html5.js"></script>
                                   <![endif]-->""")

  def bodyComment = Unparsed(
    """  <!--[if IE]>
                        <link rel="stylesheet" type="text/css" href="/css/custom-theme/jquery.ui.1.8.16.ie.css"/>
                        <![endif]-->""")
}

import scala.xml._
import common._


/**
  * This trait is used to identify an object that is representable as a {@link NodeSeq}.
  */
trait Bindable {
  def asHtml: NodeSeq
}

trait AttrHelper[+Holder[X]] {
  type Info

  def apply(key: String): Holder[Info] = convert(findAttr(key))

  def apply(prefix: String, key: String): Holder[Info] =
    convert(findAttr(prefix, key))

  def apply(key: String, default: => Info): Info =
    findAttr(key) getOrElse default

  def apply(prefix: String, key: String, default: => Info): Info =
    findAttr(prefix, key) getOrElse default

  def apply[T](key: String, f: Info => T): Holder[T] =
    convert(findAttr(key).map(f))

  def apply[T](prefix: String, key: String, f: Info => T): Holder[T] =
    convert(findAttr(prefix, key).map(f))

  def apply[T](key: String, f: Info => T, default: => T): T =
    findAttr(key).map(f) getOrElse default

  def apply[T](prefix: String, key: String, f: Info => T, default: => T): T =
    findAttr(prefix, key).map(f) getOrElse default

  protected def findAttr(key: String): Option[Info]

  protected def findAttr(prefix: String, key: String): Option[Info]

  protected def convert[T](in: Option[T]): Holder[T]
}

/**
  * BindHelpers can be used to obtain additional information while a {@link bind} call is executing.
  * This informaiton includes node attributes of the current bound node or the entire NodeSeq that is
  * to be bound. Since the context is created during bind execution and destroyed when bind terminates,
  * you can benefit of these helpers in the context of FuncBindParam or FuncAttrBindParam. You can
  * also provide your own implementation of BindParam and your BindParam#calcValue function will be called
  * in the appropriate context.
  *
  * Example:
  * <pre name="code" class="scala">
  * bind("hello", xml,
  * "someNode" -> {node: NodeSeq => <function-body>})
  * </pre>
  *
  * In <code>function-body</code> you can safely use BindHelpers methods to obtain correctly-scoped information.
  */
object BindHelpers extends BindHelpers {

  private val _bindNodes = new ThreadGlobal[List[NodeSeq]]
  private val _currentNode = new ThreadGlobal[Elem]

  /**
    * A list of NodeSeq that preceeds the NodeSeq passed to bind. The head of the list
    * is the most recent NodeSeq. This returns Empty if it is called outside its context,
    * or Full(Nil) if there are no child nodes but the function is called within the
    * appropriate context.
    */
  def bindNodes: Box[List[NodeSeq]] = _bindNodes.box

  /**
    * A Box containing the current Elem, the children of which are passed to the bindParam
    */
  def currentNode: Box[Elem] = _currentNode.box

  /**
    * Helpers for obtaining attributes of the current Elem
    */
  object attr extends AttrHelper[Option] {
    type Info = NodeSeq

    protected def findAttr(key: String): Option[Info] =
      for {n <- _currentNode.box.toOption
           at <- n.attributes.find(at => at.key == key && !at.isPrefixed)}
        yield at.value

    protected def findAttr(prefix: String, key: String): Option[Info] =
      for {n <- _currentNode.box.toOption
           at <- n.attributes.find {
             case at: PrefixedAttribute => at.key == key && at.pre == prefix
             case _ => false
           }}
        yield at.value

    protected def convert[T](in: Option[T]): Option[T] = in

  }

}

/**
  * Helpers assocated with bindings
  */
trait BindHelpers {
  private lazy val logger = Logger(classOf[BindHelpers])

  def errorDiv(body: NodeSeq): Box[NodeSeq] =
    Props.mode match {
      case Props.RunModes.Development | Props.RunModes.Test =>
        Full(<div class="snippeterror" style="display: block; padding: 4px; margin: 8px; border: 2px solid red">
          {body}<i>note: this error is displayed in the browser because
            your application is running in "development" or "test" mode.If you
            set the system property run.mode=production, this error will not
            be displayed, but there will be errors in the output logs.
          </i>
        </div>)

      case _ => Empty
    }

  /**
    * Adds a css class to the existing class tag of an Elem or create
    * the class attribute
    */
  def addCssClass(cssClass: Box[String], elem: Elem): Elem =
    cssClass match {
      case Full(css) => addCssClass(css, elem)
      case _ => elem
    }

  /**
    * Adds a css class to the existing class tag of an Elem or create
    * the class attribute
    */
  def addCssClass(cssClass: String, elem: Elem): Elem = {
    elem.attribute("class") match {
      case Some(clz) => {
        def fix(in: MetaData) =
          new UnprefixedAttribute("class", clz.text.trim + " " + cssClass.trim,
            in.filter {
              case p: UnprefixedAttribute =>
                p.key != "class"
              case _ => true
            })

        new Elem(elem.prefix,
          elem.label,
          fix(elem.attributes),
          elem.scope,
          false,
          elem.child: _*)
      }
      case _ => elem % new UnprefixedAttribute("class", cssClass, Null)
    }
  }

  /**
    * Takes attributes from the first node of 'in' (if any) and mixes
    * them into 'out'. Curried form can be used to produce a
    * NodeSeq => NodeSeq for bind.
    *
    * @param in  where to take the attributes from
    * @param out where to put the attributes
    * @return 'out' element with attributes from 'in'
    */
  def mixinAttributes(out: Elem)(in: NodeSeq): NodeSeq = {
    val attributes = in.headOption.map(_.attributes).getOrElse(Null)
    out % attributes
  }

  /**
    * Finds and returns one of many templates from the children based
    * upon the namespace and tag name: for example, for prefix "choose"
    * and tag name "stuff" this would return the contents of the
    * first tag <code>&lt;choose:stuff&gt; ... &lt;/choose:stuff&gt;</code>
    * in the specified NodeSeq.
    *
    * @param prefix the prefix (e.g., "choose")
    * @param tag    the tag to choose (e.g., "stuff")
    * @param xhtml  the node sequence to search for the specified element
    * @return the first matching node sequence
    */
  def chooseTemplate(prefix: String, tag: String, xhtml: NodeSeq): NodeSeq =
    Helpers.findElems(xhtml)(e => e.label == tag && e.prefix == prefix).toList match {
      case Nil => NodeSeq.Empty
      case x :: xs => x.child
    }

  /**
    * Similar to chooseTemplate, this returns the contents of the element in a Full Box if
    * found or an Empty Box otherwise.
    */
  def template(xhtml: NodeSeq, prefix: String, tag: String): Box[NodeSeq] =
    Helpers.findElems(xhtml)(e => e.label == tag && e.prefix == prefix).toList match {
      case Nil => Empty
      case x :: xs => Full(x.child)
    }

  /**
    * Find two of many templates from the children
    */
  def template(xhtml: NodeSeq, prefix: String, tag1: String,
               tag2: String): Box[(NodeSeq, NodeSeq)] =
    for (x1 <- template(xhtml, prefix, tag1);
         x2 <- template(xhtml, prefix, tag2)) yield (x1, x2)

  /**
    * Find three of many templates from the children
    */
  def template(xhtml: NodeSeq, prefix: String, tag1: String,
               tag2: String, tag3: String): Box[(NodeSeq, NodeSeq, NodeSeq)] =
    for (x1 <- template(xhtml, prefix, tag1);
         x2 <- template(xhtml, prefix, tag2);
         x3 <- template(xhtml, prefix, tag3)) yield (x1, x2, x3)

  /**
    * Base class for Bind parameters. A bind parameter has a name and is able to extract its value from a NodeSeq.
    */
  sealed trait BindParam {
    def name: String

    def calcValue(in: NodeSeq): Option[NodeSeq]
  }

  /**
    * A trait that indicates what the newly bound attribute name should be.
    */
  trait BindWithAttr {
    def newAttr: String
  }

  /**
    * A case class that wraps attribute-oriented BindParams to allow prefixing the resulting attribute
    */
  sealed case class PrefixedBindWithAttr(prefix: String, binding: BindParam with BindWithAttr) extends BindParam with BindWithAttr {
    val name = binding.name

    def calcValue(in: NodeSeq) = binding.calcValue(in)

    val newAttr = binding.newAttr
  }

  /**
    * Constant BindParam always returning the same value
    */
  final class TheBindParam(val name: String, value: NodeSeq)
    extends BindParam {
    def calcValue(in: NodeSeq): Option[NodeSeq] = Some(value)
  }

  object TheBindParam {
    def apply(name: String, value: NodeSeq) = new TheBindParam(name, value)
  }

  /**
    * Constant BindParam always returning the same value
    */
  final class TheStrBindParam(val name: String, value: String)
    extends BindParam {
    def calcValue(in: NodeSeq): Option[NodeSeq] = value match {
      case null => Some(NodeSeq.Empty)
      case str => Some(Text(str))
    }
  }

  object TheStrBindParam {
    def apply(name: String, value: String) = new TheStrBindParam(name, value)
  }

  /**
    * BindParam that binds a given value into a new attribute.
    * For example, given the following markup:
    *
    * <pre name="code" class="xml">
    * &lt;lift:AttrBinds &gt;
    * &lt;div test:w="foo" /&gt;
    * &lt;div test:x="foo" /&gt;
    * &lt;div test:y="foo" /&gt;
    * &lt;div test:z="foo" /&gt;
    * &lt;/lift:AttrBinds &gt;
    * </pre>
    *
    * The following snippet:
    *
    * <pre name="code" class="scala">
    * import scala.xml._
    * class AttrBinds {
    * def render(xhtml : NodeSeq) : NodeSeq =
    *     BindHelpers.bind("test", xhtml,
    * AttrBindParam("w", Text("fooW"), "id"),
    * AttrBindParam("x", "fooX", "id"),
    * AttrBindParam("y", Text("fooW"), ("lift","calcId")),
    * AttrBindParam("z", "fooZ", ("lift", "calcId")))
    * </pre>
    *
    * produces this markup:
    *
    * <pre name="code" class="xml">
    * &lt;div id="fooW" /&gt;
    * &lt;div id="fooX" /&gt;
    * &lt;div lift:calcId="fooY" /&gt;
    * &lt;div lift:calcId="fooZ" /&gt;
    * </pre>
    *
    * @param name    the name of the binding to replace
    * @param myValue the value of the new attribute
    * @param newAttr The new attribute label
    */
  final class AttrBindParam(val name: String, myValue: => NodeSeq, val newAttr: String)
    extends BindParam with BindWithAttr {
    def calcValue(in: NodeSeq): Option[NodeSeq] = Some(myValue)
  }


  /**
    * BindParam that binds a given value into a new attribute.
    *
    * This object provides factory methods for convenience.
    */
  object AttrBindParam {
    /**
      * Returns an unprefixed attribute binding containing the specified NodeSeq
      *
      * @param name    The name to bind against
      * @param myValue The value to place in the new attribute
      * @param newAttr The new attribute label
      */
    def apply(name: String, myValue: => NodeSeq, newAttr: String) =
      new AttrBindParam(name, myValue, newAttr)

    /**
      * Returns an unprefixed attribute binding containing the specified String
      * wrapped in a Text() element
      *
      * @param name    The name to bind against
      * @param myValue The value to place in the new attribute
      * @param newAttr The new attribute label
      */
    def apply(name: String, myValue: String, newAttr: String) =
      new AttrBindParam(name, if (null eq myValue) NodeSeq.Empty else Text(myValue), newAttr)

    /**
      * Returns a prefixed attribute binding containing the specified NodeSeq
      *
      * @param name    The name to bind against
      * @param myValue The value to place in the new attribute
      * @param newAttr The new attribute in the form (prefix,label)
      */
    def apply(name: String, myValue: => NodeSeq, newAttr: (String, String)) =
      PrefixedBindWithAttr(newAttr._1, new AttrBindParam(name, myValue, newAttr._2))

    /**
      * Returns a prefixed attribute binding containing the specified String
      * wrapped in a Text() element
      *
      * @param name    The name to bind against
      * @param myValue The value to place in the new attribute
      * @param newAttr The new attribute in the form (prefix,label)
      */
    def apply(name: String, myValue: String, newAttr: (String, String)) =
      PrefixedBindWithAttr(newAttr._1, new AttrBindParam(name,
        if (null eq myValue) NodeSeq.Empty else Text(myValue), newAttr._2))
  }

  /**
    * BindParam using a function to calculate its value
    */
  final class FuncBindParam(val name: String, value: NodeSeq => NodeSeq)
    extends BindParam {
    def calcValue(in: NodeSeq): Option[NodeSeq] = Some(value(in))
  }

  object FuncBindParam {
    def apply(name: String, value: NodeSeq => NodeSeq) = new FuncBindParam(name, value)
  }

  /**
    * BindParam that computes a new attribute value based on the current
    * attribute value. For example, given the following markup:
    *
    * <pre name="code" class="xml">
    * &lt;lift:AttrBinds &gt;
    * &lt;div test:x="foo" /&gt;
    * &lt;div test:y="foo" /&gt;
    * &lt;/lift:AttrBinds &gt;
    * </pre>
    *
    * The following snippet:
    *
    * <pre name="code" class="scala">
    * import scala.xml._
    * class AttrBinds {
    * def render(xhtml : NodeSeq) : NodeSeq =
    *     BindHelpers.bind("test", xhtml,
    * FuncAttrBindParam("x", { ns : NodeSeq => Text(ns.text.toUpperCase + "X")}, "id"),
    * FuncAttrBindParam("y", { ns : NodeSeq => Text(ns.text.length + "Y")}, ("lift","calcId")))
    * </pre>
    *
    * produces this markup:
    *
    * <pre name="code" class="xml">
    * &lt;div id="FOOX" /&gt;
    * &lt;div lift:calcId="3Y" /&gt;
    * </pre>
    *
    * @param name    The name to bind against
    * @param value   A function that takes the current attribute's value and computes
    *                the new attribute value
    * @param newAttr The new attribute label
    */
  final class FuncAttrBindParam(val name: String, value: => NodeSeq => NodeSeq, val newAttr: String)
    extends BindParam with BindWithAttr {
    def calcValue(in: NodeSeq): Option[NodeSeq] = Some(value(in))
  }

  /**
    * BindParam using a function to calculate its value.
    *
    * This object provides factory methods for convenience.
    *
    */
  object FuncAttrBindParam {
    /**
      * Returns an unprefixed attribute binding computed by the provided function
      *
      * @param name    The name to bind against
      * @param value   The function that will transform the original attribute value
      *                into the new attribute value
      * @param newAttr The new attribute label
      */
    def apply(name: String, value: => NodeSeq => NodeSeq, newAttr: String) = new FuncAttrBindParam(name, value, newAttr)

    /**
      * Returns a prefixed attribute binding computed by the provided function
      *
      * @param name    The name to bind against
      * @param value   The function that will transform the original attribute value
      *                into the new attribute value
      * @param newAttr The new attribute name in the form (prefix,label)
      */
    def apply(name: String, value: => NodeSeq => NodeSeq, newAttr: (String, String)) =
      PrefixedBindWithAttr(newAttr._1, new FuncAttrBindParam(name, value, newAttr._2))
  }

  final class OptionBindParam(val name: String, value: Option[NodeSeq])
    extends BindParam {
    def calcValue(in: NodeSeq): Option[NodeSeq] = value
  }

  object OptionBindParam {
    def apply(name: String, value: Option[NodeSeq]) = new OptionBindParam(name, value)
  }

  final class BoxBindParam(val name: String, value: Box[NodeSeq])
    extends BindParam {
    def calcValue(in: NodeSeq): Option[NodeSeq] = value
  }

  object BoxBindParam {
    def apply(name: String, value: Box[NodeSeq]) = new BoxBindParam(name, value)
  }

  /**
    * BindParam that computes an optional new attribute value based on
    * the current attribute value. Returning None in the transform function
    * will result in the Attribute being omitted. For example, given the
    * following markup:
    *
    * <pre name="code" class="xml">
    * &lt;lift:AttrBinds>
    * &lt;div test:x="foo">
    * &lt;div test:y="foo">
    * &lt;/lift:AttrBinds>
    * </pre>
    *
    * The following snippet:
    *
    * <pre name="code" class="scala">
    * import scala.xml._
    * class AttrBinds {
    * def render(xhtml : NodeSeq) : NodeSeq =
    *BindHelpers.bind("test", xhtml,
    * FuncAttrOptionBindParam("x", { ns : NodeSeq =>
    * Some(Text(ns.text.toUpperCase + "X"))
    * }, ("lift","calcId")),
    * FuncAttrOptionBindParam("y", { ns : NodeSeq =>
    * if (ns.text.length > 10) {
    * Some(Text(ns.text.length + "Y"))
    * } else {
    * None
    * }, ("lift","calcId")))
    * </pre>
    *
    * produces this markup:
    *
    * <pre name="code" class="xml">
    * &lt;div lift:calcId="FOOX" />
    * &lt;div />
    * </pre>
    *
    * @param name    The name to bind against
    * @param value   The function that will transform the original attribute value
    *                into the new attribute value. Returning None will cause this attribute to
    *                be omitted.
    * @param newAttr The new attribute label
    *
    */
  final class FuncAttrOptionBindParam(val name: String, func: => NodeSeq => Option[NodeSeq], val newAttr: String)
    extends BindParam with BindWithAttr {
    def calcValue(in: NodeSeq): Option[NodeSeq] = func(in)
  }

  /**
    * BindParam that computes an optional new attribute value based on
    * the current attribute value. Returning None in the transform function
    * will result in the Attribute not being bound.
    *
    * This object provides factory methods for convenience.
    */
  object FuncAttrOptionBindParam {
    /**
      * Returns an unprefixed attribute binding computed by the provided function
      *
      * @param name    The name to bind against
      * @param value   The function that will transform the original attribute value
      *                into the new attribute value. Returning None will cause this attribute to
      *                be omitted.
      * @param newAttr The new attribute label
      */
    def apply(name: String, func: => NodeSeq => Option[NodeSeq], newAttr: String) =
      new FuncAttrOptionBindParam(name, func, newAttr)


    /**
      * Returns a prefixed attribute binding computed by the provided function
      *
      * @param name    The name to bind against
      * @param value   The function that will transform the original attribute value
      *                into the new attribute value. Returning None will cause this attribute to
      *                be omitted.
      * @param newAttr The new attribute name in the form (prefix,label)
      */
    def apply(name: String, func: => NodeSeq => Option[NodeSeq], newAttr: (String, String)) =
      PrefixedBindWithAttr(newAttr._1, new FuncAttrOptionBindParam(name, func, newAttr._2))
  }

  /**
    * BindParam that computes an optional new attribute value based on
    * the current attribute value. Returning Empty in the transform function
    * will result in the Attribute being omitted. For example, given the
    * following markup:
    *
    * <pre name="code" class="xml">
    * &lt;lift:AttrBinds>
    * &lt;div test:x="foo">
    * &lt;div test:y="foo">
    * &lt;/lift:AttrBinds>
    * </pre>
    *
    * The following snippet:
    *
    * <pre name="code" class="scala">
    * import scala.xml._
    * class AttrBinds {
    * def render(xhtml : NodeSeq) : NodeSeq =
    *BindHelpers.bind("test", xhtml,
    * FuncAttrBoxBindParam("x", { ns : NodeSeq => Full(Text(ns.text.toUpperCase + "X"))}, ("lift","calcId")),
    * FuncAttrBoxBindParam("y", { ns : NodeSeq =>
    * if (ns.text.length > 10) {
    * Full(Text(ns.text.length + "Y"))
    * } else {
    * Empty
    * }, ("lift","calcId")))
    * </pre>
    *
    * produces this markup:
    *
    * <pre name="code" class="xml">
    * &lt;div lift:calcId="FOOX" />
    * &lt;div />
    * </pre>
    *
    * @param name    The name to bind against
    * @param value   The function that will transform the original attribute value
    *                into the new attribute value. Returning Empty will cause this attribute to
    *                be omitted.
    * @param newAttr The new attribute label
    *
    */
  final class FuncAttrBoxBindParam(val name: String, func: => NodeSeq => Box[NodeSeq], val newAttr: String)
    extends BindParam with BindWithAttr {
    def calcValue(in: NodeSeq): Option[NodeSeq] = func(in)
  }

  /**
    * BindParam that computes an optional new attribute value based on
    * the current attribute value. Returning Empty in the transform function
    * will result in the Attribute being omitted.
    *
    * This object provides factory methods for convenience.
    */
  object FuncAttrBoxBindParam {
    /**
      * Returns an unprefixed attribute binding computed by the provided function
      *
      * @param name    The name to bind against
      * @param value   The function that will transform the original attribute value
      *                into the new attribute value. Returning Empty will cause this attribute to
      *                be omitted.
      * @param newAttr The new attribute label
      */
    def apply(name: String, func: => NodeSeq => Box[NodeSeq], newAttr: String) =
      new FuncAttrBoxBindParam(name, func, newAttr)

    /**
      * Returns a prefixed attribute binding computed by the provided function
      *
      * @param name    The name to bind against
      * @param value   The function that will transform the original attribute value
      *                into the new attribute value. Returning Empty will cause this attribute to
      *                be omitted.
      * @param newAttr The new attribute name in the form (prefix,label)
      */
    def apply(name: String, func: => NodeSeq => Box[NodeSeq], newAttr: (String, String)) =
      PrefixedBindWithAttr(newAttr._1, new FuncAttrBoxBindParam(name, func, newAttr._2))
  }

  final class SymbolBindParam(val name: String, value: Symbol)
    extends BindParam {
    def calcValue(in: NodeSeq): Option[NodeSeq] = Some(if (null eq value.name) NodeSeq.Empty else Text(value.name))
  }

  object SymbolBindParam {
    def apply(name: String, value: Symbol) = new SymbolBindParam(name, value)
  }

  final class IntBindParam(val name: String, value: Int)
    extends BindParam {
    def calcValue(in: NodeSeq): Option[NodeSeq] = Some(value.toString match {
      case null => NodeSeq.Empty
      case str => Text(str)
    })
  }

  object IntBindParam {
    def apply(name: String, value: Int) = new IntBindParam(name, value)
  }

  final class LongBindParam(val name: String, value: Long)
    extends BindParam {
    def calcValue(in: NodeSeq): Option[NodeSeq] = Some(Text(value.toString))
  }

  object LongBindParam {
    def apply(name: String, value: Long) = new LongBindParam(name, value)
  }

  final class BooleanBindParam(val name: String, value: Boolean)
    extends BindParam {
    def calcValue(in: NodeSeq): Option[NodeSeq] = Some(Text(value.toString))
  }

  object BooleanBindParam {
    def apply(name: String, value: Boolean) = new BooleanBindParam(name, value)
  }

  final class TheBindableBindParam[T <: Bindable](val name: String, value: T)
    extends BindParam {
    def calcValue(in: NodeSeq): Option[NodeSeq] = Some(value.asHtml)
  }

  object TheBindableBindParam {
    def apply[T <: Bindable](name: String, value: T) = new TheBindableBindParam(name, value)
  }

  /**
    * Remove all the <head> tags, just leaving the child tags
    */
  def stripHead(in: NodeSeq): NodeSeq = {
    ("head" #> ((ns: NodeSeq) => ns.asInstanceOf[Elem].child)).apply(in)
  }

  /**
    * Replace the element with the id that matches with the replacement
    * nodeseq.
    */
  def replaceIdNode(in: NodeSeq, id: String, replacement: NodeSeq): NodeSeq = {
    (("#" + id) #> replacement).apply(in)
  }

  /**
    * transforms a Box into a Text node
    */
  @deprecated("use -> instead", "2.4")
  object BindParamAssoc {
    implicit def canStrBoxNodeSeq(in: Box[Any]): Box[NodeSeq] = in.map(_ match {
      case null => Text("null")
      case v => v.toString match {
        case null => NodeSeq.Empty
        case str => Text(str)
      }
    })
  }

  /**
    * takes a NodeSeq and applies all the attributes to all the Elems at the top level of the
    * NodeSeq.  The id attribute is applied to the first-found Elem only
    */
  def addAttributes(in: NodeSeq, attributes: MetaData): NodeSeq = {
    if (attributes == Null) in
    else {
      val noId = attributes.filter(_.key != "id")
      var doneId = false
      in map {
        case e: Elem =>
          if (doneId) e % noId else {
            doneId = true
            e % attributes
          }
        case x => x
      }
    }
  }

  private def snToNs(in: Seq[Node]): NodeSeq = in

  class SuperArrowAssoc(name: String) {
    // Because JsObj is a subclass of Node, we don't want it
    // getting caught because it's not a bind param
    def ->[T <: SpecialNode](in: T with SpecialNode) = Tuple2[String, T](name, in)

    def ->(in: String) = TheStrBindParam(name, in)

    def ->(in: NodeSeq) = TheBindParam(name, in)

    def ->(in: Text) = TheBindParam(name, in)

    def ->(in: Node) = TheBindParam(name, in)

    def ->(in: Seq[Node]) = TheBindParam(name, in)

    def ->(in: NodeSeq => NodeSeq) = FuncBindParam(name, in)

    def ->(in: Box[NodeSeq]) = BoxBindParam(name, in)

    def ->(in: Option[NodeSeq]) = OptionBindParam(name, in)

    def ->(in: Symbol) = SymbolBindParam(name, in)

    def ->(in: Int) = IntBindParam(name, in)

    def ->(in: Long) = LongBindParam(name, in)

    def ->(in: Boolean) = BooleanBindParam(name, in)

    def ->[T <: Bindable](in: T with Bindable) = TheBindableBindParam[T](name, in)

    def ->[T](in: T) = Tuple2[String, T](name, in)

    def -%>(in: NodeSeq) = FuncBindParam(name, old => addAttributes(in, (BindHelpers.currentNode.map(_.attributes) openOr Null)))

    def -%>(in: Box[NodeSeq]) = FuncBindParam(name,
      old => in.map(a => addAttributes(a,
        (BindHelpers.currentNode.map(_.attributes) openOr Null))) openOr
        NodeSeq.Empty)

    def -%>(in: Option[NodeSeq]) = FuncBindParam(name, old => in.map(a => addAttributes(a,
      (BindHelpers.currentNode.map(_.attributes) openOr
        Null))) getOrElse NodeSeq.Empty)

    def -%>(in: NodeSeq => NodeSeq) = FuncBindParam(name, old => addAttributes(in(old),
      (BindHelpers.currentNode.map(_.attributes) openOr Null)))

    def _id_>(in: Elem) = FuncBindParam(name, _ => in % new UnprefixedAttribute("id", name, Null))

    def _id_>(in: Box[Elem]) = FuncBindParam(name, _ => in.map(_ % new UnprefixedAttribute("id", name, Null)) openOr NodeSeq.Empty)

    def _id_>(in: Option[Elem]) = FuncBindParam(name, _ => in.map(_ % new UnprefixedAttribute("id", name, Null)) getOrElse NodeSeq.Empty)

    def _id_>(in: NodeSeq => Elem) = FuncBindParam(name, kids => in(kids) % new UnprefixedAttribute("id", name, Null))

  }

  implicit def strToSuperArrowAssoc(in: String): SuperArrowAssoc = new SuperArrowAssoc(in)


  /**
    * This class creates a BindParam from an input value
    *
    * @deprecated use -> instead
    */
  @deprecated("use -> instead", "2.4")
  class BindParamAssoc(val name: String) {
    def -->(value: String): BindParam = TheBindParam(name, if (null eq value) NodeSeq.Empty else Text(value))

    def -->(value: NodeSeq): BindParam = TheBindParam(name, value)

    def -->(value: Symbol): BindParam = TheBindParam(name, Text(value.name))

    def -->(value: Any): BindParam = TheBindParam(name, Text(value match {
      case null => "null"
      case v => v.toString match {
        case null => ""
        case str => str
      }
    }))

    def -->(func: NodeSeq => NodeSeq): BindParam = FuncBindParam(name, func)

    def -->(value: Box[NodeSeq]): BindParam = TheBindParam(name, value.openOr(Text("Empty")))
  }

  /**
    * transforms a String to a BindParamAssoc object which can be associated to a BindParam object
    * using the --> operator.<p/>
    * Usage: <code>"David" --> "name"</code>
    *
    * @deprecated use -> instead
    */
  @deprecated("use -> instead", "2.4")
  implicit def strToBPAssoc(in: String): BindParamAssoc = new BindParamAssoc(in)

  /**
    * transforms a Symbol to a SuperArrowAssoc object which can be associated to a BindParam object
    * using the -> operator.<p/>
    * Usage: <code>'David -> "name"</code>
    *
    * @deprecated use -> instead
    */
  @deprecated("use -> instead", "2.4")
  implicit def symToSAAssoc(in: Symbol): SuperArrowAssoc = new SuperArrowAssoc(in.name)

  /**
    * Experimental extension to bind which passes in an additional "parameter" from the XHTML to the transform
    * function, which can be used to format the returned NodeSeq.
    *
    * @deprecated use bind instead
    */
  @deprecated("use bind instead", "2.4")
  def xbind(namespace: String, xml: NodeSeq)(transform: PartialFunction[String, NodeSeq => NodeSeq]): NodeSeq = {
    def rec_xbind(xml: NodeSeq): NodeSeq = {
      xml.flatMap {
        node =>
          node match {
            case s: Elem if (node.prefix == namespace) =>
              if (transform.isDefinedAt(node.label))
                transform(node.label)(node)
              else
                Text("FIX" + "ME failed to bind <" + namespace + ":" + node.label + " />")
            case Group(nodes) => Group(rec_xbind(nodes))
            case s: Elem => Elem(node.prefix, node.label, node.attributes, node.scope, false, rec_xbind(node.child): _*)
            case n => node
          }
      }
    }

    rec_xbind(xml)
  }

  /**
    * Bind a set of values to parameters and attributes in a block of XML.<p/>
    *
    * For example: <pre name="code" class="scala">
    * bind("user", <user:hello>replace this</user:hello>, "hello" -> <h1/>)
    * </pre>
    * will return <pre><h1></h1></pre>
    *
    * @param namespace the namespace of tags to bind
    * @param xml       the NodeSeq in which to find elements to be bound.
    * @param params    the list of BindParam bindings to be applied
    * @return the NodeSeq that results from the specified transforms
    */
  def bind(namespace: String, xml: NodeSeq, params: BindParam*): NodeSeq =
    bind(namespace, Empty, Empty, xml, params: _*)


  /**
    * Bind a set of values to parameters and attributes in a block of XML
    * with defined transforms for unbound elements within the specified
    * namespace.<p/>
    *
    * For example:<pre name="code" class="scala">
    * bind("user",
    * Full(xhtml: NodeSeq => Text("Default Value")),
    * Empty,
    * <user:hello>replace this</user:hello><user:dflt>replace with default</user:dflt>,
    * "hello" -> <h1/>)
    * </pre>
    * will return <pre><h1></h1>Default Value</pre>
    *
    * @param namespace         the namespace of tags to bind
    * @param nodeFailureXform  a box containing the function to use as the default transform
    *                          for tags in the specified namespace that do not have bindings specified.
    * @param paramFailureXform a box containing the function to use as the default transform
    *                          for unrecognized attributes in bound elements.
    * @param xml               the NodeSeq in which to find elements to be bound.
    * @param params            the list of BindParam bindings to be applied
    * @return the NodeSeq that results from the specified transforms
    */
  def bind(namespace: String, nodeFailureXform: Box[NodeSeq => NodeSeq],
           paramFailureXform: Box[PrefixedAttribute => MetaData],
           xml: NodeSeq, params: BindParam*): NodeSeq =
    bind(namespace, nodeFailureXform, paramFailureXform, false, xml, params: _*)

  /**
    * Bind a set of values to parameters and attributes in a block of XML
    * with defined transforms for unbound elements within the specified
    * namespace.<p/>
    *
    * For example:<pre name="code" class="scala">
    * bind("user",
    * Full(xhtml: NodeSeq => Text("Default Value")),
    * Empty,
    * <user:hello>replace this</user:hello><user:dflt>replace with default</user:dflt>,
    * "hello" -> <h1/>)
    * </pre>
    * will return <pre><h1></h1>Default Value</pre>
    *
    * @param namespace         the namespace of tags to bind
    * @param nodeFailureXform  a box containing the function to use as the default transform
    *                          for tags in the specified namespace that do not have bindings specified.
    * @param paramFailureXform a box containing the function to use as the default transform
    *                          for unrecognized attributes in bound elements.
    * @param preserveScope     : true if the scope should be preserved, false is the normal setting
    * @param xml               the NodeSeq in which to find elements to be bound.
    * @param params            the list of BindParam bindings to be applied
    * @return the NodeSeq that results from the specified transforms
    */
  def bind(namespace: String, nodeFailureXform: Box[NodeSeq => NodeSeq],
           paramFailureXform: Box[PrefixedAttribute => MetaData],
           preserveScope: Boolean,
           xml: NodeSeq, params: BindParam*): NodeSeq = {
    val nsColon = namespace + ":"

    BindHelpers._bindNodes.doWith(xml :: (BindHelpers._bindNodes.box.openOr(Nil))) {
      val map: scala.collection.immutable.Map[String, BindParam] = scala.collection.immutable.HashMap.empty ++ params.map(p => (p.name, p))

      def attrBind(attr: MetaData): MetaData = attr match {
        case Null => Null
        case upa: UnprefixedAttribute => new UnprefixedAttribute(upa.key, upa.value, attrBind(upa.next))
        case pa: PrefixedAttribute if pa.pre == namespace => map.get(pa.key) match {
          case None => paramFailureXform.map(_ (pa)) openOr new PrefixedAttribute(pa.pre, pa.key, Text("FIX" + "ME find to bind attribute"), attrBind(pa.next))
          case Some(PrefixedBindWithAttr(prefix, binding)) => binding.calcValue(pa.value).map(v => new PrefixedAttribute(prefix, binding.newAttr, v, attrBind(pa.next))) getOrElse attrBind(pa.next)
          case Some(abp: BindWithAttr) => abp.calcValue(pa.value).map(v => new UnprefixedAttribute(abp.newAttr, v, attrBind(pa.next))) getOrElse attrBind(pa.next)
          case Some(bp: BindParam) => bp.calcValue(pa.value).map(v => new PrefixedAttribute(pa.pre, pa.key, v, attrBind(pa.next))) getOrElse attrBind(pa.next)
        }
        case pa: PrefixedAttribute => new PrefixedAttribute(pa.pre, pa.key, pa.value, attrBind(pa.next))
      }

      def in_bind(xml: NodeSeq): NodeSeq = {
        xml.flatMap {
          case BoundAttr(e, av) if av.startsWith(nsColon) => {
            val fixedAttrs = e.attributes.filter {
              case up: UnprefixedAttribute => true
              case pa: PrefixedAttribute => {
                val res = !(pa.pre == "lift" && pa.key == "bind")
                res
              }
              case _ => true
            }

            val fixedLabel = av.substring(nsColon.length)

            val fake = new Elem(namespace, fixedLabel, fixedAttrs,
              e.scope, false,
              new Elem(e.namespace,
                e.label,
                fixedAttrs,
                e.scope,
                false,
                e.child: _*))

            BindHelpers._currentNode.doWith(fake) {
              map.get(fake.label) match {
                case None =>
                  nodeFailureXform.map(_ (fake)) openOr fake

                case Some(ns) =>
                  ns.calcValue(fake.child) getOrElse NodeSeq.Empty
              }
            }
          }

          case s: Elem if s.prefix == namespace => BindHelpers._currentNode.doWith(s) {
            map.get(s.label) match {
              case None =>
                nodeFailureXform.map(_ (s)) openOr s

              case Some(ns) =>
                //val toRet = ns.calcValue(s.child)
                //mergeBindAttrs(toRet, namespace, s.attributes)
                ns.calcValue(s.child) getOrElse NodeSeq.Empty
            }
          }

          case s: Elem if bindByNameType(s.label) && (attrStr(s, "name").startsWith(namespace + ":")) &&
            bindByNameTag(namespace, s) != "" => BindHelpers._currentNode.doWith(s) {
            val tag = bindByNameTag(namespace, s)
            map.get(tag) match {
              case None => nodeFailureXform.map(_ (s)) openOr s
              case Some(bindParam) => bindByNameMixIn(bindParam, s)
            }
          }
          case Group(nodes) => Group(in_bind(nodes))
          case s: Elem => Elem(s.prefix, s.label, attrBind(s.attributes), if (preserveScope) s.scope else TopScope,
            false,
            in_bind(s.child): _*)
          case n => n
        }
      }

      in_bind(xml)
    }
  }

  private object BoundAttr {
    def unapply(in: Node): Option[(Elem, String)] = {
      in match {
        case e: Elem => {
          val bound = e.attributes.filter {
            case up: UnprefixedAttribute => false
            case pa: PrefixedAttribute =>
              pa.pre == "lift" && pa.key == "bind" && (null ne pa.value)
            case _ => false
          }

          bound.iterator.toList match {
            case xs :: _ => Some(e -> xs.value.text)
            case _ => None
          }
        }

        case _ => None
      }
    }
  }

  private def setElemId(in: NodeSeq, attr: String, value: Seq[Node]): NodeSeq =
    in.map {
      case e: Elem => e % new UnprefixedAttribute(attr, value, Null)
      case v => v
    }

  /**
    * Replace the content of lift:bind nodes with the corresponding nodes found in a map,
    * according to the value of the "name" attribute.<p/>
    * Usage: <pre name="code" class="scala">
    * bind(Map("a" -> <h1/>), <b><lift:bind name="a">change this</lift:bind></b>) must ==/(<b><h1></h1></b>)
    * </pre>
    *
    * @param vals map of name/nodes to replace
    * @param xml  nodes containing lift:bind nodes
    * @return the NodeSeq that results from the specified transforms
    */
  def bind(vals: Map[String, NodeSeq], xml: NodeSeq): NodeSeq =
    bind(vals, xml, true, scala.collection.mutable.Set(vals.keySet.toSeq: _*))

  /**
    * This method exists so that we can do recursive binding with only root-node
    * error reporting.
    *
    * Replace the content of lift:bind nodes with the corresponding nodes found in a map,
    * according to the value of the "name" attribute.<p/>
    * Usage: <pre name="code" class="scala">
    * bind(Map("a" -> <h1/>), <b><lift:bind name="a">change this</lift:bind></b>) must ==/(<b><h1></h1></b>)
    * </pre>
    *
    * @param vals           map of name/nodes to replace
    * @param xml            nodes containing lift:bind nodes
    * @param reportUnused   If true, report unused binding vals to the log
    * @param unusedBindings The set of unused binding values. Mutable for expediency, but it would be
    *                       nice to figure out a cleaner way
    * @return the NodeSeq that results from the specified transforms
    */
  private def bind(vals: Map[String, NodeSeq], xml: NodeSeq, reportUnused: Boolean, unusedBindings: scala.collection.mutable.Set[String]): NodeSeq = {
    val isBind = (node: Elem) => {
      node.prefix == "lift" && node.label == "bind"
    }

    val bindResult = xml.flatMap {
      node =>
        node match {
          case s: Elem if (isBind(s)) => {
            node.attributes.get("name") match {
              case None => {
                if (Props.devMode) {
                  logger.warn("<lift:bind> tag encountered without name attribute!")
                }
                bind(vals, node.child, false, unusedBindings)
              }
              case Some(ns) => {
                vals.get(ns.text) match {
                  case None => {
                    if (Props.devMode) {
                      logger.warn("No binding values match the <lift:bind> name attribute: " + ns.text)
                    }
                    bind(vals, node.child, false, unusedBindings)
                  }
                  case Some(nodes) => {
                    // Mark this bind as used by removing from the unused Set
                    unusedBindings -= ns.text
                    nodes
                  }
                }
              }
            }
          }
          case Group(nodes) => Group(bind(vals, nodes))
          case s: Elem => Elem(node.prefix, node.label, node.attributes, node.scope, false,
            bind(vals, node.child, false, unusedBindings): _*)
          case _ => node
        }
    }

    if (Props.devMode && reportUnused && unusedBindings.size > 0) {
      logger.warn("Unused binding values for <lift:bind>: " + unusedBindings.mkString(", "))
    }

    bindResult
  }

  /**
    * Bind a list of name/xml maps to a block of XML containing lift:bind nodes (see the bind(Map, NodeSeq) function)
    *
    * @return the NodeSeq that results from the specified transforms
    */
  def bindlist(listvals: List[Map[String, NodeSeq]], xml: NodeSeq): Box[NodeSeq] = {
    def build(listvals: List[Map[String, NodeSeq]], ret: NodeSeq): NodeSeq = listvals match {
      case Nil => ret
      case vals :: rest => build(rest, ret ++ bind(vals, xml))
    }

    if (listvals.length > 0) Full(build(listvals.drop(1), bind(listvals.head, xml)))
    else Empty
  }

  /**
    * Bind parameters to XML.
    *
    * @param around XML with lift:bind elements
    * @param atWhat data to bind
    * @deprecated use the bind function instead
    */
  @deprecated("use the bind function instead", "2.3")
  def processBind(around: NodeSeq, atWhat: Map[String, NodeSeq]): NodeSeq = {

    /** Find element matched predicate f(x).isDefined, and return f(x) if found or None otherwise. */
    def findMap[A, B](s: Iterable[A])(f: A => Option[B]): Option[B] =
      s.view.map(f).find(_.isDefined).getOrElse(None)

    around.flatMap {
      v =>
        v match {
          case Group(nodes) => Group(processBind(nodes, atWhat))
          case Elem("lift", "bind", attr@_, _, kids@_*) =>
            findMap(atWhat) {
              case (at, what) if attr("name").text == at => Some(what)
              case _ => None
            }.getOrElse(processBind(v.asInstanceOf[Elem].child, atWhat))

          case e: Elem => {
            Elem(e.prefix, e.label, e.attributes, e.scope, processBind(e.child, atWhat): _*)
          }
          case _ => {
            v
          }
        }

    }
  }

  /**
    * Finds the named attribute in specified XML element and returns
    * a Full Box containing the value of the attribute if found.
    * Empty otherwise.
    *
    * @return a Full Box containing the value of the attribute if found; Empty otherwise
    */
  def xmlParam(in: NodeSeq, param: String): Box[String] = {
    val tmp = (in \ ("@" + param))
    if (tmp.length == 0) Empty else Full(tmp.text)
  }

  /**
    * Given a NodeSeq and a function that returns an Option[T],
    * return the first value found in which the function evaluates
    * to Some
    */
  def findOption[T](nodes: Seq[Node])(f: Elem => Option[T]): Option[T] = {
    nodes.view.flatMap {
      case Group(g) => findOption(g)(f)
      case e: Elem => f(e) orElse findOption(e.child)(f)
      case _ => None
    }.headOption
  }

  /**
    * Given an id value, find the Elem with the specified id
    */
  def findId(nodes: Seq[Node], id: String): Option[Elem] =
    findOption(nodes) {
      e => e.attribute("id").filter(_.text == id).map(i => e)
    }

  /**
    * Given a NodeSeq and a function that returns a Box[T],
    * return the first value found in which the function evaluates
    * to Full
    */
  def findBox[T](nodes: Seq[Node])(f: Elem => Box[T]): Box[T] = {
    nodes.view.flatMap {
      case Group(g) => findBox(g)(f)
      case e: Elem => f(e) or findBox(e.child)(f)
      case _ => Empty
    }.headOption
  }

  /**
    * Find the first Elem in the NodeSeq.  If it has an id attribute,
    * then call the function, f, with that id.  If the first Elem
    * does not have an id attribute, create an id attribute and
    * pass that id attribute to the function
    */
  def findOrCreateId(f: String => NodeSeq => NodeSeq): NodeSeq => NodeSeq =
    ns => {
      var id: Box[String] = Empty

      val realNs = ns map {
        case e: Elem if id.isEmpty => {
          id = e.attribute("id").map(_.text)
          if (id.isDefined) {
            e
          } else {
            val tid = Helpers.nextFuncName
            id = Full(tid)
            import Helpers._
            e % ("id" -> tid)
          }
        }
        case x => x
      }

      f(id openOr Helpers.nextFuncName)(realNs)
    }


  /**
    * Finds the first Element in the NodeSeq (or any children)
    * that has an ID attribute
    */
  def findId(ns: NodeSeq): Box[String] = findBox(ns)(_.attribute("id").map(_.text))

  /**
    * Ensure that the first Element has the specified ID
    */
  def ensureId(ns: NodeSeq, id: String): NodeSeq = {
    var found = false

    ns.map {
      case x if found => x
      case e: Elem => {
        val meta = e.attributes.filter {
          case up: UnprefixedAttribute => up.key != "id"
          case _ => true
        }

        found = true

        new Elem(e.prefix,
          e.label, new UnprefixedAttribute("id", id, meta),
          e.scope, false, e.child: _*)
      }

      case x => x
    }
  }

  /**
    * Finds and returns the first node in the specified NodeSeq and its children
    * with the same label and prefix as the specified element.
    */
  def findNode(in: Elem, nodes: NodeSeq): Box[Elem] = nodes match {
    case seq if seq.isEmpty => Empty
    case Seq(x: Elem, xs@_*)
      if x.label == in.label && x.prefix == in.prefix => Full(x)
    case Seq(x, xs@_*) => findNode(in, x.child) or findNode(in, xs)
  }

  // get the attribute string or blank string if it doesnt exist
  private def attrStr(elem: Elem, attr: String): String = elem.attributes.get(attr) match {
    case None => ""
    case Some(Nil) => "" // why is a blank string converted to a List
    case Some(x) => x.toString // get string on scala.xml.Text
  }

  // types that can be bindByName
  private def bindByNameType(b: String) = b == "input" || b == "select" || b == "button" || b == "a"

  // allow bind by name eg - <input name="namespace:tag"/>
  private def bindByNameTag(namespace: String, elem: Elem) =
    attrStr(elem, "name").replaceAll(namespace + ":", "")


  // mixin what comes from xhtml with what is programatically added
  private def bindByNameMixIn(bindParam: BindParam, s: Elem): NodeSeq = {
    def mix(nodeSeq: NodeSeq): NodeSeq = nodeSeq match {
      case elem: Elem =>
        // mix in undefined attributes
        val attributes = s.attributes.filter(attr => !elem.attribute(attr.key).isDefined)
        elem % attributes
      case Seq(x1: Elem, x2: Elem) if attrStr(x2, "type") == "checkbox" =>
        x1 ++ mix(x2)

      case other =>
        other
    }

    mix(bindParam.calcValue(s) getOrElse NodeSeq.Empty)

  }

  /*
    /**
     * promote a String to a ToCssBindPromotor
     */
    implicit def strToCssBindPromoter(str: String): ToCssBindPromoter =
      new ToCssBindPromoter(Full(str), CssSelectorParser.parse(str))

    /**
     * promote a String to a ToCssBindPromotor
     */
    implicit def cssSelectorToCssBindPromoter(sel: CssSelector): ToCssBindPromoter =
      new ToCssBindPromoter(Empty, Full(sel))
   */

  /**
    * For a list of NodeSeq, ensure that the the id of the root Elems
    * are unique.  If there's a duplicate, that Elem will be returned
    * without an id
    */
  def ensureUniqueId(in: Seq[NodeSeq]): Seq[NodeSeq] = {
    var ids: Set[String] = Set()

    def ensure(in: Elem): Elem = {
      in.attribute("id") match {
        case Some(id) => {
          if (ids.contains(id.text)) {
            new Elem(in.prefix,
              in.label, in.attributes.filter {
                case up: UnprefixedAttribute => up.key != "id"
                case _ => true
              }, in.scope, false, in.child: _*)
          } else {
            ids += id.text
            in
          }
        }

        case _ => in
      }
    }

    in.map {
      case e: Elem => ensure(e)
      case x => x.map {
        case e: Elem => ensure(e)
        case x => x
      }
    }
  }

  /**
    * For a list of NodeSeq, ensure that the the id of the root Elems
    * are unique.  If there's a duplicate, that Elem will be returned
    * without an id
    */
  def deepEnsureUniqueId(in: NodeSeq): NodeSeq = {
    var ids: Set[String] = Set()

    def ensure(node: Node): Node = node match {
      case Group(ns) => Group(ns.map(ensure))
      case in: Elem =>
        in.attribute("id") match {
          case Some(id) => {
            if (ids.contains(id.text)) {
              new Elem(in.prefix,
                in.label, in.attributes.filter {
                  case up: UnprefixedAttribute => up.key != "id"
                  case _ => true
                }, in.scope, false, in.child.map(ensure): _*)
            } else {
              ids += id.text
              new Elem(in.prefix,
                in.label, in.attributes,
                in.scope, false, in.child.map(ensure): _*)
            }

          }

          case _ =>
            new Elem(in.prefix,
              in.label, in.attributes,
              in.scope, false, in.child.map(ensure): _*)
        }

      case x => x
    }

    in.map(ensure)
  }

}

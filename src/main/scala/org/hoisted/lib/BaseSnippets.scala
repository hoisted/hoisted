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
         target="_blank"><img
      src={"http://sketchboard.me/img/" + url}></img>
      </a>
    }) openOr in
  }


  def embedBy: NodeSeq => NodeSeq = ns => {
    val (by, pages) = selectAGroup

    val css = S.attr("css") openOr "*"

    ("*" #> pages.collect{
      case h: HasHtml => h
    }.map(p => css #> p.html)).apply(ns)
  }

  def archivedPosts: NodeSeq => NodeSeq = ns =>  {
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
        val posts = byMonth(month).sortWith{
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
    } yield <a href={env.computeLink(it)}>{ns}</a>) openOr ns
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
        def buildIconLeftLink: NodeSeq =  <span>{icon}{env.computeLinkText(p)}</span>
        def buildIconRightLink: NodeSeq = <span>{env.computeLinkText(p)}{icon}</span>   
        def icon = <i class={ls.mkString(" ")}></i>
        p.findData(MenuIconPlacementKey).flatMap(_.asString).map(_.toLowerCase()) match {
          case Full("right") => buildIconRightLink 
          case _ => buildIconLeftLink 
        }      
      }
      def builTextdLink: NodeSeq =  Text(env.computeLinkText(p))    
      p.findData(MenuIconKey).map(_.forceListString) match {
        case Full(ls) => buildIconLink(ls)
        case _ => builTextdLink 
      }
    }    
    
    def buildMenuDivider(p: ParsedFile): NodeSeq = {
      def buildWithMenuDivider(ls: List[String]): NodeSeq = <li class={ls.mkString(" ")}></li> 
      p.findData(MenuDividerKey).map(_.forceListString) match {
        case Full(ls) => buildWithMenuDivider(ls)    
        case _ =>  NodeSeq.Empty
      }
    }  
    
    if ((ns \\ "item").filter {
      case e: Elem => e.prefix == "menu"
      case _ => false
    }.isDefined) {
      sorted.flatMap(fr =>        
        bind("menu", ns, "item" -> buildLink(fr), "divider" -> buildMenuDivider(fr),
          FuncAttrOptionBindParam("class", (value: NodeSeq) =>
            if (fr eq CurrentFile.value) Some(value) else None, "class"),
          AttrBindParam("href", env.computeLink(fr), "href"))
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

  def doTitle(ns: NodeSeq): NodeSeq = <head><title>{
    val x: Box[NodeSeq] =
      for {
        ses <- S.session
      } yield ses.processSurroundAndInclude("Do Title", ns)

        x.openOr(NodeSeq.Empty).text
    }</title></head>

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
        "data-htag=root [class+]" #> (depthClass + rd) & "a *" #> body & "a [href]" #> ("#"+id) andThen  "* [data-htag]" #> (Empty: Box[String]))).apply(ns)
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
        (a, b) => a.findData(k).flatMap(_.asString).map(_.toLowerCase).openOr("") <
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

    xml.map(v => <lift:tail>{Unparsed(v)}</lift:tail>) openOr NodeSeq.Empty

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
      <li data-post="item"><h2><a data-post="link" href="#">Blog Post</a></h2>
        <h4 style="padding-left: 8px;"><span data-post="date">2012/12/14</span> </h4>
        <div style="padding-left: 15px;" data-post="shortcontent">
          Post Content goes here
        </div>
        <div data-post="more"><a href="#">Read More...</a></div>
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
    (S.attr("address") or S.attr("where") or ns.toList.collect{case e: Elem => e}.headOption.flatMap(_.attribute("data-address").map(_.text))) match {
      case Full(where) =>
        <iframe width={S.attr("width") openOr "640"} height={S.attr("height") openOr "480"} frameborder="0" scrolling="no" marginheight="0" marginwidth="0" src={
        "http://maps.google.com/maps?q="+URLEncoder.encode(where, "UTF-8")+"&output=embed"
        }></iframe>

      case _ =>
        logger.warn("'google.map' snippet invoked, but no 'address' or 'where' parameter specified")
        ns
    }
  }

  def disqusCount: NodeSeq => NodeSeq = ns => {
    S.attr("shortname") match {
      case Full(acct) =>

        val xf = "data-hoisted-type=post" #> ((ns: NodeSeq) =>
          ns ++ <small>&nbsp; {
    <a href={ns.collect{case e: Elem => e}.headOption.flatMap(_.attribute("href").
      map(a => Text(a.text+"#disqus_thread")))}></a>
    }</small>
          )

        PostPageTransforms.set(PostPageTransforms.get :+ xf)


        <tail>
    <script type="text/javascript">
      {
      Unparsed("""
      // <![CDATA[
      /* * * CONFIGURATION VARIABLES: EDIT BEFORE PASTING INTO YOUR WEBPAGE * * */
      var disqus_shortname = """ + acct.encJs + """;

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
        {
        Unparsed("""

        // <![CDATA[
        /* * * CONFIGURATION VARIABLES: EDIT BEFORE PASTING INTO YOUR WEBPAGE * * */
        var disqus_shortname = """ + acct.encJs + """;

        /* * * DON'T EDIT BELOW THIS LINE * * */
        (function() {
        var dsq = document.createElement('script'); dsq.type = 'text/javascript'; dsq.async = true;
    dsq.src = 'https://' + disqus_shortname + '.disqus.com/embed.js';
    (document.getElementsByTagName('head')[0] || document.getElementsByTagName('body')[0]).appendChild(dsq);
    })();
    // ]]>
    """)}
    </script>
    <noscript>Please enable JavaScript to view the <a href="http://disqus.com/?ref_noscript">comments powered by Disqus.</a></noscript>
      <a href="http://disqus.com" class="dsq-brlink">comments powered by <span class="logo-disqus">Disqus</span></a>
    </xml:group>

      case _ =>
        logger.warn("'disqus' snippet invoked, but no 'shortname' parameter specified")
        ns

  }
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


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

/**
 * Created with IntelliJ IDEA.
 * User: dpp
 * Date: 7/2/12
 * Time: 6:14 PM
 * To change this template use File | Settings | File Templates.
 */

object BaseSnippets {
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
          AttrBindParam("href", Text(rec.findData("redirect").flatMap(_.asString) openOr env.computeLink(rec)), "href"))
      }
    } else {
      ("*" #> info.map{
        case (rec, title, desc) => "@title *" #> title &
          "@desc *" #> desc &
          "a [href]" #> (rec.findData("redirect").flatMap(_.asString) openOr env.computeLink(rec))
      }).apply(in)
    }
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

  def pageInfo: NodeSeq => NodeSeq = ns => {
    val name = S.attr("name") or S.attr("info")

    val data: Box[NodeSeq] = name.flatMap(CurrentFile.value.findData(_)).flatMap(data =>
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
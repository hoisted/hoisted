package com.liftweb
package lib

import net.liftweb._
import common._
import http._
import util._
import Helpers._

import java.io.File
import java.util.Locale
import scala.xml.{Elem, NodeSeq, Text}

object CMS extends LiftRules.DispatchPF {
  type Key = CMSKey
  type Record = Content

  def rootDir: File = null

  def isDefinedAt(req: Req): Boolean = {
    req.path.wholePath.takeRight(1) match {
      case x :: _ if x.endsWith(".html") => true
      case _ =>
      setReq(req)
      req.get_? && (currentRecord.is.map(_.content.standAlone_?) openOr false)
    }
  }


  def apply(req: Req): () => Box[LiftResponse] = 
    serveIt(req) _

  private def serveIt(req: Req)(): Box[LiftResponse] = {
    req.path.wholePath.takeRight(1) match {
      case x :: _ if x.endsWith(".html") => 
        val p = req.path.partPath.mkString("/", "/", "")
        S.redirectTo(p)
      
      case _ =>
      setReq(req)
      currentRecord.is.map(fr => fr -> fr.content).
      flatMap {
        case (r, _) if r.redirectTo.isDefined =>
          r.redirectTo.map(S.redirectTo(_))
        
        case (_, HtmlContent(elem, true)) =>
          for {
            session <- S.session
            resp <- processRequest(elem, session, req)
          } yield resp
        
        case (_, CSSContent(css)) => Full(CSSResponse(css))
        case (_, JavaScriptContent(javascript)) => 
          Full(JavaScriptResponse(js.JE.JsRaw(javascript).cmd))
        
        case _ => Empty
      }
    }
  }

  def chooseNode(in: NodeSeq): NodeSeq = {
    val num = S.attr("cnt").flatMap(Helpers.asInt) openOr 1

    val lst = in.toArray.collect {
      case e: Elem => e
    }
    val cnt = lst.size

    for {
      i <- 0 until cnt
    } {
      val ri = randomInt(cnt)
      val n = lst(ri)
      lst(ri) = lst(i)
      lst(i) = n
    }
    
    lst.take(num).toList
  }

  def testAttr(in: NodeSeq): NodeSeq = {
    for {
      toTest <- S.attr("toTest").toList
      attr <- S.attr(toTest).toList if attr == "true"
      node <- in
    } yield node
  }
  
  def filterAndOrder(st: Stream[FileRecord]) = {
    st.filter(_.content.standAlone_?).filter(_.currentlyValid_?).
    filter(r => Full(Host(r.host)) == currentHost.is).sortWith {
      case (a, b) => 
        (a.validFrom openOr a.changeDate).millis > 
      (b.validFrom openOr b.changeDate).millis
    }
  }

  
  def link(in: NodeSeq): NodeSeq = {

    val r: Box[FileRecord] = 
      for {
        name <- S.attr("name")
        recs <- filterAndOrder(CMSStore.findByTag("name", Full(name))).headOption
      } yield recs
      
    r match {
      case Full(rec) => <a href={rec.path.mkString("/", "/", "")}>{in}</a>
      case _ => in
    }
  }
  
  def menu(in: NodeSeq): NodeSeq = {
    val map = Map(filterAndOrder(CMSStore.findByTag("menu", Empty)).toList.
    reverse.map(r => r.path -> r) :_*)

    // get rid of duplicate path entries
    map.values.toList.sortWith {
      case (a, b) => (a.findTag("order").
                      flatMap(_.value).
                      flatMap(Helpers.asInt) openOr 0) <
      (b.findTag("order").
       flatMap(_.value).
       flatMap(Helpers.asInt) openOr 0)
    }.flatMap {
      fr =>
        bind("menu", in, "item" -> 
             (fr.findTag("menu").flatMap(_.value) openOr fr.path.mkString("/", "/", "")),
             FuncAttrOptionBindParam("class", (value: NodeSeq) => 
               if (currentRecord.is == Full(fr.content)) Some(value) else None, "class"),
           AttrBindParam("href", Text(fr.redirectTo openOr fr.path.mkString("/", "/", "")), "href"))
    }
    
  }

  def subs(in: NodeSeq): NodeSeq = 
    for {
      tpe <- S.attr("type").toList
      max = S.attr("max").flatMap(asInt).filter(_ > 0) openOr 10
      rec <- filterAndOrder(CMSStore.findByTag("sub", Full(tpe))).take(max)
      val title = rec.findTag("sub").flatMap(_.attr) openOr Text("Happening")
      val desc = rec.findTag("desc").flatMap(_.attr)
      node <- bind("sub", in, "title" -> title, "desc" -> desc,
                 AttrBindParam("href", Text(rec.redirectTo openOr rec.path.mkString("/", "/", "")), "href"))
    } yield node
  

  def processRequest(elem: Elem, session: LiftSession, request: Req): Box[LiftResponse] = {
    import actor.LAFuture

    val future = new LAFuture[LiftResponse]

    // it's a little hacky that we have to ship this off to
    // another thread for processing, but the Lift function snapshotting
    // helpers make it better to do it this way.
    val f = session.buildDeferredFunction(wrapState
      {
        S.mapSnippet("choose", chooseNode _)
        S.mapSnippet("if", testAttr _)
        S.mapSnippet("a", link _)
        S.mapSnippet("xmenu", menu _)
        S.mapSnippet("subs", subs _)

        val ieMode = session.ieMode.is

        // Phase 1: snippets & templates processing
        val rawXml = session.processSurroundAndInclude(PageName get, elem)
        // Phase 2: Head & Tail merge, add additional elements to body & head
        val xml = session.performHeadMerge(rawXml, request)

        // Phase 3: Response conversion including fixHtml
        val resp = LiftRules.convertResponse((xml, 200),
                                             S.getHeaders(("Content-Type" -> "text/html") :: LiftRules.defaultHeaders((xml, request)).filter(_._1 equalsIgnoreCase "content-type")),
                                             S.responseCookies,
                                             request)
        
        future.satisfy(resp)
      })

    // run it on another thread
    ActorPing.schedule(f, 0 seconds)
    
    // and get the response
    future.get(2000)
  }

  /**
   * Sets the current Req and clears out the key, etc. vars
   */
  private def setReq(req: Req) {
    if (currentReq.is != Full(req)) {
      currentReq.set(Full(req))
      currentLocale.remove()
      currentHost.remove()
      currentKey.remove()
      currentRecord.remove()
    }
  }

  private def wrapState[T](f: => T): () => T = {
    val req = currentReq.is
    val loc = currentLocale.is
    val host = currentHost.is
    val key = currentKey.is
    val rec = currentRecord.is

    () => currentReq.doWith(req) {
      currentLocale.doWith(loc) {
        currentHost.doWith(host) {
          currentKey.doWith(key) {
            currentRecord.doWith(rec) {
              f
            }
          }
        }
      }
    }
  }
  
  def defaultHost = "liftweb.net"

  def validHosts = Set("liftweb.net" /*, "liftweb.com"*/)

  def fixHost(host: String) = host.toLowerCase.roboSplit("\\.").takeRight(2).mkString(".")

  /**
   * Set up current request state... calculate the host
   */
  private def calcHost() = currentReq.map(req => fixHost(req.hostName)) match {
    case Full(h) if validHosts.contains(h) => Full(Host(h))
    case _ => Full(Host(defaultHost))
  }

  private def calcLocale() = Locale.US
  /*LiftRules.localeCalculator(
    currentReq.is.map(_.request))*/

  /**
   * Set up current request state... calculate the Key
   */
  private def calcKey(): Box[Key] = 
    for {
      host <- currentHost.is
      req <- currentReq
      path = Path(req.path.wholePath)
      locale <- currentLocale.is
    } yield CMSKey(host, path, locale)
  

  /**
   * Set up current request state... calculate the Key
   */
  private def calcRecord(): Box[FileRecord] = 
    for {
      key <- currentKey.is
      record <- CMSStore.find(key)
    } yield record

  private object currentReq extends TransientRequestVar[Box[Req]](Empty) {
    override val __nameSalt = randomString(10)
  }

  private[lib] object currentLocale extends 
  TransientRequestVar[Box[Locale]](Full(calcLocale())) {
    override val __nameSalt = randomString(10)
  }

  private[lib] object currentHost extends TransientRequestVar[Box[Host]](calcHost()) {
    override val __nameSalt = randomString(10)
  }

  private object currentKey extends TransientRequestVar[Box[Key]](calcKey()) {
    override val __nameSalt = randomString(10)
  }

  private object currentRecord extends TransientRequestVar[Box[FileRecord]](calcRecord()) {
    override val __nameSalt = randomString(10)
  }

}

object CMSViewFinder extends LiftRules.ViewDispatchPF {
  def isDefinedAt(path: List[String]) = {
    val record = 
      for {
        host <- CMS.currentHost.is
        locale <- CMS.currentLocale.is
        key = CMSKey(host, new Path(path), locale)
        record <- CMSStore.find(key)
      } yield record

    record.collect {
      case r if r.redirectTo.isEmpty && r.htmlContent_? => true
    } openOr false
  }

  def apply(path: List[String]) = Left(serve(path) _)

  def serve(path: List[String])(): Box[NodeSeq] = {
    for {
      host <- CMS.currentHost.is
      locale <- CMS.currentLocale.is
      key = CMSKey(host, new Path(path), locale)
      HtmlContent(elem, _) <- CMSStore.find(key).map(_.content)
    } yield {
      elem
    }
  }
}

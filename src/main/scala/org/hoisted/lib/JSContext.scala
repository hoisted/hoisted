package org.hoisted.lib

import net.liftweb.common.Box
import net.liftweb.common.Full
import net.liftweb.http.{LiftSession, TransientRequestVar, LiftRules}
import net.liftweb.util._
import Helpers._
import org.mozilla.javascript._
import scala.util.Success
import scala.xml.NodeSeq
import net.liftweb.actor.LAFuture
import net.liftweb.common._

/**
 * You can add a JavaScript context to Lift so that
 * you can run server-side JavaScript as part of Lift page
 * rendering.
 *
 * In Boot.scala, just do `JavaScriptContext.install()`
 * and you get a JavaScript execution context around
 * all HTTP requests.
 */
object JSContext {

  def runSourceContext(session: LiftSession, value: Any, xform: Any => NodeSeq => NodeSeq, ns: NodeSeq): NodeSeq = {
    import scala.collection.JavaConverters._

    value match {
      case null => NodeSeq.Empty
      case None => NodeSeq.Empty
      case _: EmptyBox => NodeSeq.Empty
      case b: Box[_] => runSourceContext(session, b.toList, xform, ns)
      case b: Option[_] => runSourceContext(session, b.toList, xform, ns)
      case fut: LAFuture[_] => runSourceContext(session, fut.get(5.seconds).openOr(Empty), xform, ns)
      case node: scala.xml.Node => session.currentSourceContext.doWith(node)(session.processSurroundAndInclude("Source", xform(node)(ns)))
      case na: org.mozilla.javascript.NativeArray =>
        val len = na.getLength.toInt
        val ar = new Array[Object](len)
        var pos = 0
        while (pos < len) {
          ar(pos) = na.get(pos, na)
          pos += 1
        }
        runSourceContext(session, ar.toList, xform, ns)
      case n: java.lang.Iterable[_] => runSourceContext(session, n.iterator(), xform, ns)
      case n: java.util.Iterator[_] =>
        for {i <- n.asScala.toSeq; nodes <- session.currentSourceContext.doWith(i)(session.processSurroundAndInclude("Source", xform(i)(ns)))} yield nodes
      case en: java.util.Enumeration[_] =>
        for {i <- en.asScala.toSeq; nodes <- session.currentSourceContext.doWith(i)(session.processSurroundAndInclude("Source", xform(i)(ns)))} yield nodes
      case se: scala.collection.Iterable[_] => runSourceContext(session, se.iterator, xform, ns)
      case se: scala.collection.Iterator[_] =>
        for {i <- se.toSeq; nodes <- session.currentSourceContext.doWith(i)(session.processSurroundAndInclude("Source", xform(i)(ns)))} yield nodes
      case a: Array[_] => runSourceContext(session, a.toList, xform, ns)
      case x =>
        session.currentSourceContext.doWith(x)(session.processSurroundAndInclude("Source", xform(x)(ns)))
    }
  }

  def buildXformer(session: LiftSession, xformRule: String, value: Any): NodeSeq => NodeSeq = {


    def retFunc(value: Any)(ns: NodeSeq): NodeSeq = {
      val func: NodeSeq => NodeSeq =
        value match {
          case n: scala.xml.Node => xformRule #> n
          case ns: NodeSeq => xformRule #> ns
          case ns: Seq[_] if !ns.exists(!_.isInstanceOf[scala.xml.Node]) => xformRule #> ns.asInstanceOf[NodeSeq]
          case n: String => xformRule #> n
          // case b: Bindable => xformRule #> b
          case n: java.lang.Number => xformRule #> n
          case d: Double => xformRule #> d
          case jc: ToJsCmd => xformRule #> jc
          case i: Int => xformRule #> i
          case sb: StringPromotable => xformRule #> sb
          case sym: Symbol => xformRule #> sym
          case lng: Long => xformRule #> lng
          case b: Boolean => xformRule #> b
          case b: Box[_] => b match {
            case Full(x) => buildXformer(session, xformRule, x)
            case _ => xformRule #> Empty.asInstanceOf[Box[String]]
          }
          case b: Option[_] => b match {
            case Some(x) => buildXformer(session, xformRule, x)
            case _ => xformRule #> None.asInstanceOf[Option[String]]
          }

          case fut: LAFuture[_] => buildXformer(session, xformRule, fut.get(5 second))
          case n: java.lang.Iterable[_] => runSourceContext(session, n.iterator(), retFunc _, _)
          case n: java.util.Iterator[_] => runSourceContext(session, n, retFunc _, _)
          case en: java.util.Enumeration[_] => runSourceContext(session, en, retFunc _, _)
          case se: scala.collection.Iterable[_] => runSourceContext(session, se, retFunc _, _)
          case se: scala.collection.Iterator[_] => runSourceContext(session, se, retFunc _, _)
          case x => xformRule #> x.toString
        }

      func(ns)
    }

    retFunc(value) _
  }

  /**
   * Hook into LiftRules to put a JavaScript
   * execution loanwrapper around everything and
   * also slurp in <script> tags with the data-lift-server attribute.
   */
  def install() {
    LiftRules.allAround.append(JSWrapper)
    LiftRules.tagProcessor.prepend {
      case ("script", e, session) if e.attribute("data-server-js").isDefined =>
        exec(e.text)
        NodeSeq.Empty
    }

    LiftRules.dataAttributeProcessor.append {
      case ("js", value, elem, session) =>
        currentScript.get.withIt(session.currentSourceContext.get) {
          val (rule, v2): (NodeSeq => NodeSeq, Box[String]) =
            value.roboSplit("\\#\\>") match {
              case x :: Nil => (PassThru, Full(x))
              // case x :: "it" :: Nil => session.buildXformer(x, Nil) -> Empty
              case x :: str :: Nil =>
                buildXformer(session, x, exec(str)) -> Empty
              case x :: xs => buildXformer(session, x, Nil) -> Full(xs.mkString)
              case _ => (PassThru, Full(value))

            }


          v2 match {
            case Full(v22) =>
              exec(v22) match {
                case fut: LAFuture[_] => val ret = new LAFuture[NodeSeq]
                  fut.foreach(v => ret.satisfy(session.runSourceContext(v, rule, elem)))
                  ret

                case func: Function0[_] =>
                  () => {
                    session.runSourceContext(func(), rule, elem)
                  }

                case x => session.runSourceContext(x, rule, elem)
              }

            case _ => rule(elem)
          }
        }

    }
  }

  object currentScript extends TransientRequestVar[JSScope]({
    new JSScope
  }) {
    registerCleanupFunc(in => get.bye())
  }

  private object JSWrapper extends LoanWrapper {
    def apply[T](f: => T): T = {
      currentScript.get
      f
    }
  }

  /**
   * Execute some JavaScript in the current context
   * @param str the string to execute
   * @return the value returned from the JavaScript execution
   */
  def exec(str: String): AnyRef = currentScript.get.exec(str)

  class JSScope {
    private var initted = false
    private var context: Context = null
    private var scope: ScriptableObject = null

    def theScope() = scope

    def theContext() = context

    def withIt[T](currentIt: Any)(f: => T): T = {
      val cur = scope.get("it")
      scope.put("it", scope, currentIt)
      try {
        f
      } finally {
        if (null eq cur) {
          scope.delete("it")
        } else {
          scope.put("it", scope, cur)
        }
      }
    }

    def init() {
      initted = true
      context = Context.enter()
      scope = context.initStandardObjects()
      scope.put("currentFile", scope, CurrentFile)
      scope.put("fileParser", scope, ParsedFile)
      scope.put("help", scope, JSHelper)
    }

    def bye() {
      if (initted) Context.exit()
    }

    def exec(str: String): AnyRef = synchronized {
      if (!initted) init()

      context.evaluateString(scope, str, "Lift", 0, null) match {
        case njo: NativeJavaObject => njo.unwrap()
        case x => x
      }
    }
  }

}

object JSHelper {
  def toJs(in: Any): Any = {
    val scope = JSContext.currentScript.get.theScope()
    val context = JSContext.currentScript.get.theContext()
    def _toJs(in: Any): Any = {
      in match {
        case x: Seq[_] =>
          val nobj = context.newArray(scope, 0).asInstanceOf[NativeArray]
          x.zipWithIndex.foreach { case (v, i) => nobj.put(i, nobj, _toJs(v)) }
          nobj

        case x: java.util.List[_] =>
          val nobj = context.newArray(scope, 0).asInstanceOf[NativeArray]
          var n = 0
          val it = x.iterator()
          while (it.hasNext) {
            nobj.put(n, nobj, _toJs(it.next()))
            n += 1
          }

          nobj

        case it: java.util.Iterator[_] =>
          val nobj = context.newArray(scope, 0).asInstanceOf[NativeArray]
          var n = 0
          while (it.hasNext) {
            nobj.put(n, nobj ,_toJs(it.next()))
            n += 1
          }

          nobj

        case m: Map[_, _] =>
          val nobj = context.newObject(scope).asInstanceOf[ScriptableObject]
          m.foreach { case (k, v) => nobj.defineProperty(_toJs(k).toString, _toJs(v), 0) }
          nobj

        case m: java.util.Map[_, _] =>
          val nobj = context.newObject(scope).asInstanceOf[ScriptableObject]
          val it = m.entrySet().iterator()
          while (it.hasNext) {
            val n = it.next()
            val k = n.getKey
            val v = n.getValue
            nobj.defineProperty(_toJs(k).toString, _toJs(v), 0)
          }
          nobj

        case Full(x) => _toJs(x)

        case x: EmptyBox => null

        case Some(x) => _toJs(x)

        case None => null

        case Success(x) => _toJs(x)

        case _: scala.util.Failure[_] => null

        case x => x
      }
    }

    _toJs(in)

  }
}
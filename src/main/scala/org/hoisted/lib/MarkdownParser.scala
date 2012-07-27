package org.hoisted.lib

import net.liftweb._
import common._
import util.Html5
import org.pegdown.{Extensions, PegDownProcessor}
import xml.{Elem, NodeSeq}

/**
 * Created with IntelliJ IDEA.
 * User: dpp
 * Date: 6/8/12
 * Time: 11:56 AM
 * To change this template use File | Settings | File Templates.
 */


object MarkdownParser {
  lazy val matchMetadata = """(?m)\A(:?[ \t]*\n)?(?:-{3,}+\n)?(^([a-zA-Z0-9 _\-]+)[=:]([^\n]*)\n+(:?[ \t]*\n)?)+(:?-{3,}+\n)?""".r

  lazy val topMetadata = """(?m)^([^:]+):[ \t]*(.*)$""".r

  lazy val lineSplit = """(?m)^(.*)$""".r

  lazy val linkDefs = """(?m)^\p{Space}{0,3}\[([^:]+)[=:](?:[ ]*)(.+)\]:""".r


  def readTopMetadata(in: String): (String, List[(String, String)]) = {
    val (_in, pairs): (String, List[(String, String)]) = matchMetadata.findFirstIn(in) match {
      case Some(data) =>
        (matchMetadata.replaceAllIn(in, ""),
        lineSplit.findAllIn(data).toList.flatMap(s =>
          topMetadata.findAllIn(s).matchData.toList.map(md => (md.group(1).trim, md.group(2).trim))))
      case None => (in, Nil)
    }

    val pairs2: List[(String, String)] =
      linkDefs.findAllIn(_in).matchData.toList.map(md => (md.group(1).trim, md.group(2).trim))

    (_in, pairs ::: pairs2)
  }

  def parse(in: String): Box[(NodeSeq, List[(String, String)])] = {
    val (_in, retPairs) = readTopMetadata(in)

    val pd = new PegDownProcessor(Extensions.FENCED_CODE_BLOCKS | Extensions.QUOTES | Extensions.SMARTYPANTS)
    val raw = pd.markdownToHtml(
      _in.replace("![](", "![ ](") // Fixes #8 -- change when we change from Pegdown processor
    )

    val res = Html5.parse("<html><head><title>I eat yaks</title></head><body>"+raw+"</body></html>")

    val r2: Box[NodeSeq] = res.map{
      res => (res \ "body").collect{case e: Elem => e}.flatMap(_.child)
    }
    r2.map(v => v -> retPairs)
  }
}

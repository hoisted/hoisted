package code
package snippet

import org.specs2.mutable._
//import org.specs.runner.JUnit4
//import org.specs.runner.ConsoleRunner
import scala.xml._
import java.io.{OutputStreamWriter, ByteArrayOutputStream}
import org.hoisted.lib.{HoistedHtml5, ParsedFile}


//class HelloWorldTestSpecsAsTest extends JUnit4(HelloWorldTestSpecs)
//object HelloWorldTestSpecsRunner extends ConsoleRunner(HelloWorldTestSpecs)

object HelloWorldTestSpecs extends Specification {

  "HTML Parser" should {
    "Deal with Entities" in {
      val xml: NodeSeq = <div>&nbsp;Dog</div>
      val out = new ByteArrayOutputStream()
      val or = new OutputStreamWriter(out, "UTF-8")
      HoistedHtml5.write(xml.collect {
        case e: Elem => e
      }.headOption getOrElse <html/>, or, false, false)
      or.flush()
      val str = new String(out.toByteArray, "UTF-8")

      str must_== "<div>&nbsp;Dog</div>"
    }


    "Round trip the parsing" in {
      val xmlStr: String = "<div>&nbsp;Dog</div>"

      val xml: NodeSeq = ParsedFile.parseHtml5File(xmlStr).
        openOrThrowException("This should succeed... it's a test failure if it doesn't")

      val out = new ByteArrayOutputStream()
      val or = new OutputStreamWriter(out, "UTF-8")
      HoistedHtml5.write(xml.collect {
        case e: Elem => e
      }.headOption getOrElse <html/>, or, false, false)
      or.flush()
      val str = new String(out.toByteArray, "UTF-8")

      str must_== xmlStr
    }

  }


}

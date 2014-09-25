package org.hoisted.lib

/**
 * Created with IntelliJ IDEA.
 * User: dpp
 * Date: 11/13/12
 * Time: 4:00 PM
 * To change this template use File | Settings | File Templates.
 */

class TelegramRunner extends Function0[AnyRef] with LazyLoggableWithImplicitLogger {
  import net.liftweb._
  import util._
  import java.io._
  import java.net._
  import common._
  import java.util._
  import org.joda.time._
  import scala.sys.process._

  var serverMode = false
  var rootDir: String = ""

  var theLocale: String = "en_US" // en_US
  var theTimeZone: String = "PST"// PST

  var theSiteUrl: String = "http://blog.goodstuff.im" // http://dogscape.com

  var theGuid: String = "XXXGUIDXXX" // F1235554

  var rootUrl: String = "https://telegr.am" // https://telegr.am

  /*
  var theLocale: String = "XXXLOCALEXXX" // en_US
  var theTimeZone: String = "XXXTIMEZONEXXX"// PST

  var theSiteUrl: String = "XXXURLXXX" // http://dogscape.com

  var theGuid: String = "XXXGUIDXXX" // F1235554

  var rootUrl: String = "XXXROOTURL" // https://telegr.am
*/

  /**
   * Load a repo if the current user has a matching site
   * @param urlStr
   * @param env
   * @param dest
   * @return
   */
  private def myRepoLoader(urlStr: String, env: EnvironmentManager, dest: File): Box[Boolean] = {
    val place = Helpers.nextFuncName
    val url = new URL(rootUrl+"/api/pull_files/"+theGuid+"/"+
      Helpers.urlEncode(urlStr)+"/"+place)

    for {
      conn <- Helpers.tryo(url.openConnection())
      contentStream <-Helpers.tryo(conn.getContent).asA[InputStream]
      stuff <- Helpers.tryo(Helpers.readWholeStream(contentStream))
      _ <- Helpers.tryo(contentStream.close)
      destDir = s"/data/pulled/${place}"
      _ = Seq("rm", "-rf", dest.getAbsolutePath).!
      _ = Seq("cp", "-r", destDir, dest.getAbsolutePath).!
      _ = println(s"Yay... got a repo... and copied it from ${destDir} to ${dest.getAbsolutePath}... ls:\n ${s"ls -l ${destDir}".!!}")
    } yield true
  }

  def writeMetadata(in: Box[HoistedTransformMetaData]): Box[HoistedTransformMetaData] = {
    val out = new File(new File( System.getProperty("user.home")), theGuid+".out")
    import net.liftweb.json._
    import ext._
    import Serialization.{read, write}
    implicit val fmts: Formats = Serialization.formats(NoTypeHints) + new JsonBoxSerializer

    Helpers.tryo {
    val s: String = write(in)
    val fos = new FileOutputStream(out)
    fos.write(s.getBytes("UTF-8"))
    fos.close()
    }
    in
  }


  def apply(): AnyRef = {

    def emSetup(em: EnvironmentManager): EnvironmentManager = {
      em.setMetadata(SiteLinkKey, StringMetadataValue(theSiteUrl))

      logger.info("Set the "+SiteLinkKey+" to "+theSiteUrl)

      if (!serverMode) {
        em.externRepoLoader = Full(myRepoLoader)
      }

      em.runWrapper = new CommonLoanWrapper() {
        def apply[T](f: => T): T =
          DateUtils.CurrentLocale.doWith(new Locale(theLocale))(
            DateUtils.CurrentTimeZone.doWith(DateTimeZone.forID(theTimeZone))(f))
      }
      em
    }

    if (serverMode) {
      val server = new HttpStaticFileServer(8080)
      server.run(new File(rootDir), emSetup(_))
      while (true) {
        Thread.sleep(1000)
      }
    } else {
      val em = emSetup(HoistedEnvironmentManager.value)
      em.addToFinalFuncs(writeMetadata)
    }

    ""
  }

}
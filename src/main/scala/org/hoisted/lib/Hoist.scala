package org.hoisted.lib

import java.io.File
import net.liftweb.common._
import net.liftweb.common.Full
import net.liftweb.util.{PCDataXmlParser, ThreadGlobal, Helpers}
import org.joda.time.format.{DateTimeFormatter, ISODateTimeFormat, DateTimeFormat}
import org.joda.time.{DateTime, DateTimeZone}
import java.util.Locale
import xml.{NodeSeq, Elem}

/**
 * Created with IntelliJ IDEA.
 * User: dpp
 * Date: 7/23/12
 * Time: 10:30 AM
 * To change this template use File | Settings | File Templates.
 */

object Hoist extends LazyLoggableWithImplicitLogger {
  def main(args: Array[String]) {

    val em = new EnvironmentManager()

    val s: Box[HoistedTransformMetaData] =
    HoistedEnvironmentManager.doWith(em) {
      JSContext.install()
      val info = slurpParams(args.toList)

      info.classInfo match {
        case Full((cl, clz)) =>
          val toRun = clz.filter(s => s == "Boot" || s.endsWith(".Boot"))
          for {
            clzName <- toRun
            theClz <- Helpers.tryo(Nil)(cl.loadClass(clzName).asInstanceOf[Class[AnyRef]])
            inst1 <- Helpers.tryo(Nil)(theClz.newInstance)
            inst <- Full(inst1).asA[Function0[AnyRef]]
          } logger.info("Booting " + clzName + " res " + inst.apply())

        case x =>
      }

      info match {
        case Info(_, _, f: Failure) =>
          logger.error("Failed to compile code " + f.msg); sys.exit(127)
        case Info(Full(from), Full(to), _) =>
          logger.info("Running hoisted with "+em.metadata)
          RunHoisted(new File(from), new File(to), HoistedEnvironmentManager.value)

        case _ => logger.error("Usage 'java -jar hoisted.jar source_directory destination_directory"); sys.exit(127)
      }
    }


    s match {
      case Full(_) => System.exit(0)
      case _ => System.exit(1)
    }
  }

  case class Info(source: Box[String], dest: Box[String], classInfo: Box[(ClassLoader, Set[String])] = Empty)


  def slurpParams(p: List[String]): Info = {


    val (p2, cl) = {
      val (sc, notSc) = p.partition(_.toLowerCase().endsWith(".scala"))
      val read = sc.flatMap(name => Helpers.tryo {
        (name -> new String(Helpers.readWholeFile(new File(name)), "UTF-8"))
      })

      val ct = new CompileTool()

      (notSc, Full(1).filter(i => !read.isEmpty).flatMap(i => {
        val res = ct.classloaderFor(read);
        res
      }))
    }

    def theRunner(in: Box[TelegramRunner]): TelegramRunner = in openOr (new TelegramRunner)

    def doIt(runner: Box[TelegramRunner], rest: List[String], toDo: List[String]): (Box[TelegramRunner], List[String]) = toDo match {
      case Nil => runner -> rest
      case "-server" :: dir :: _ =>
        val r = theRunner(runner)
        r.serverMode = true
        r.rootDir = dir
        (Full(r), Nil)

      case "-tz" :: tz :: stuff => val r = theRunner(runner)
      r.theTimeZone = tz
      doIt(Full(r), rest, stuff)
      case "-locale" :: locale :: stuff => val r = theRunner(runner)
      r.theLocale = locale
      doIt(Full(r), rest, stuff)
      case "-site" :: site :: stuff => val r = theRunner(runner)
      r.theSiteUrl = site
      doIt(Full(r), rest, stuff)
      case "-guid" :: guid :: stuff => val r = theRunner(runner)
      r.theGuid = guid
      doIt(Full(r), rest, stuff)
      case "-rooturl" :: rooturl :: stuff => val r = theRunner(runner)
      r.rootUrl = rooturl
      doIt(Full(r), rest, stuff)
      case x :: stuff => doIt(runner, rest ::: List(x), stuff)
    }

    val (runner, p3) = doIt(Empty, Nil, p2)

    runner.map(_.apply())

    p3 match {
      case from :: to :: Nil => Info(Full(from), Full(to), cl)
      case _ => Info(Empty, Empty, cl)
    }
  }
}

trait LoggerWithImplicitLogger extends Logger {
  protected implicit def __myImplicitLoggerDude: Logger = this
}

trait LazyLoggableWithImplicitLogger extends LazyLoggable {
  protected implicit def __myImplicitLoggerDude = logger
}

object HoistedUtil extends LazyLoggableWithImplicitLogger {
  def allFiles(dir: File, filter: File => Boolean): List[File] = {
    if (!filter(dir)) Nil
    else if (dir.isDirectory()) {
      dir.listFiles().toList.sortWith(_.isDirectory <= _.isDirectory).flatMap(allFiles(_, filter))
    } else if (dir.isFile() && !dir.getName.startsWith(".")) List(dir)
    else Nil
  }

  def loadFilesFrom(inDir: File, current: Map[String, ParsedFile]): Box[List[ParsedFile]] = {
    def computeFileInfo(f: File): FileInfo = {
      val cp: String = f.getAbsolutePath().substring(inDir.getAbsolutePath.length)
      val pureName = f.getName
      val dp = pureName.lastIndexOf(".")
      val (name, suf) = if (dp <= 0) (pureName, None)
      else if (pureName.toLowerCase.endsWith(".cms.xml"))
        (pureName.substring(0, pureName.length - 8), Some("cms.xml"))
      else (pureName.substring(0, dp),
        Some(pureName.substring(dp + 1)))
      FileInfo(Full(f), cp, name, pureName, suf)
    }
    for {
      allFiles <- HoistedUtil.logFailure("allFiles for "+inDir)(allFiles(inDir, f => f.exists() && !f.getName.startsWith(".") && f.getName.toLowerCase != "readme" &&
        f.getName.toLowerCase != "readme.md"))

      fileInfo <- HoistedUtil.logFailure("File Info for allFiles")(allFiles.map(computeFileInfo(_)))

      ret <- HoistedUtil.logFailure("Reading files")(fileInfo.flatMap(fi =>
        HoistedUtil.reportFailure("Loading "+fi.pathAndSuffix.display)(ParsedFile(fi, current))))

    } yield ret.filter(_.findData(RemovedKey).isEmpty)
  }

  private var localeMap: Map[String, Box[Locale]] = Map.empty
  private val localeSync = new Object

  def fileForName(path: List[String], files: List[ParsedFile]): Box[ParsedFile] =
  files.toStream.find(_.matchPath(path)).headOption

  def bytesForFile(path: List[String], files: List[ParsedFile]): Box[Array[Byte]] =
  for {
    file <- fileForName(path, files)
    bytes <- file.bytes
  } yield bytes

  def stringForFile(path: List[String], files: List[ParsedFile]): Box[String] =
  for {
    bytes <- bytesForFile(path, files)
    str <- Helpers.tryo(new String(bytes, "UTF-8"))
  } yield str

  def xmlForFile(path: List[String], files: List[ParsedFile]): Box[NodeSeq] =
    for {
      str <- stringForFile(path, files)
      xml <- PCDataXmlParser.apply(str)
    } yield xml


  def toLocale(in: String): Box[Locale] = {
    localeSync.synchronized{
      val is = in.trim.toLowerCase
      localeMap.get(is) match {
        case Some(ret) => ret
        case _ =>
          val ret: Box[Locale] = Locale.getAvailableLocales.filter(_.toString.toLowerCase == is).headOption
          localeMap += is -> ret
          ret
      }
    }
  }

  lazy val safe = """[^\w]""".r

  lazy val noLeadingDash = """^(\-)+""".r
  lazy val notrailingDash = """(\-)+$""".r
  lazy val multipleDash = """(\-){2,}""".r

  def slugify: String => String = in => {

    val r1 = safe.replaceAllIn(in.trim.toLowerCase, "-")

    val r2 = noLeadingDash.replaceAllIn(r1, "")
    multipleDash.replaceAllIn(notrailingDash.replaceAllIn(r2, ""), "-") match {
      case "" => "x"
      case s => s
    }
  }

  /*
 * Recursively delete files, directories, etc.
  */
  def deleteAll(f: File) {
    if ((null eq f) || !f.exists()) {} else {
      if (f.isDirectory()) {
        f.listFiles().foreach(deleteAll)
        f.delete()
      } else f.delete()
    }
  }


  /**
   * Execute some code and if it throws an exception, turn the exception
   * into a Failure and log the failure
   * @param msg the error message to help in reporting
   * @param f the code block
   * @tparam T the return type for the code block
   * @return the result of the code block or a Failure
   */
  def logFailure[T](msg: => String)(f: => T)(implicit logger: Logger): Box[T] =
    reportFailure(msg)(Helpers.tryo(f))(logger)

  /**
   * Execute some code and if it results in a Failure, log the Failure
   * @param msg the error message to help in reporting
   * @param f the code block
   * @tparam T the return type for the code block
   * @return the result of the code block or a Failure
   */
  def reportFailure[T](msg: => String)(f: => Box[T])(implicit logger: Logger): Box[T] =
  Helpers.tryo(f).flatMap(a => a) match {
    case fail: Failure =>
      boxToErrorString(fail).foreach(errMsg => logger.info(msg+": "+errMsg))
      fail
    case x => x
  }

  def prettyPrintExceptionInfo(exp: Throwable, first: Boolean = true): String = {
    if (exp eq null) "" else {
      (if (first) "" else "Enclosed ")+
    "Exception: "+exp.toString+"\n" +
    "Stack Trace: "+exp.getStackTrace.toList.take(50).map(i => "         "+i.toString).mkString("\n", "\n", "\n") +
      prettyPrintExceptionInfo(exp.getCause, false)
    }
  }

  def boxToErrorString[T](eb: Box[T]): Box[String] = {
    eb match {
      case Full(_) => Empty

      case Empty => Full("Failed with no error message")
      case ParamFailure(msg, expb, nested, code) =>
        Full((expb match {
          case Full(exp) =>
            "Error Message: "+msg+" error code "+code+"\n" + prettyPrintExceptionInfo(exp)
          case _ => msg+" error code: "+code
        })+(nested.map(f => "\nNested Failure: \n"+boxToErrorString(f)) openOr ""))

      case Failure(msg, expb, nested) =>
        Full((expb match {
          case Full(exp) =>
            "Error Message: "+msg+"\n" + prettyPrintExceptionInfo(exp)
          case _ => msg
        })+(nested.map(f => "\nNested Failure: \n"+boxToErrorString(f)) openOr ""))

    }
  }
}

object DateUtils {

  lazy val w3cDateTimeFormat = DateTimeFormat.forPattern("yyyy-MM-dd'T'HH:mm:ssZ")

  object CurrentTimeZone extends ThreadGlobal[DateTimeZone]

  object CurrentLocale extends ThreadGlobal[Locale]

  lazy val dateFormats: Stream[DateTimeFormatter] = List(
    w3cDateTimeFormat,
    DateTimeFormat.forPattern("EEE, dd MMM yyyy HH:mm:ss Z"),
    DateTimeFormat.forPattern("yyyy-MM-dd HH:mm:ss Z"),
    DateTimeFormat.forPattern("yyyy/MM/dd HH:mm:ss Z"),
    DateTimeFormat.forPattern("yyyy-MM-dd HH:mm:ss"),
    DateTimeFormat.forPattern("yyyy/MM/dd HH:mm:ss"),
    DateTimeFormat.forPattern("yyyy-MM-dd HH:mm Z"),
    DateTimeFormat.forPattern("yyyy/MM/dd HH:mm Z"),
    DateTimeFormat.forPattern("yyyy-MM-dd HH:mm"),
    DateTimeFormat.forPattern("yyyy/MM/dd HH:mm"),
    DateTimeFormat.forPattern("yyyy-MM-dd Z"),
    DateTimeFormat.forPattern("yyyy/MM/dd Z"),
    DateTimeFormat.forPattern("yyyy-MM-dd"),
    DateTimeFormat.forPattern("yyyy/MM/dd"),
    ISODateTimeFormat.basicDateTime(),
    ISODateTimeFormat.basicDate(),
    DateTimeFormat.longDateTime(),
    DateTimeFormat.fullDateTime(),
    DateTimeFormat.fullDate(),
    DateTimeFormat.longDate(),
    DateTimeFormat.mediumDateTime(),
    DateTimeFormat.mediumDate(),
    DateTimeFormat.shortDateTime(),
    DateTimeFormat.shortDate()).toStream

  /**
   * Create a function that takes a DateTimeFormatter and updates it based on the
   * locale and timezone in CurrentLocale and CurrentTimezone
   * @return
   */
  def fixDateTimeFormatter: DateTimeFormatter => DateTimeFormatter =
    (CurrentTimeZone.box, CurrentLocale.box) match {
      case (Full(tz), Full(locale)) => ((f: DateTimeFormatter) => f.withZone(tz).withLocale(locale))
      case (Full(tz), _) => ((f: DateTimeFormatter) => f.withZone(tz))
      case (_, Full(locale)) => ((f: DateTimeFormatter) => f.withLocale(locale))
      case _ => f => f
    }

  def parseDate(str: String): Box[DateTime] = {
    val mod: DateTimeFormatter => DateTimeFormatter = fixDateTimeFormatter

    dateFormats.flatMap(f => Helpers.tryo(mod(f).parseDateTime(str))).headOption
  }


  private val cache = new LRUMap[String, Box[DateTime]](15000)

  def uglyParseDate(str: String): Box[DateTime] = synchronized {
    cache.get(str) match {
      case Full(x) => x
      case _ =>
        val ret = _uglyParseDate(str)
        cache.update(str, ret)
        ret
    }
  }

  @scala.annotation.tailrec
  private def _uglyParseDate(str: String): Box[DateTime] = if (str.length < 8) Empty else {
    DateUtils.parseDate(str) match {
      case Full(d) => Full(d)
      case _ => _uglyParseDate(str.dropRight(1))
    }
  }



}
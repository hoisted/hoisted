package org.hoisted.lib

import java.io.File
import net.liftweb.common._
import net.liftweb.common.Full
import net.liftweb.util.{ThreadGlobal, Helpers}
import org.joda.time.format.{DateTimeFormatter, ISODateTimeFormat, DateTimeFormat}
import org.joda.time.{DateTime, DateTimeZone}
import java.util.Locale

/**
 * Created with IntelliJ IDEA.
 * User: dpp
 * Date: 7/23/12
 * Time: 10:30 AM
 * To change this template use File | Settings | File Templates.
 */

object Hoist {
  def main(args: Array[String]) {
    args.toList match {
      case from :: to :: Nil => RunHoisted(new File(from), new File(to))
      case _ => "Usage 'java -jar hoisted.jar source_directory destination_directory"
    }
  }
}

trait LoggerWithImplicitLogger extends Logger {
  protected implicit def __myImplicitLoggerDude: Logger = this
}

trait LazyLoggableWithImplicitLogger extends LazyLoggable {
  protected implicit def __myImplicitLoggerDude = logger
}

object HoistedUtil {
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

  def boxToErrorString[T](eb: Box[T]): Box[String] = {
    eb match {
      case Full(_) => Empty

      case Empty => Full("Failed with no error message")
      case ParamFailure(msg, expb, nested, code) =>
        Full((expb match {
          case Full(exp) =>
            "Error Message: "+msg+" error code "+code+"\n" +
            "Exception: "+exp.toString+"\n" +
            "Stack Trace: "+exp.getStackTrace.toList.take(20).map(i => "         "+i.toString).mkString("\n", "\n", "\n")
          case _ => msg+" error code: "+code
        })+(nested.map(f => "\nNested Failure: \n"+boxToErrorString(f)) openOr ""))

      case Failure(msg, expb, nested) =>
        Full((expb match {
          case Full(exp) =>
            "Error Message: "+msg+"\n" +
            "Exception: "+exp.toString+"\n" +
            "Stack Trace: "+exp.getStackTrace.toList.take(20).map(i => "         "+i.toString).mkString("\n", "\n", "\n")
          case _ => msg
        })+(nested.map(f => "\nNested Failure: \n"+boxToErrorString(f)) openOr ""))

    }
  }
}

object DateUtils {

  lazy val w3cDateTimeFormat = DateTimeFormat.forPattern("yyyy-MM-dd'T'HH:mm:ssZ")

  object CurrentTimeZone extends ThreadGlobal[DateTimeZone]

  object CurrentLocale extends ThreadGlobal[Locale]

  lazy val dateFormats = List(DateTimeFormat.forPattern("EEE, dd MMM yyyy HH:mm:ss Z"),
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


  @scala.annotation.tailrec
  def uglyParseDate(str: String): Box[DateTime] = if (str.length < 8) Empty else {
    DateUtils.parseDate(str) match {
      case Full(d) => Full(d)
      case _ => uglyParseDate(str.dropRight(1))
    }
  }
}
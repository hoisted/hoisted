package org.hoisted.lib

import java.io.File
import net.liftweb.common._
import net.liftweb.common.Full

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

object HoistedUtil {

  def boxToErrorString[T](eb: Box[T]): Box[String] = {
    eb match {
      case Full(_) => Empty

      case Empty => Full("Failed with no error message")
      case ParamFailure(msg, expb, nested, code) =>
        Full((expb match {
          case Full(exp) =>
            "Error Message: "+msg+" error code "+code+"\n" +
            "Exception: "+exp.toString+"\n" +
            "Stack Trace: "+exp.getStackTrace.toList.take(15).map(i => "         "+i.toString).mkString("\n", "\n", "\n")
          case _ => msg+" error code: "+code
        })+(nested.map(f => "\nNested Failure: \n"+boxToErrorString(f)) openOr ""))

      case Failure(msg, expb, nested) =>
        Full((expb match {
          case Full(exp) =>
            "Error Message: "+msg+"\n" +
            "Exception: "+exp.toString+"\n" +
            "Stack Trace: "+exp.getStackTrace.toList.take(15).map(i => "         "+i.toString).mkString("\n", "\n", "\n")
          case _ => msg
        })+(nested.map(f => "\nNested Failure: \n"+boxToErrorString(f)) openOr ""))

    }
  }
}
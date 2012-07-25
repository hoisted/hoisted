package org.hoisted.lib

import java.io.File

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

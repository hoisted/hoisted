package org.hoisted
package lib

import net.liftweb._
import common._
import tools.nsc.reporters.Reporter
import collection.mutable.ListBuffer
import java.util.concurrent.ConcurrentHashMap
import tools.nsc.util.{BatchSourceFile, Position}


/**
 * Created with IntelliJ IDEA.
 * User: dpp
 * Date: 10/16/12
 * Time: 1:45 PM
 *
 * A set of tools for compiling text into Scala code
 */


class CompileTool {

  import java.io.File
  import scala.tools.nsc.Global
  // import scala.reflect.internal.util.{SourceFile, Position, BatchSourceFile}
  import scala.tools.nsc.io.{VirtualDirectory, AbstractFile}
  import scala.tools.nsc.Settings
  import scala.tools.nsc.reporters.ConsoleReporter


  def compiler(vd: AbstractFile, reporter: Reporter) = {
    val settings = new Settings

    settings.outputDirs.setSingleOutput(vd)

    val wheresScala = Class.forName("scala.ScalaObject").getProtectionDomain.getCodeSource
    val wheresCompiler = Class.forName("scala.tools.nsc.Interpreter").getProtectionDomain.
      getCodeSource.getLocation.getFile
    val wheresLibs = wheresScala.getLocation.getFile

    val path = List(wheresCompiler, wheresLibs)
    val settingsClasspath = settings.bootclasspath.value
    settings.bootclasspath.value = (settingsClasspath :: path).mkString(File.pathSeparator)


    val compiler = new Global(settings, reporter)

    compiler
  }


  private def fixPath(str: String): String = {
    val one = str.substring("(memory)/".length)
    val two = one.substring(0, one.length - 6)
    two.replace('/', '.')
  }

  def fileToBytes(in: AbstractFile): List[(String, Array[Byte])] =
    if (in.isDirectory) in.iterator.toList.flatMap(fileToBytes(_))
    else List(fixPath(in.path) -> in.toByteArray)

  def classloaderFor(src: Seq[(String, String)], oldClasses: Map[String, Array[Byte]] = Map.empty): Box[ClassLoader] = {
    for {
      newMap <- compile(src)
      map = newMap.toList.foldLeft(oldClasses){case (mip, nb) => mip + nb }
      masterCL = this.getClass.getClassLoader
    } yield new ClassLoader(masterCL) {
      private val classes: ConcurrentHashMap[String, Class[_]] = new ConcurrentHashMap()

      override def loadClass(className: String): Class[_] = {
        map.get(className) match {
          case Some(_) => findClass(className)
          case _ => masterCL.loadClass(className)
        }
      }

      override def findClass(className: String): Class[_] = {
        classes.get(className) match {
          case null =>
            try {
              val ret = findSystemClass(className)
              ret
            } catch {
              case e: Exception =>
                map.get(className) match {
                  case Some(bytes) =>
                    val ret = defineClass(className, bytes, 0, bytes.length, null)
                    classes.put(className, ret)
                    ret
                  case _ => null

                }
            }
          case c => c
        }

      }
    }
  }

  def compile(src: Seq[(String, String)]): Box[Map[String, Array[Byte]]] = {
    val virtualDirectory = new VirtualDirectory("(memory)", None)

    val errors: ListBuffer[(Position, String)] = new ListBuffer[(Position, String)]

    val reporter = new Reporter {
      protected def info0(pos: Position, msg: String, severity: this.type#Severity, force: Boolean) {
        severity match {
          case this.ERROR => errors.append(pos -> msg)
          case _ =>
        }
      }
    }


    val comp = compiler(virtualDirectory, reporter)

    val files = src.toList.map {
      case (name, code) => new BatchSourceFile(name, code)
    }

    val run = new comp.Run
    run.compileSources(files)

    errors.toList match {
      case Nil => Full(Map(fileToBytes(virtualDirectory): _*))
      case errs => ParamFailure(errs.mkString((", ")), errs)
    }
  }


}
package org.hoisted
package lib

import net.liftweb._
import common._
import tools.nsc.reporters.Reporter
import collection.mutable.ListBuffer
import java.util.concurrent.ConcurrentHashMap
import scala.reflect.internal.util.BatchSourceFile
import scala.reflect.internal.util.Position
import tools.nsc.io.VirtualFile
import util.Helpers
import java.net.URLClassLoader


/**
 * Created with IntelliJ IDEA.
 * User: dpp
 * Date: 10/16/12
 * Time: 1:45 PM
 *
 * A set of tools for compiling text into Scala code
 */


class CompileTool extends LazyLoggableWithImplicitLogger {

  import java.io.File
  import scala.tools.nsc.Global
  // import scala.reflect.internal.util.{SourceFile, Position, BatchSourceFile}
  import scala.tools.nsc.io.{VirtualDirectory, AbstractFile}
  import scala.tools.nsc.Settings
  import scala.tools.nsc.reporters.ConsoleReporter


  def compiler(vd: AbstractFile, reporter: Reporter) = {
    val settings = new Settings

    settings.outputDirs.setSingleOutput(vd)

    val classes = List("scala.ScalaObject", "scala.tools.nsc.Interpreter",
      "org.hoisted.lib.CompileTool", "net.liftweb.common.Box",
      "net.liftweb.util.Helpers", "net.liftweb.http.S", "net.liftweb.actor.LiftActor")

    val path = (classes.map(cn => Class.forName(cn).getProtectionDomain.getCodeSource.getLocation.getFile) :::
      (this.getClass.getClassLoader match {
        case cl: URLClassLoader => cl.getURLs.toList.map(_.getFile)
        case _ => Nil
      })).distinct

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

  def classloaderFor(src: Seq[(String, String)], oldClasses: Map[String, Array[Byte]] = Map.empty): Box[(ClassLoader, Set[String])] = {
    for {
      newMap <- compile(src)
      map = newMap.toList.foldLeft(oldClasses){case (mip, nb) => mip + nb }
      masterCL = this.getClass.getClassLoader
    } yield (new ClassLoader(masterCL) {
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
    }, map.keySet)
  }

  def compile(src: Seq[(String, String)]): Box[Map[String, Array[Byte]]] = {
    val virtualDirectory = new VirtualDirectory("(memory)", None)

    val errors: ListBuffer[(Position, String)] = new ListBuffer[(Position, String)]

    val reporter = new Reporter {
      protected def info0(pos: Position, msg: String, severity: this.type#Severity, force: Boolean) {
        severity match {
          case this.ERROR => errors.append(pos -> msg)
            logger.error(pos+": "+msg)
          case _ =>
        }
      }
    }


    val comp = compiler(virtualDirectory, reporter)

    val files = src.toList.map {
      case (name, code) => new BatchSourceFile(new VirtualFile(name) {
        override def container: AbstractFile = this
      }, code.toArray)
    }

    // this(new VirtualFile(sourceName), cs.toArray)

    val run = new comp.Run
    val runRes = Helpers.tryo(run.compileSources(files))

    errors.toList match {
      case Nil => Full(Map(fileToBytes(virtualDirectory): _*))
      case errs => ParamFailure(errs.mkString((", ")), errs)
    }
  }


}
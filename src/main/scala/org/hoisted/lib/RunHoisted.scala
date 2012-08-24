package org.hoisted
package lib

import net.liftweb._
import common._
import common.Full
import http._
import http.LiftRules.{SnippetFailures, SnippetFailure}
import util._
import Helpers._
import java.util.Locale
import java.io._
import xml._

object VeryTesty {


  def apply() = {

    DateUtils.CurrentLocale.doWith(Locale.FRANCE) {
    RunHoisted(new File("/Users/dpp/proj/dpp-blog"),
      new File("/Users/dpp/tmp/outfrog")).map(_.logs)
    }
  }
}

/**
 * This singleton will take a directory, find all the files in the directory
 * and then generate a static site in the output directory and return metadata about
 * the transformation
 */

object RunHoisted extends HoistedRenderer

object CurrentFile extends ThreadGlobal[ParsedFile]

object PostPageTransforms extends TransientRequestVar[Vector[NodeSeq => NodeSeq]](Vector())

trait HoistedRenderer extends LazyLoggableWithImplicitLogger {
  @scala.annotation.tailrec
  private def seekInDir(in: File): File = {
    if (!in.exists()) in
    else {
      def filesAsNonNullList(in: File): List[File] = {
        if (null eq in) Nil
        else if (!in.isDirectory) Nil
        else {
          val fa = in.listFiles()
          if (null eq fa) Nil
          else fa.toList.filter(v => null ne v).filter(f => null ne f.getName)
        }
      }

      val all = filesAsNonNullList(in).
        filter(!_.getName.startsWith(".")).filterNot(_.getName.toLowerCase.startsWith("readme"))

      all match {
        case Nil => in
        case x :: Nil if !x.isDirectory => x
        case x :: y :: _ => in
        case x :: _ => seekInDir(x)
      }
    }
  }

  /**
   * Collect the global metadata from the incoming files, then
   * update the metadata for each file.  Then pull any externally referenced files
   * and parse them and apply metadata.
   *
   * @param _in
   * @return
   */
  def doMetadataMagicAndSuch(_in: List[ParsedFile]): Box[List[ParsedFile]] = {
    _in.foreach(pf => env.updateGlobalMetadata(pf.metaData))

    val in = _in.map(env.transformFile)

    val withLoadedTemplates =
      env.findMetadata(ExternalLinkKey) match {
        case Full(ListMetadataValue(lst)) => lst.foldLeft(in)(loadExternal)
        case Full(md) => loadExternal(in, md)
        case _ => in
      }

    Full(withLoadedTemplates)
  }

  private def dedupNames(first: List[ParsedFile], second: List[ParsedFile]): List[ParsedFile] = {
    val curSet = Set(first.map(_.fileInfo.pathAndSuffix.path): _*)

    def fixConflict(in: ParsedFile): ParsedFile =
      if (!curSet.contains(in.fileInfo.pathAndSuffix.path)) in
      else {
        val old = in.fileInfo.pathAndSuffix
        val newer = old.copy(path = old.path.dropRight(1) ::: old.path.takeRight(1).map(_ + "_dup"))
        val fixed = in.updateFileInfo(newer.toFileInfo(in.fileInfo.file))
        fixConflict(fixed)
      }

    second.map(fixConflict _)
  }

  private def dedupPaths(first: List[ParsedFile], second: List[ParsedFile]): List[ParsedFile] = {
    val curSet = Set(first.flatMap(_.findData(OutputPathKey).flatMap(_.asString)): _*)

    def fixConflict(in: ParsedFile): ParsedFile = {
      in.findData(OutputPathKey).flatMap(_.asString) match {
        case e: EmptyBox => in
        case Full(str) if !curSet.contains(str) => in
        case Full(str) =>
          fixConflict(in.updateMetadata(OutputPathKey, str + "_dup"))
      }
    }

    second.map(fixConflict _)
  }

  def loadExternal(cur: List[ParsedFile], info: MetadataValue): List[ParsedFile] = {
    info match {
      case k: KeyedMetadataValue =>

        (HoistedUtil.reportFailure("Trying to fetch external resource for " + k)(for {
          url <- k.findString(UrlKey) ?~ ("Failed to get URL for external link in " + k)

          first_prime <- env.loadTemplates(url, Nil, false)
          first = dedupNames(cur, first_prime)
          xform = env.metadataTransformRules ::: Transformer.listFromMetadata(k)
          tests = TransformTest.fromMetadata(k)
          xformed = first.map(f => xform.foldLeft(f)((pf, func) => func(pf)))
          filtered = env.removeRemoved(xformed.filter(tests))
          filtered_2 = dedupPaths(cur, filtered)
        } yield env.mergeTemplateSets(cur, filtered_2, true))) openOr cur
      case md =>
        (HoistedUtil.reportFailure("Trying to fetch external resource for " + md)(for {
          url <- md.asString ?~ ("Couldn't turn " + md + " into a URL")
          first_prime <- env.loadTemplates(url, Nil, true)
          first = dedupNames(cur, first_prime)
          xform = env.metadataTransformRules
          xformed = first.map(f => xform.foldLeft(f)((pf, func) => func(pf)))
          filtered = env.removeRemoved(xformed)
          filtered_2 = dedupPaths(cur, filtered)
        } yield env.mergeTemplateSets(cur, filtered_2, true))) openOr cur
    }
  }

  /**
   * Do an initial pass on the files to include other files
   * @param in
   * @return
   */
  def doInitialTemplating(in: List[ParsedFile]): Box[List[ParsedFile]] = {
    env.allPages = in
    env.pages = in.filter(env.isValid)

    val templates = createTemplateLookup(env.pages)
    Full(in.map {
      case f if env.isHtml(f) && env.shouldWriteFile(f) =>
        runTemplater(f, templates, true, env.earlySnippets)
      case other => other
    })
  }

  def updateHeaderMetadata(in: List[ParsedFile]): Box[List[ParsedFile]] = {
    Full(in.map {
      case h: HasHtml =>
        val (html, metadata) = ParsedFile.findHeaders(h.html)
        h.updateHtml(html).updateMetadata(h.metaData +&+ metadata)
      case x => x
    })
  }

  private def avoidScalaNamingHell(__parsedFiles_2: List[ParsedFile],
                                   log: ByteArrayOutputStream,
                                   inDir: File, outDir: File): Box[HoistedTransformMetaData] = {

    for {
      __parsedFiles_3 <- updateHeaderMetadata(env.removeRemoved(__parsedFiles_2))
      __parsedFiles = env.removeRemoved(__parsedFiles_3)
      _ = env.allPages = __parsedFiles
      _parsedFiles = __parsedFiles.filter(env.isValid)
      parsedFilesPrime <- ensureTemplates(_parsedFiles)

      _ = {
        if (env.hasBlogPosts(parsedFilesPrime)) {
          env.setMetadata(HasBlogKey, BooleanMetadataValue(true))
        }
      }

      parsedFiles = env.filterBasedOnMetadata(parsedFilesPrime)

      _ = HoistedEnvironmentManager.value.pages = parsedFiles

      fileMap = byName(parsedFiles)
      templates = createTemplateLookup(parsedFiles)
      menu = env.computeMenuItems(parsedFiles)
      _ = env.menuEntries = menu

      transformedFiles = (env.syntheticFiles(parsedFiles).toList ::: parsedFiles).map(f =>
        runTemplater(f, templates, false, env.snippets))

      aliases = {
        val ret = transformedFiles.flatMap(pf =>
          pf.findData(AliasKey).toList.flatMap(_.forceListString).map(a => Alias(a, env.computeOutputFileName(pf)))
        ).toList

        ret
      }

      done <- HoistedUtil.logFailure("Writing rendered files")(writeFiles(transformedFiles, inDir, outDir))
      _ = HoistedUtil.logFailure("Post run")(env.runPostRun(outDir))
    } yield HoistedTransformMetaData(new String(log.toByteArray), transformedFiles, env.metadata, env, aliases)
  }

  def apply(_inDir: File, outDir: File, environment: EnvironmentManager = new EnvironmentManager): Box[HoistedTransformMetaData] = {
    environment.runWrapper {
      val special = environment.additionalKeys.flatMap(k => (k.key :: k.alt).map(_.trim.toLowerCase).map(_ -> k)).
        foldLeft(MetadataKey.special)(_ + _)
      MetadataKey.localSpecial.doWith(special) {
        if ((null eq _inDir) || !_inDir.exists()) Failure("No valid source directory " + _inDir)
        else {

          val log = new ByteArrayOutputStream()
          Logstream.doWith(log) {
            HoistedEnvironmentManager.doWith(environment) {
              val __inDir = seekInDir(_inDir)
              for {
                deleteAll <- HoistedUtil.logFailure("Deleting all files in " + outDir)(HoistedUtil.deleteAll(outDir))
                theDir <- HoistedUtil.logFailure("Making dir " + outDir)(outDir.mkdirs())
                inDir <- Full(__inDir).filter(_.exists()) ?~ "Failed to get source repository"
                orgFiles <- HoistedUtil.reportFailure("Loading files from " + inDir)(env.loadFilesFrom(inDir))
                __parsedFiles_1 <- doMetadataMagicAndSuch(env.removeRemoved(orgFiles))
                __parsedFiles_2 <- doInitialTemplating(env.removeRemoved(__parsedFiles_1))
                res <- avoidScalaNamingHell(__parsedFiles_2, log, inDir, outDir)
              } yield res
            }
          }
        }
      }
    }
  }


  def ensureTemplates(in: List[ParsedFile]): Box[List[ParsedFile]] =
    if (env.needsTemplates(in)) {
      val name = env.computeTemplateURL()
      env.loadTemplates(name, in, false)
    } else Full(in)

  def dropSuffix(in: String): String = {
    if (in.toLowerCase.endsWith(".cms.xml")) {
      in.substring(0, in.length - 8)
    } else in.lastIndexOf(".") match {
      case x if x < 0 => in
      case x => in.substring(0, x)
    }
  }

  def captureSuffix(in: String): String = {
    if (in.toLowerCase.endsWith(".cms.xml")) {
      "cms.xml"
    } else in.lastIndexOf(".") match {
      case x if x < 0 => ""
      case x => in.substring(x + 1)
    }
  }


  def writeFiles(toWrite: Seq[ParsedFile], inDir: File, outDir: File): Unit = {
    def translate(source: String): File = {
      new File(outDir.getAbsolutePath + source)
    }

    def calcFile(pf: ParsedFile): File = {
      val ret = translate(env.computeOutputFileName(pf))
      ret
    }

    toWrite.foreach {
      pf =>
        if (env.shouldWriteFile(pf)) {
          val where: File = calcFile(pf)
          where.getParentFile.mkdirs()
          val out = new FileOutputStream(where)
          try {
            pf.writeTo(out)
          } finally {
            HoistedUtil.logFailure("Trying to flush " + pf.pathAndSuffix)(out.flush())
            HoistedUtil.logFailure("Trying to close " + pf.pathAndSuffix)(out.close())
          }
          // where.setLastModified(env.computeDate(pf).getMillis)
        }
    }
  }

  type TemplateLookup = PartialFunction[(List[String], String), ParsedFile]

  def createTemplateLookup(in: Seq[ParsedFile]): TemplateLookup = {
    def makeName(f: ParsedFile): (List[String], String) = {
      f match {
        case h: HasHtml => (dropSuffix(f.fileInfo.relPath).roboSplit("/"), "html")
        case f => (dropSuffix(f.fileInfo.relPath).roboSplit("/"), captureSuffix(f.fileInfo.relPath))
      }
    }
    Map(in.map(f => (makeName(f), f)): _*)
  }

  def env = HoistedEnvironmentManager.value

  def runTemplater(_f: ParsedFile, templates: TemplateLookup, ignoreTemplateFailure: Boolean,
                   snippets: PartialFunction[(String, String), Box[NodeSeq => NodeSeq]]): ParsedFile = {
    _f match {
      case f: ParsedFile with HasHtml if HoistedEnvironmentManager.value.isHtml(f) =>

        val lu = new PartialFunction[(Locale, List[String]), Box[NodeSeq]] {
          def isDefinedAt(in: (Locale, List[String])): Boolean = {

            true
          }

          def apply(in: (Locale, List[String])): Box[NodeSeq] = {
            lazy val html = if (templates.isDefinedAt((in._2, "html"))) {
              val ret = templates((in._2, "html"))
              ret match {
                case h: HasHtml => Full(h.html)
                case _ => Empty
              }
            } else {
              Empty
            }

            lazy val markdown =
              if (templates.isDefinedAt((in._2, "md"))) {
                val ret = templates((in._2, "md"))
                ret match {
                  case h: HasHtml => Full(h.html)
                  case _ => Empty
                }
              } else {
                Empty
              }

            lazy val xml =
              if (templates.isDefinedAt((in._2, "xml"))) {
                val ret = templates((in._2, "xml"))
                ret match {
                  case h: HasHtml => Full(h.html)
                  case _ => Empty
                }
              } else {
                Empty
              }

            lazy val xml_cms =
              if (templates.isDefinedAt((in._2, "cms.xml"))) {
                val ret = templates((in._2, "cms.xml"))
                ret match {
                  case h: HasHtml if HoistedEnvironmentManager.value.isHtml(ret) => Full(h.html)
                  case _ => Empty
                }
              } else {
                Empty
              }

            html or markdown or xml or xml_cms
          }
        }

        val session = new LiftSession("", Helpers.nextFuncName, Empty) with StatelessSession {
          override def stateful_? = false
        }

        def insureChrome(todo: ParsedFile, node: NodeSeq): NodeSeq = {
          if (ignoreTemplateFailure) {
            node
          } else {
            val _processed = if ((node \\ "html" \\ "body").length > 0) node
            else {
              val templateName = env.chooseTemplateName(todo)
              val res = session.processSurroundAndInclude("Surrounding page " + todo.fileInfo.pathAndSuffix + " with template: " + templateName, <lift:surround with={templateName} at="content">
                {node}
              </lift:surround>)
              res
            }

            val _processed1 = PostPageTransforms.get.foldLeft(_processed)((ns, f) => f(ns))
            val processed = session.processSurroundAndInclude("Post transforms for " + todo.fileInfo.pathAndSuffix, env.computeTransforms(todo).foldLeft(_processed1)((ns, f) => f(ns)))

            session.processSurroundAndInclude("Post merge transforms for " + todo.fileInfo.pathAndSuffix,
              env.computePostMergeTransforms(todo).foldLeft[NodeSeq](session.merge(processed, Req.nil))((ns, f) => f(ns)))
          }
        }

        def snippetFailure(in: SnippetFailure) {
          import SnippetFailures._

          in match {
            case SnippetFailure(page, Full(snippet), MethodNotFound) =>
              logger.error("Trying to execute snippet " + snippet + " but could not find the method on the snippet instance object")
            case SnippetFailure(page, Full(snippet), ExecutionFailure) => logger.error("Failure while executing snippet " + snippet)
            case SnippetFailure(page, Full(snippet), InstantiationException) =>
              logger.error("Trying to instantiate class the provides snippet " + snippet + " but failed")

            case SnippetFailure(page, Full(snippet), ClassNotFound) =>
              logger.error("Could not find any providers for the snippet named '" + snippet + "'.  Perhaps you mis-typed the name of the snippet in the data-lift='" + snippet + "' attribute.")
            case _ => logger.info("Snippet Failure: " + in)
          }
        }

        MDC.clear()
        S.initIfUninitted(session) {
          S.runSnippetsWithIgnoreFailed(ignoreTemplateFailure) {
            LiftRules.snippetFailedFunc.prependWith(snippetFailure _) {
              LiftRules.autoIncludeAjaxCalc.doWith(() => ignore => false) {
                LiftRules.allowParallelSnippets.doWith(() => false) {
                  LiftRules.allowAttributeSnippets.doWith(() => false) {
                    LiftRules.snippetWhiteList.doWith(() => snippets) {
                      LiftRules.externalTemplateResolver.doWith(() => () => lu) {
                        CurrentFile.doWith(f) {
                          MDC.put("file_name" -> f.pathAndSuffix.display)
                          env.beginRendering(f)
                          try {
                            f match {
                              case todo: ParsedFile with HasHtml if HoistedEnvironmentManager.value.isHtml(todo) =>
                                val revised: NodeSeq = insureChrome(todo,
                                  session.processSurroundAndInclude(todo.pathAndSuffix.display, todo.html))

                                todo.updateHtml(revised)
                              case d => d
                            }
                          } finally {
                            env.endRendering(f)
                          }
                        }
                      }
                    }
                  }
                }
              }
            }
          }
        }
      case ret => ret
    }
  }

  def byName(in: Seq[ParsedFile]): Map[String, List[ParsedFile]] = {
    in.foldLeft[Map[String, List[ParsedFile]]](Map.empty) {
      (m, f) =>
        val name = f.fileInfo.name

        m + (name -> (f :: m.getOrElse(name, Nil)))
    }
  }

  def byPureName(in: Seq[ParsedFile]): Map[String, List[ParsedFile]] = {
    in.foldLeft[Map[String, List[ParsedFile]]](Map.empty) {
      (m, f) =>
        val name = f.fileInfo.pureName

        m + (name -> (f :: m.getOrElse(name, Nil)))
    }
  }


}

final case class HoistedTransformMetaData(logs: String, files: Seq[ParsedFile],
                                          globalMetadata: MetadataValue,
                                          env: EnvironmentManager,
                                          aliases: List[Alias])



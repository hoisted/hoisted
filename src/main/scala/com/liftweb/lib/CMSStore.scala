package com.liftweb
package lib

import net.liftweb._
import common._
import Box._
import util._
import Helpers._
import java.io.{File, FileInputStream}
import java.util.Locale
import scala.xml.{NodeSeq, Elem}

object CMSStore {
  type Key = CMS.Key
  type Record = CMS.Record

  def find(in: Key): Box[FileRecord] = {
    for {
      possible <- pages.get(in)
      it <- possible.find(a => true)
    } yield {
      it
    }
  }

  def _defaultHost = "liftweb.net"
  
  def baseDir = {
    val ret = new File(Props.get("cms.root").open_!)
    ret
  }

  /**
   * What's the name of the default host (the Lift CMS system)
   * supports multiple hosts
   */
  def defaultHost: Host = Host(_defaultHost)

  @volatile
  private var pages: Map[Key, List[FileRecord]] = Map()

  @volatile
  private var changeDates: Map[String, (Long, Key)] = Map()
  
  /**
   * Scan the files at the basedir
   */
  private def scanFiles() {
    val (unchanged, changed)= changeDates.partition
    {
      case (path, (changeTime, key)) =>
        val f = new File(path)
        f.exists && f.lastModified() == changeTime
    }

    val unchangedPages = changed.foldLeft(pages){
      case (p, (path, (changeTime, key))) => {
        p.getOrElse(key, Nil).filter(_.fromFile != path) match {
          case Nil => p - key
          case xs => p + (key -> xs)
        }
      }
    }

    def filesFor(root: File): Stream[File] = {
      if (root.isDirectory) 
        Stream(root.listFiles :_*).flatMap(filesFor)
      else if (root.isFile && root.getName.endsWith(".cms.xml")) {
        Stream(root)
      }
      else Stream.Empty
    }

    val filesToCheck = filesFor(baseDir).
    filter{
      f => 
        val cp = f.getCanonicalPath
        !changeDates.contains(cp) || changed.contains(cp)
    }.
    flatMap(parseFile).toList

    filesToCheck.foldLeft(unchanged -> unchangedPages) {
      case ((cd, pa), (f, key, record)) =>
        (cd + (f.getCanonicalPath -> (f.lastModified() -> key))) -> 
      (pa + (key -> (record :: pa.getOrElse(key, Nil))))
    } match {
      case (cd, pa) => 
        pages = pa
        changeDates = cd
    }

    ActorPing.schedule(() => scanFiles(), 
                       if (Props.productionMode) 60 seconds else
                         1 seconds)
  }

  ActorPing.schedule(() => scanFiles(), 0 seconds)


  private def parseFile(f: File): Box[(File, Key, FileRecord)] = {
    def parseKeyRecord(in2: NodeSeq): Box[(Key, FileRecord)] = {
      val in: NodeSeq = in2(0)
      def parseContent(): Box[Content] = (in \ "@type").headOption.
      map(_.text.toLowerCase) match {
        case Some("css") => {
          (in \ "content").headOption.map(n => CSSContent(n.text))
        }

        case Some("js") => {
          (in \ "content").headOption.map(n => JavaScriptContent(n.text))
        }

        case _ => {
          val ret = 
            (for {
              content <- (in \ "content").toStream
              kid <- content.child
            } yield kid).collect {
              case e: Elem => e
            }.headOption.map(HtmlContent(_, true))
          ret
        }
      }

      for {
        pathStr <- Box((in \ "@path").headOption)
        content <- parseContent()
      } yield {
        val path = pathStr.text.roboSplit("/")
        val host = ((in \ "@host").headOption.map(_.text)) getOrElse defaultHost.host

        val tags: List[TagInfo] = (in \ "tag").toList.flatMap {
          node =>
          for {
            name <- (node \ "@name")
          } yield TagInfo(name.text, 
                          (node \ "@value").headOption.map(_.text),
                          Full(node.child: NodeSeq).filter(_.size > 0))
            
        }

        val locale = (
          for {
            l <- (in \ "@locale")
            lo <- Locale.getAvailableLocales if lo.toString == l
          } yield lo).headOption getOrElse defaultLocale

        val validFrom: Box[CMSDate] = 
          for {
            attr <- (in \ "@valid_from").headOption
            date <- Helpers.toDate(attr)
          } yield CMSDate.fromJavaDate(date)

        val validTo: Box[CMSDate] = 
          for {
            attr <- (in \ "@valid_to").headOption
            date <- Helpers.toDate(attr)
          } yield CMSDate.fromJavaDate(date)
        
        CMSKey(Host(host), Path(path), locale) -> 
        FileRecord(host,
                   path,
                   locale,
                   f.getCanonicalPath(),
                   tags = tags,
                   validFrom = validFrom,
                   validTo = validTo,
                   changeDate = f.lastModified(),
                   content = content)
      }
    }
    
    for {
      fis <- tryo(new FileInputStream(f))
      xml <- PCDataXmlParser(fis)
      (key, record) <- parseKeyRecord(xml)
    } yield {
      (f, key, record)
    }
  }

  def findByTag(tag: String, value: Box[String]): Stream[FileRecord] = {
    pages.values.toStream.flatten.filter {
      _.findTag(tag) match {
        case Full(_) if value.isEmpty => true
        case Full(TagInfo(_, testValue, _)) => testValue == value
        case _ => false
      }
    }
  }

  /**
   * Get a record from backing store
   */
  // def getRecord(key: Key): Box[Record] = Empty

  /**
   * What's the default locale
   */
  lazy val defaultLocale = Locale.getDefault

  def localeFor(in: String): Box[Locale] = 
    Locale.getAvailableLocales().filter(_.toString == in).headOption



}

final case class FileRecord(host: String,
                            path: List[String], locale: Locale,
                            fromFile: String,
                            tags: List[TagInfo] = Nil, 
                            validFrom: Box[CMSDate],
                            validTo: Box[CMSDate],
                            changeDate: CMSDate,
                            content: Content) {
  def findTag(name: String): Box[TagInfo] = tags.find(_.name == name)
  def currentlyValid_? =  {
    val n = Helpers.millis

    (validFrom.map(_.millis <= n) openOr true) &&
    (validTo.map(_.millis >= n) openOr true)
  }

  lazy val redirectTo = findTag("redirect").flatMap(_.value)
  lazy val htmlContent_? = content match {
    case c: HtmlContent => true
    case _ => false
  }
}

final case class TagInfo(name: String, value: Box[String], attr: Box[NodeSeq])

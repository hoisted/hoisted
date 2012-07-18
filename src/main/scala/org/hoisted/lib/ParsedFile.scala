package org.hoisted.lib

import net.liftweb._
import common._
import common.Full
import util._
import Helpers._
import scala.xml._
import java.io._
import org.joda.time.{DateTimeZone, DateTime}
import org.joda.time.format.{DateTimeFormatter, ISODateTimeFormat, DateTimeFormat}
import collection.mutable.ListBuffer

/**
 * Created with IntelliJ IDEA.
 * User: dpp
 * Date: 6/8/12
 * Time: 12:02 PM
 * To change this template use File | Settings | File Templates.
 */

final case class PathAndSuffix(path: List[String], suffix: Option[String]) {
  def display: String = path.mkString("/", "/", "") + (suffix.map(s => "." + s) getOrElse "")
}

object ParsedFile {

  lazy val w3cDateTimeFormat = DateTimeFormat.forPattern("yyyy-MM-dd'T'HH:mm:ssZ")

  object CurrentTimeZone extends ThreadGlobal[DateTimeZone]

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

  def parseDate(str: String): Box[DateTime] = {
    val mod: DateTimeFormatter => DateTimeFormatter = CurrentTimeZone.box match {
      case Full(tz) => ((f: DateTimeFormatter) => f.withZone(tz))
      case _ => f => f
    }
    dateFormats.flatMap(f => tryo(mod(f).parseDateTime(str))).headOption
  }

  @scala.annotation.tailrec
  def uglyParseDate(str: String): Box[DateTime] = if (str.length < 8) Empty else {
    parseDate(str) match {
      case Full(d) => Full(d)
      case _ => uglyParseDate(str.dropRight(1))
    }
  }


  def findBody(in: NodeSeq): NodeSeq =
    (in \ "content").headOption.map(_.child) getOrElse in


  /*


    HWPFDocumentCore wordDocument = WordToHtmlUtils.loadDoc(new FileInputStream(file));

    WordToHtmlConverter wordToHtmlConverter = new WordToHtmlConverter(
            DocumentBuilderFactory.newInstance().newDocumentBuilder()
                    .newDocument());
    wordToHtmlConverter.processDocument(wordDocument);
    Document htmlDocument = wordToHtmlConverter.getDocument();
    ByteArrayOutputStream out = new ByteArrayOutputStream();
    DOMSource domSource = new DOMSource(htmlDocument);
    StreamResult streamResult = new StreamResult(out);

    TransformerFactory tf = TransformerFactory.newInstance();
    Transformer serializer = tf.newTransformer();
    serializer.setOutputProperty(OutputKeys.ENCODING, "UTF-8");
    serializer.setOutputProperty(OutputKeys.INDENT, "yes");
    serializer.setOutputProperty(OutputKeys.METHOD, "html");
    serializer.transform(domSource, streamResult);
    out.close();


 */

  def apply(fi: FileInfo): Box[ParsedFile] = {
    fi.suffix.map(_.toLowerCase) match {
        /*
      case Some("doc") | Some("docx") | Some("rtf") | Some("pages") =>
        (for {
          realFile <- fi.file
          inputStream <- tryo(new FileInputStream(realFile))
          out = new ByteArrayOutputStream()
          handler = new ToHTMLContentHandler(out, "UTF-8")
          metadata = new Metadata()
          myParser = new AutoDetectParser()
          thing <- tryo(try {myParser.parse(inputStream, handler, metadata)} finally {tryo(inputStream.close)})
          str = new String(out.toByteArray, "UTF-8")
          html <- parseHtml5File(str)

        } yield {
          val h2: NodeSeq = (html \ "body").toList.collect{
            case e: Elem => e
          }.flatMap(_.child)

          val md =
          metadata.names().foldLeft[MetadataMeta.Metadata](Map.empty){(map, key) =>
            val k2 = MetadataKey(key)
            metadata.getValues(key).toList match {
            case Nil => map
            case x::Nil => map + (k2 -> MetadataValue(x))
            case xs => map + (k2 -> ListMetadataValue(xs.map(MetadataValue(_))))
          }}

          HtmlFile(fi, h2, HoistedEnvironmentManager.value.updateMetadata(md, fi))
        }) or Full(OtherFile(fi))

        /*

scala> MD is Map(StringMetadataKey(line-count) -> StringMetadataValue(1), StringMetadataKey(creation-date) -> StringMetadataValue(2012-07-08T00:09:00Z),
 StringMetadataKey(last-author) -> StringMetadataValue(David Pollak), StringMetadataKey(page-count) -> StringMetadataValue(1),
 StringMetadataKey(revision-number) -> StringMetadataValue(1), StringMetadataKey(last-modified) -> StringMetadataValue(2012-07-08T17:38:00Z),
  StringMetadataKey(content-type) -> StringMetadataValue(application/vnd.openxmlformats-officedocument.wordprocessingml.document),
   StringMetadataKey(xmptpg:npages) -> StringMetadataValue(1), StringMetadataKey(paragraph-count) -> StringMetadataValue(1),
   StringMetadataKey(application-name) -> StringMetadataValue(Microsoft Macintosh Word),
   StringMetadataKey(application-version) -> StringMetadataValue(12.0000), DateKey -> StringMetadataValue(2012-07-08T00:09:00Z),
   StringMetadataKey(total-time) -> StringMetadataValue(16), TemplateKey -> StringMetadataValue(Normal.dotm))
MD is Map(StringMetadataKey(creation-date) -> StringMetadataValue(2012-07-08T18:56:00Z), StringMetadataKey(last-author) -> StringMetadataValue(David Pollak),
 StringMetadataKey(word-count) -> StringMetadataValue(41), StringMetadataKey(page-count) -> StringMetadataValue(1),
  StringMetadataKey(revision-number) -> StringMetadataValue(2), StringMetadataKey(content-type) -> StringMetadataValue(application/msword),
  StringMetadataKey(last-save-date) -> StringMetadataValue(2012-07-08T18:56:00Z), StringMetadataKey(subject) -> StringMetadataValue(),
   StringMetadataKey(xmptpg:npages) -> StringMetadataValue(1), TitleKey -> StringMetadataValue(),
    StringMetadataKey(application-name) -> StringMetadataValue(Microsoft Macintosh Word), StringMetadataKey(keywords) -> StringMetadataValue(),
    StringMetadataKey(last-printed) -> StringMetadataValue(2012-07-08T18:55:00Z), StringMetadataKey(character count) -> StringMetadataValue(239),
     TemplateKey -> StringMetadataValue(Normal.dotm), AuthorKey -> StringMetadataValue(David Pollak))
MD is Map(AuthorKey -> StringMetadataValue(David Pollak), StringMetadataKey(content-type) -> ListMetadataValue(List(StringMetadataValue(application/rtf), StringMetadataValue(application/rtf))))
SLF4J: Failed to load class "org.slf4j.impl.StaticLoggerBinder".
SLF4J: Defaulting to no-operation (NOP) logger implementation
SLF4J: See http://www.slf4j.org/codes.html#StaticLoggerBinder for further details.
res0: net.liftweb.common.Box[org.hoisted.lib.HoistedTransformMetaData] = Full(HoistedTransformMetaData())
         */

*/
      case Some("xml") | Some("cms.xml") =>
        for {
          realFile <- fi.file
          fis <- tryo(new FileInputStream(realFile))
          xml <- PCDataXmlParser(fis)
          _ <- tryo(fis.close())
          metaData = findXmlMetaData(xml)
          (realBody, headMeta) = findHeaders(findBody(xml))
        } yield XmlFile(fi, realBody, xml,
          HoistedEnvironmentManager.value.updateMetadata(metaData ++ headMeta, fi))

      case Some("html") | Some("htm") =>
        for {
          realFile <- fi.file
          fis <- tryo(new FileInputStream(realFile))
          bytes <- tryo(Helpers.readWholeStream(fis))
          str = new String(bytes, "UTF-8")
          (str2, info) = MarkdownParser.readTopMetadata(str)
          html <- parseHtml5File(str2)
          _ <- tryo(fis.close())
          (_html, metaData) = findHtmlMetaData(html)
          (__html, headMeta) = findHeaders(_html)
        } yield HtmlFile(fi, __html,
        HoistedEnvironmentManager.value.updateMetadata(
          mergeMetadata(pairsToMetadata(info), metaData ++ headMeta), fi))

      case Some("md") =>
        for {
          realFile <- fi.file
          whole <- tryo(Helpers.readWholeFile(realFile))
          str = new String(whole, "UTF-8")
          (elems, rawMeta) <- MarkdownParser.parse(str)
          (html, headMeta) = findHeaders(elems)
        } yield MarkdownFile(fi, html,
          HoistedEnvironmentManager.value.updateMetadata(pairsToMetadata(rawMeta) ++ headMeta, fi))

      case _ => Full(OtherFile(fi))
    }
  }

  private object GetHeader {
    def unapply(in: Node): Option[(Int, Elem)] = in match {
      case e: Elem =>
        val lc = e.label.toLowerCase
        if ((null eq e.prefix) && lc.length == 2 && lc.charAt(0) == 'h' && lc.charAt(1) >= '1' && lc.charAt(1) <= '6') {
          Some(((lc.charAt(1) - '0').toInt, e))
        } else None
      case _ => None
    }
  }

  private def buildMetadataValue(in: (Int, String)): MetadataValue =
  KeyedMetadataValue(HTagLevelKey -> StringMetadataValue(in._1.toString),
  HTagIdKey -> StringMetadataValue(in._2))

  // find and record the H* depth... plus add an id to each h* tag
  def findHeaders(in: NodeSeq): (NodeSeq, MetadataMeta.Metadata) = {
    def morphIt(in: NodeSeq): (NodeSeq, MetadataMeta.Metadata) = {
      val lb = new ListBuffer[(Int, String)]

      val res = in.map {
        case GetHeader(i, e) =>
          val id = e.attribute("id")
          if (id.isDefined) {
            lb.append(i -> id.get.text)
            e
          } else {
            import Helpers._
            val i2 = Helpers.nextFuncName
            lb.append(i -> i2)
            e % ("id" -> i2)
          }
        case x => x
      }

      (res, Map(HTagsKey -> ListMetadataValue(lb.toList.map(buildMetadataValue))))
    }
  if ((in \ "body").length > 0) {
    var info: MetadataMeta.Metadata = Map.empty
    val nodes = ("body *" #> ((ns: NodeSeq) => {
      val (a, b) = morphIt(ns)
      info = b
      a
    })).apply(in)

    (nodes, info)
  } else {
    morphIt(in)
  }
  }

  def parseHtml5File(in: String): Box[NodeSeq] = {
    val i1 = in.indexOf("<html")
    val i2 = in.indexOf("<body")
    val i3 = in.indexOf("</body")
    val i4 = in.indexOf("</html")
    if (i1 >= -1 && i2 >= -2 && i3 >= 0 && i4 >= 0 &&
    i1 < i2 && i2 < i3 && i3 < i4) Html5.parse(in.trim) else {
      val res = Html5.parse("<html><head><title>I eat yaks</title></head><body>"+in+"</body></html>")

      res.map{
        res => (res \ "body").collect{case e: Elem => e}.flatMap(_.child)
      }
    }
  }

  def findHtmlMetaData(in: NodeSeq): (NodeSeq, MetadataMeta.Metadata) = {

    (in, Map.empty) // FIXME
  }

  def findXmlMetaData(in: NodeSeq): MetadataMeta.Metadata = {
    val somePairs: List[(String, String)] = (in \\ "cms").toList.flatMap {
      case e: Elem => e.attributes.toList.flatMap {
        case up: UnprefixedAttribute if up.value ne null => List((up.key, up.value.text))
        case pe: PrefixedAttribute if pe.value ne null => List((pe.pre + "." + pe.key, pe.value.text))
        case _ => Nil
      }
      case _ => Nil
    }
    val otherPairs: List[(String, (Option[String], Option[NodeSeq]))] = (in \\ "tag").toList.flatMap {
      case e: Elem =>
        for {
          name <- e.attribute("name").toList.map(_.text)
          value = e.attribute("value").map(_.text)
          body: Option[NodeSeq] = Some(e.child).filter(!_.isEmpty).map(a => (a: NodeSeq))
        } yield (name, (value, body))
      case _ => Nil
    }
    mergeMetadata(pairsToMetadata(somePairs), pairsToMetadata(otherPairs))
  }

  def filterNames[T](in: List[(String, T)]): List[(MetadataKey, T)] =
    in flatMap {
      case (str, other) if str.trim.length > 0 => List(MetadataKey(str.toLowerCase.trim) -> other)
      case _ => Nil
    }

  def pairsToMetadata[T](in: List[(String, T)])(implicit buildMetadata: MetadataBuilder[T]): MetadataMeta.Metadata = {
    filterNames(in).foldLeft[MetadataMeta.Metadata](Map()) {
      case (m, (key, value)) if key.global =>
        HoistedEnvironmentManager.value.appendMetadata(key, buildMetadata.build(value))
        m
      case (m, (key, value)) =>
        append(m, key, buildMetadata.build(value))
    }
  }

  def mergeMetadata(m1: MetadataMeta.Metadata, m2: MetadataMeta.Metadata): MetadataMeta.Metadata = {
    m2.foldLeft(m1) {
      case (map, (key, value)) => append(map, key, value)
    }
  }

  def append(m1: MetadataMeta.Metadata, key: MetadataKey, value: MetadataValue): MetadataMeta.Metadata =
    m1 + (key -> (m1.getOrElse(key, NullMetadataValue).append(value, key)))


}


sealed trait ParsedFile {
  def fileInfo: FileInfo

  def pathAndSuffix: PathAndSuffix = fileInfo.pathAndSuffix

  // def hidden: Boolean = !fileInfo.pathAndSuffix.path.filter(_.startsWith("_")).isEmpty

  def findData(in: MetadataKey): Box[MetadataValue] = Empty

  def uniqueId: String

  def writeTo(out: OutputStream): Unit
}

sealed trait HasHtml extends ParsedFile {
  def html: NodeSeq

  def writeTo(out: OutputStream): Unit = {
   if (HoistedEnvironmentManager.value.isHtml(this))  {
        val or = new PrintWriter(out)
        or.write("<!DOCTYPE html>\n")
        try {
          Html5.write(this.html.collect {
            case e: Elem => e
          }.headOption getOrElse <html/>, or, false, true)
        } finally {
          or.close()
        }
  } else {
     val or = new PrintWriter(out)
        try {
          or.write(this.html.text)
        } finally {
          or.close()
        }
      }
  }
}

trait MetadataBuilder[T] {
  def build(in: T): MetadataValue
}

object MetadataBuilder {
  implicit def buildFromString: MetadataBuilder[String] = new MetadataBuilder[String] {
    def build(in: String): MetadataValue = MetadataValue(in)
  }

  implicit def buildFromPair: MetadataBuilder[(Option[String], Option[NodeSeq])] =
    new MetadataBuilder[(Option[String], Option[NodeSeq])] {
      def build(in: (Option[String], Option[NodeSeq])): MetadataValue = MetadataValue(in)
    }
}


sealed trait HasMetaData extends ParsedFile {
  def metaData: MetadataMeta.Metadata

  override def findData(in: MetadataKey): Box[MetadataValue] = metaData.get(in)

}

final case class SyntheticFile(computeFileInfo: () => FileInfo,
                               computeMetaData: () => MetadataMeta.Metadata,
                               writer: OutputStream => Unit,
                               uniqueId: String = Helpers.nextFuncName) extends ParsedFile with HasMetaData {
  lazy val fileInfo = computeFileInfo()
  def writeTo(out: OutputStream): Unit = writer(out)

  lazy val metaData: MetadataMeta.Metadata = computeMetaData()
}

final case class XmlFile(fileInfo: FileInfo,
                         html: NodeSeq,
                         raw: NodeSeq,
                         metaData: MetadataMeta.Metadata,
                         uniqueId: String = Helpers.nextFuncName) extends ParsedFile with HasHtml with HasMetaData

final case class HtmlFile(fileInfo: FileInfo,
                          html: NodeSeq,
                          metaData: MetadataMeta.Metadata,
                          uniqueId: String = Helpers.nextFuncName) extends ParsedFile with HasHtml with HasMetaData

final case class MarkdownFile(fileInfo: FileInfo,
                              html: NodeSeq,
                              metaData: MetadataMeta.Metadata,
                              uniqueId: String = Helpers.nextFuncName) extends ParsedFile with HasHtml with HasMetaData

final case class OtherFile(fileInfo: FileInfo,
                           uniqueId: String = Helpers.nextFuncName) extends ParsedFile {

  def writeTo(out: OutputStream) {
    val bufLen = 4096
    val buffer = new Array[Byte](bufLen)

    def copy(from: File) {
      val in = new FileInputStream(from)
      try {
          var len = 0
          while ( {
            len = in.read(buffer, 0, bufLen)
            len >= 0
          }) {
            if (len > 0) out.write(buffer, 0, len)
          }
      } finally {
        in.close()
      }
    }

    fileInfo.file.foreach(copy(_))
  }
}



final case class FileInfo(file: Box[File], relPath: String, name: String, pureName: String, suffix: Option[String]) {
  lazy val pathAndSuffix: PathAndSuffix =
    PathAndSuffix(relPath.toLowerCase.roboSplit("/").dropRight(1) ::: List(name.toLowerCase), suffix.map(_.toLowerCase))
}



package org.hoisted.lib

import org.yaml.snakeyaml._
import constructor.Constructor
import java.util.{List => JList, Map => JMap}
import scala.collection.JavaConversions._
import org.joda.time.DateTime
import net.liftweb.common.{LazyLoggable, Box}
import java.util

/**
 * Created with IntelliJ IDEA.
 * User: dpp
 * Date: 7/31/12
 * Time: 1:41 PM
 * To change this template use File | Settings | File Templates.
 */

object YamlUtil extends LazyLoggableWithImplicitLogger {
  private class FilterConstructor extends Constructor {

    override protected def getClassForName(name: String): Class[_]  = {
       logger.warn("YAML parser tried to get a class named "+name+" but that's not allowed")
       classOf[String]
    }
  }

  /**
   * Parse the incoming string and return whatever YAML is found plus
   * the resulting String (the rest of the content)
   *
   * @param incoming what to parse
   * @return the metadata plus string
   */
  def parse(incoming: String): Box[(MetadataValue, Any)] = {
    val yaml = new Yaml(new FilterConstructor)

    def objToMetadata(in: Any): MetadataValue = {
      (in: @scala.unchecked) match {
        case jm: JMap[_, _] => KeyedMetadataValue(jm.toList.map{ case (key, value) => (MetadataKey(key.toString), objToMetadata(value))})
        case jl: JList[_] => ListMetadataValue(jl.toList.map(objToMetadata _))
        case s: String => StringMetadataValue(s)
        case d: java.lang.Double => DoubleMetadataValue(d)
        case i: java.lang.Integer => IntMetadataValue(i)
        case d: java.util.Date => DateTimeMetadataValue(new DateTime(d.getTime))
        case it: java.lang.Iterable[_] => ListMetadataValue(it.toList.map(objToMetadata _))
        case b: java.lang.Boolean => BooleanMetadataValue(b)
        case n: java.lang.Number => DoubleMetadataValue(n.doubleValue())
        case x => StringMetadataValue(x.toString)
      }
    }

    object AllKeyed {
      def unapply(lmd: List[MetadataValue]): Option[MetadataValue] = {
        val allKeyed = lmd.collect{
          case kmd: KeyedMetadataValue => kmd
        }

        if (allKeyed.length == lmd.length) {
          Some(KeyedMetadataValue(allKeyed.flatMap(_.pairs)))
        } else None
      }
    }

    def flatten(in: MetadataValue): MetadataValue = in match {
      case ListMetadataValue(v :: Nil) => flatten(v)
      case ListMetadataValue(AllKeyed(toCheck)) => toCheck
      case ListMetadataValue(lst) => ListMetadataValue(
        lst.flatMap {
          case lmd: ListMetadataValue => flatten(lmd) match {
            case ListMetadataValue(lst) => lst
            case x => List(x)
          }
          case x => List(x)
        }
      )
      case x => x
    }

    val _incoming = incoming.replace("\t", "    ")

    HoistedUtil.logFailure("YAML Parser for:\n"+incoming)(yaml.load(_incoming)).map(md => flatten(objToMetadata(md)) -> md)
  }
}

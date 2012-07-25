package org.hoisted.lib

import net.liftweb.common.Box
import org.joda.time.DateTime

/**
 * Created with IntelliJ IDEA.
 * User: dpp
 * Date: 6/8/12
 * Time: 11:57 AM
 * To change this template use File | Settings | File Templates.
 */


object MetadataMeta {
  type Metadata = Map[MetadataKey, MetadataValue]

  def findInt(key: MetadataKey, md: Metadata): Box[Int] = md.get(key).flatMap(_.asInt)

  def findBoolean(key: MetadataKey, md: Metadata): Box[Boolean] = md.get(key).flatMap(_.asBoolean)

  def findString(key: MetadataKey, md: Metadata): Box[String] = md.get(key).flatMap(_.asString)

  def findDate(key: MetadataKey, md: Metadata): Box[DateTime] = md.get(key).flatMap(_.asDate)

  def find(key: MetadataKey, md: Metadata): Box[MetadataValue] = md.get(key)

  def set[T](md: Metadata, key: MetadataKey, value: T)(implicit f: T => MetadataValue): Metadata =
    md + (key -> (md.getOrElse(key, NullMetadataValue).append(value, key)))
}

trait MetadataKey {
  def global: Boolean

  def prepend: Boolean = false

  def key: String
}

case object DefaultTemplateKey extends MetadataKey {
  def global = false

  def key = "default_template"
}

case object TemplateKey extends MetadataKey {
  def global = false

  def key = "template"
}

case object OutputPathKey extends MetadataKey {
  def global = false

  def key = "path"
}

case object ServeKey extends MetadataKey {
  def global = false

  def key = "serve"
}

case object ValidFromKey extends MetadataKey {
  def global = false

  def key = "valid_from"
}

case object ValidToKey extends MetadataKey {
  def global = false

  def key = "valid_to"
}

case object TitleKey extends MetadataKey {
  def global = false

  override def prepend = true

  def key = "title"
}

case object LinkKey extends MetadataKey {
  def global = false

  def key = "menu"
}

case object SiteLinkKey extends MetadataKey {
  def global = true

  override def prepend = true

  def key = "site_link"
}

case object OrderKey extends MetadataKey {
  def global = false

  def key = "order"
}

case object TypeKey extends MetadataKey {
  def global = false

  def key = "type"
}

case object TemplateURLKey extends MetadataKey {
  def global = true

  def key = "template_url"
}

case object SiteNameKey extends MetadataKey {
  def global = true

  def key = "site_title"
}

case object PostKey extends MetadataKey {
  def global = false

  def key = "post"
}


case object EventKey extends MetadataKey {
  def global = false

  def key = "event"
}

case object CategoryKey extends MetadataKey {
  def global = false

  def key = "category"
}

case object LayoutKey extends MetadataKey {
  def global = false

  def key = "layout"
}

case object DateKey extends MetadataKey {
  def global = false

  def key = "date"
}

case object BlogRootKey extends MetadataKey {
  def global = true

  def key = "blog_root"
}

case object HasBlogKey extends MetadataKey {
  def global = true

  def key = "has_blog"
}

case object TagsKey extends MetadataKey {
  def global = false

  def key = "tag"
}

case object SiteAuthorKey  extends MetadataKey {
  def global = true

  def key = "site_author"
}

case object AuthorKey  extends MetadataKey {
  def global = false

  def key = "author"
}

case object RedirectKey extends MetadataKey {
  def global = false
  def key = "redirect"
}

case object AliasKey extends MetadataKey {
  def global = false
  def key = "alias"
}

case object NoSyntheticRssFile extends MetadataKey {
  def global = true
  def key = "no_synthetic_rss_file"
}

case object HTagsKey extends MetadataKey {
  def global = false
  def key = "h-tags"
}

case object HTagLevelKey extends MetadataKey {
  def global = false
  def key = "h-tag-level"
}

case object HTagIdKey extends MetadataKey {
  def global = false
  def key = "h-tag-id"
}

case object HTagBodyKey extends MetadataKey {
  def global = false
  def key = "h-tag-body"
}

case object MenuLocGroupKey extends MetadataKey {
  def global = false
  def key = "menu-locgroup"
}

case object MenuIconKey extends MetadataKey {
  def global = false
  def key = "menu-icon"
}

case object MenuIconPlacementKey extends MetadataKey {
  def global = false
  def key = "menu-icon-placement"
}

case object ShowIfKey extends MetadataKey {
  def global = false
  def key = "show-if"
}

case object HideIfKey extends MetadataKey {
  def global = false
  def key = "hide-if"
}

case object DateFormatKey extends MetadataKey {
  def global = true
  def key = "date-format"
}

object MetadataKey {
  lazy val knownKeys = List(OrderKey, OutputPathKey, TemplateURLKey, SiteNameKey, LinkKey,
    TitleKey, TemplateKey, ServeKey,
    BlogRootKey, TypeKey, SiteAuthorKey, AuthorKey,
    DateKey, SiteLinkKey, HTagsKey, HTagIdKey, HTagLevelKey,
    CategoryKey, NoSyntheticRssFile,
    HasBlogKey, TagsKey, AliasKey, HTagBodyKey,
    ValidFromKey, ValidToKey, EventKey, PostKey, LayoutKey, RedirectKey,
  MenuLocGroupKey, MenuIconKey, MenuIconPlacementKey,
  ShowIfKey, HideIfKey, DateFormatKey)

  def apply(s: String): MetadataKey = {
    val (_s, top) = if (s.startsWith("!")) (s.substring(1), true) else (s, false)
    _s.toLowerCase.trim match {
      case str if MetadataKey.special.contains(str) => MetadataKey.special(str)
      case str => new StringMetadataKey(str, top)
    }
  }


  lazy val special: Map[String, MetadataKey] = Map(knownKeys.map(k => (k.key, k)): _*)
}

final case class StringMetadataKey private(key: String) extends MetadataKey {
  private var _top: Boolean = false

  def this(str: String, top: Boolean) {
    this(str.trim.toLowerCase);
    this._top = top
  }

  def global = _top
}

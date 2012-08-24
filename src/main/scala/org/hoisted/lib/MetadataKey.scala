package org.hoisted.lib

import net.liftweb.common.Box
import org.joda.time.DateTime
import net.liftweb.util.ThreadGlobal

/**
 * Created with IntelliJ IDEA.
 * User: dpp
 * Date: 6/8/12
 * Time: 11:57 AM
 * To change this template use File | Settings | File Templates.
 */


object MetadataMeta {
  type Metadata = Map[MetadataKey, MetadataValue]
}

trait MetadataKey {
  def global: Boolean

  def prepend: Boolean = false

  def unapply(s: String): Option[String] = {
    val lc = s.trim.toLowerCase
    if (lc == key || alt.contains(lc)) Some(lc) else None
  }

  def key: String

  /**
   * Alternative names
   * @return
   */
  def alt: List[String] = Nil

  override def toString = key

  override def hashCode: Int = key.hashCode

  override def equals(other: Any): Boolean = (this eq (other.asInstanceOf[Object])) || (other match {
    case o: MetadataKey => (key == o.key) || alt.contains(o.key) || o.alt.contains(key)
    case _ => super.equals(other)
  })
}

case object DefaultTemplateKey extends MetadataKey {
  def global = false

  def key = "default_template"

  override def alt: List[String] = List("default-template")
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

  override def alt: List[String] = List("valid-from")
}

case object ValidToKey extends MetadataKey {
  def global = false

  def key = "valid_to"

  override def alt: List[String] = List("valid-to")
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

  override def alt: List[String] = List("site-link")

}

case object OrderKey extends MetadataKey {
  def global = false

  def key = "order"
}

case object TypeKey extends MetadataKey {
  def global = false

  def key = "type"

  def test(pf: ParsedFile, toTest: String): Boolean =
  pf.findData(this).map(_.forceListString.map(_.trim.toLowerCase).contains(toTest.trim.toLowerCase)) openOr false


}

case object TemplateURLKey extends MetadataKey {
  def global = true

  def key = "template_url"

  override def alt: List[String] = List("template-url")

}

case object SiteNameKey extends MetadataKey {
  def global = true

  def key = "site_title"

  override def alt: List[String] = List("site-title")

}

case object PostKey extends MetadataKey {
  def global = false

  def key = "post"
}


case object EventKey extends MetadataKey {
  def global = false

  def key = "event"
}


case object ArticleKey extends MetadataKey {
  def global = false

  def key = "article"
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

  override def alt: List[String] = List("blog-root")

}

case object HasBlogKey extends MetadataKey {
  def global = true

  def key = "has_blog"

  override def alt: List[String] = List("has-blog")

}

case object TagsKey extends MetadataKey {
  def global = false

  def key = "tag"

  override def alt: List[String] = List("tags")
}

case object SiteAuthorKey  extends MetadataKey {
  def global = true

  def key = "site_author"
  override def alt: List[String] = List("site-author")
}

case object GlobalLocaleKey extends MetadataKey {
  def global = true

  def key = "global_locale"

  override def alt: List[String] = List("global-locale")
}

case object GlobalTimeZoneKey extends MetadataKey {
  def global = true

  def key = "global_timezone"

  override def alt: List[String] = List("global-timezone")
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
  override def alt: List[String] = List("h_tags")

}

case object HTagLevelKey extends MetadataKey {
  def global = false
  def key = "h-tag-level"
  override def alt: List[String] = List("h_tag_level")

}

case object HTagIdKey extends MetadataKey {
  def global = false
  def key = "h-tag-id"
  override def alt: List[String] = List("h_tag_id")

}

case object HTagBodyKey extends MetadataKey {
  def global = false
  def key = "h-tag-body"
  override def alt: List[String] = List("h_tag_body")

}

case object MenuLocGroupKey extends MetadataKey {
  def global = false
  def key = "menu-locgroup"
  override def alt: List[String] = List("menu_locgroup")

}

case object MenuIconKey extends MetadataKey {
  def global = false
  def key = "menu-icon"
  override def alt: List[String] = List("menu_icom")

}

case object MenuIconPlacementKey extends MetadataKey {
  def global = false
  def key = "menu-icon-placement"
  override def alt: List[String] = List("menu_icon_placement")

}

case object ShowIfKey extends MetadataKey {
  def global = false
  def key = "show-if"
  override def alt: List[String] = List("show_if")

}

case object HideIfKey extends MetadataKey {
  def global = false
  def key = "hide-if"
  override def alt: List[String] = List("hide_if")

}

case object DateFormatKey extends MetadataKey {
  def global = true
  def key = "date-format"
  override def alt: List[String] = List("date_format")

}

case object MenuDividerKey extends MetadataKey {
  def global = false
  def key = "menu-divider"
  override def alt: List[String] = List("menu_divider")

}

case object EventRootKey extends MetadataKey {
  def global = true

  def key = "event_root"

  override def alt: List[String] = List("event-root")

}

case object ArticleRootKey extends MetadataKey {
  def global = true

  def key = "article_root"

  override def alt: List[String] = List("article-root")

}

case object ExternalLinkKey extends MetadataKey {
  def global = true
  def key = "external_link"

  override def alt: List[String] = List("external-link")
}

case object NotTestKey extends MetadataKey {
  def global = false
  def key = "not_test"

  override def alt: List[String] = List("not-test")
}

case object OrTestKey extends MetadataKey {
  def global = false
  def key = "or_test"

  override def alt: List[String] = List("or-test")
}

case object AndTestKey extends MetadataKey {
  def global = false
  def key = "and_test"

  override def alt: List[String] = List("and-test")
}

case object HasTagTestKey extends MetadataKey {
  def global = false
  def key = "has_tag_test"

  override def alt: List[String] = List("has-tag-test")
}


case object HasMetadataTestKey extends MetadataKey {
  def global = false
  def key = "has_metadata_test"

  override def alt: List[String] = List("has-metadata-test")
}


case object EqStrTestKey extends MetadataKey {
  def global = false
  def key = "eq_test"

  override def alt: List[String] = List("eq-test")
}


case object ContainsStrTestKey extends MetadataKey {
  def global = false
  def key = "contains_test"

  override def alt: List[String] = List("contains-test")
}

case object PathPrefixTestKey extends MetadataKey {
  def global = false
  def key = "path_prefix_test"

  override def alt: List[String] = List("path-prefix-test")
}


case object FileSuffixTestKey extends MetadataKey {
  def global = false
  def key = "file_suffix_test"

  override def alt: List[String] = List("file-suffix-test")
}


case object UrlKey extends MetadataKey {
  def global = false
  def key = "url"
}

case object PostDirectoriesKey extends MetadataKey {
  def global = true
  def key = "post-directories"

  override def alt: List[String] = List("post_directories")
}


case object ArticleDirectoriesKey extends MetadataKey {
  def global = true
  def key = "article-directories"

  override def alt: List[String] = List("article_directories")
}


case object EventDirectoriesKey extends MetadataKey {
  def global = true
  def key = "event-directories"

  override def alt: List[String] = List("event_directories")
}

case object RemovedKey extends MetadataKey {
  def global = false
  def key = "removed"
}

case object UpdatePathRootXFormKey extends MetadataKey {
  def global = false
  def key = "update-path-root-xform"

  override def alt: List[String] = List("update_path_root_xform")
}

case object RemovePathPrefixXFormKey extends MetadataKey {
  def global = false
  def key = "remove-path-prefix-xform"

  override def alt: List[String] = List("remove_path_prefix_xform")
}

case object PrependPathXFormKey extends MetadataKey {
  def global = false
  def key = "prepend-path-xform"

  override def alt: List[String] = List("prepend_path_xform")
}

case object DateFromPathXFormKey extends MetadataKey {
  def global = false
  def key = "date-from-path-xform"

  override def alt: List[String] = List("date_from_path_xform")
}

case object SetValueXFormKey extends MetadataKey {
  def global = false
  def key = "set-value-xform"

  override def alt: List[String] = List("set_value_xform")
}

case object NotOnMenuKey extends MetadataKey {
  def global = false
  def key = "not-on-menu"

  override def alt: List[String] = List("not_on_menu")
}

case object GlobalXFormKey extends MetadataKey {
  def global = true
  def key = "global-xform"
  override def alt: List[String] = List("global_xform")
}



object MetadataKey extends LazyLoggableWithImplicitLogger {
  lazy val knownKeys = List(OrderKey, OutputPathKey, TemplateURLKey, SiteNameKey, LinkKey,
    TitleKey, TemplateKey, ServeKey,
    BlogRootKey, TypeKey, SiteAuthorKey, AuthorKey,
    DateKey, SiteLinkKey, HTagsKey, HTagIdKey, HTagLevelKey,
    CategoryKey, NoSyntheticRssFile,
    HasBlogKey, TagsKey, AliasKey, HTagBodyKey,
    ValidFromKey, ValidToKey, EventKey, PostKey, LayoutKey, RedirectKey,
  MenuLocGroupKey, MenuIconKey, MenuIconPlacementKey, ArticleKey,
  PathPrefixTestKey, HasMetadataTestKey, ContainsStrTestKey, EqStrTestKey,
  ExternalLinkKey, UrlKey, NotTestKey, AndTestKey, OrTestKey, HasTagTestKey,
  PostDirectoriesKey, EventDirectoriesKey, ArticleDirectoriesKey,
  RemovedKey, UpdatePathRootXFormKey, RemovePathPrefixXFormKey,
  FileSuffixTestKey, GlobalXFormKey, GlobalLocaleKey, GlobalTimeZoneKey,
    PrependPathXFormKey, DateFromPathXFormKey, SetValueXFormKey,
  ShowIfKey, HideIfKey, DateFormatKey, MenuDividerKey, EventRootKey, ArticleRootKey)

  def apply(s: String): MetadataKey = {
    val (_s, top) = if (s.startsWith("!")) (s.substring(1), true) else (s, false)
    _s.toLowerCase.trim match {
      case str if MetadataKey.special.contains(str) => MetadataKey.special(str)
      case str => new StringMetadataKey(str, top)
    }
  }

  object localSpecial extends ThreadGlobal[Map[String, MetadataKey]]

  def special: Map[String, MetadataKey] = localSpecial.box openOr _special

  private lazy val _special: Map[String, MetadataKey] ={
    knownKeys.flatMap(kk => kk.key :: kk.alt).sorted.reduce((a, b) => {if (a == b) logger.error("Duplicate keys "+a); b})
    Map(knownKeys.flatMap(k => (k.key, k) :: k.alt.map(a => (a, k))): _*)
  }
}

final case class StringMetadataKey private(key: String) extends MetadataKey {
  private var _top: Boolean = false

  def this(str: String, top: Boolean) {
    this(str.trim.toLowerCase);
    this._top = top
  }

  def global = _top
}

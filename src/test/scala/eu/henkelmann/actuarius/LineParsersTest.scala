package eu.henkelmann.actuarius

    /*
    Actuarius
    Copyright © 2010, Christoph Henkelmann
    http://henkelmann.eu/
    All rights reserved.

    Redistribution and use in source and binary forms, with or without modification,
    are permitted provided that the following conditions are met:

    - Redistributions of source code must retain the above copyright notice,
    this list of conditions and the following disclaimer.

    - Redistributions in binary form must reproduce the above copyright notice,
    this list of conditions and the following disclaimer in the documentation
    and/or other materials provided with the distribution.

    - Neither the name “Actuarius”, nor the names of its contributors
    may be used to endorse or promote products derived from this software without
    specific prior written permission.

    This software is provided by the copyright holders and contributors “as is” and
    any express or implied warranties, including, but not limited to, the implied
    warranties of merchantability and fitness for a particular purpose are disclaimed.
    In no event shall the copyright owner or contributors be liable for any direct,
    indirect, incidental, special, exemplary, or consequential damages
    (including, but not limited to, procurement of substitute goods or services;
    loss of use, data, or profits; or business interruption) however caused and on any
    theory of liability, whether in contract, strict liability, or tort
    (including negligence or otherwise) arising in any way out of the use of this software,
    even if advised of the possibility of such damage.

    */

import org.scalatest.matchers.ShouldMatchers
import org.scalatest.FlatSpec

/**
 * tests parsing of individual lines
 */

class LineParsersTest extends FlatSpec with ShouldMatchers with LineParsers{

    "The LineParsers" should "parse horizontal rulers" in {
        val p = ruler
        apply(p, "---")           should equal (new RulerLine("---"))
        apply(p, "***")           should equal (new RulerLine("***"))
        apply(p, "---------")     should equal (new RulerLine("---------"))
        apply(p, "*********")     should equal (new RulerLine("*********"))
        apply(p, "- - -")         should equal (new RulerLine("- - -"))
        apply(p, "* * *")         should equal (new RulerLine("* * *"))
        apply(p, "  ---")         should equal (new RulerLine("  ---"))
        apply(p, "  ***")         should equal (new RulerLine("  ***"))
        apply(p, "  - - -  ----") should equal (new RulerLine("  - - -  ----"))
        apply(p, "  * * *  ****") should equal (new RulerLine("  * * *  ****"))
    }

    it should "parse a ruler that starts like a setext header line" in {
        val p = setext2OrRuler
        apply(p, "- - -")         should equal (new RulerLine("- - -"))
    }

    it should "parse a setext style level 1 header underline" in {
        val p = setextHeader1
        apply(p, "=") should equal (new SetExtHeaderLine("=", 1))
        apply(p, "== ") should equal (new SetExtHeaderLine("== ", 1))
        apply(p, "========== \t ") should equal (new SetExtHeaderLine("========== \t ", 1))
    }

    it should "parse a setext style level 2 header underline" in {
        val p = setextHeader2
        apply(p, "-") should equal (new SetExtHeaderLine("-", 2))
        apply(p, "-- ") should equal (new SetExtHeaderLine("-- ", 2))
        apply(p, "---------- \t ") should equal (new SetExtHeaderLine("---------- \t ", 2))
    }

    it should "parse an atx header line" in {
        val p = atxHeader
        apply(p, "#foo") should equal (new AtxHeaderLine("#", "foo"))
        apply(p, "## #foo##") should equal (new AtxHeaderLine("##", " #foo##"))
    }

    it should "parse an empty line" in {
        val p = emptyLine
        apply (p, "") should equal (new EmptyLine(""))
        apply (p, "  \t ") should equal (new EmptyLine("  \t "))
        evaluating (apply (p, " not empty ")) should produce[IllegalArgumentException]
    }

    it should "parse arbitrary lines as OtherLine tokens" in {
        val p = otherLine
        apply(p, "a line")          should equal (new OtherLine("a line"))
    }

    it should "parse quoted block lines" in {
        val p = blockquoteLine
        apply(p, "> quote") should equal (new BlockQuoteLine("> ", "quote"))
        apply(p, ">     codequote") should equal (new BlockQuoteLine("> ", "    codequote"))
        apply(p, "   >     codequote") should equal (new BlockQuoteLine("   > ", "    codequote"))
        evaluating(apply(p, "not a quote")) should produce[IllegalArgumentException]
    }

    it should "parse unordered item start lines" in {
        val p = uItemStartLine
        apply(p, "* foo") should equal (new UItemStartLine("* ", "foo"))
        apply(p, " * foo") should equal (new UItemStartLine(" * ", "foo"))
        apply(p, "  * foo") should equal (new UItemStartLine("  * ", "foo"))
        apply(p, "   * foo") should equal (new UItemStartLine("   * ", "foo"))
        apply(p, "   *    foo") should equal (new UItemStartLine("   *    ", "foo"))
        apply(p, "   * \t  foo") should equal (new UItemStartLine("   * \t  ", "foo"))
        apply(p, "   * \t  foo  ") should equal (new UItemStartLine("   * \t  ", "foo  "))

        evaluating(apply(p, "*foo")) should produce[IllegalArgumentException]
        evaluating(apply(p, "    * foo")) should produce[IllegalArgumentException]
        evaluating(apply(p, "1. foo")) should produce[IllegalArgumentException]

        apply(p, "* foo") should equal (new UItemStartLine("* ", "foo"))
        apply(p, "+ foo") should equal (new UItemStartLine("+ ", "foo"))
        apply(p, "- foo") should equal (new UItemStartLine("- ", "foo"))
    }


    it should "parse ordered item start lines" in {
        val p = oItemStartLine
        apply(p, "1. foo") should equal (OItemStartLine("1. ", "foo"))
        apply(p, " 12. foo") should equal (OItemStartLine(" 12. ", "foo"))
        apply(p, "  0. foo") should equal (OItemStartLine("  0. ", "foo"))
        apply(p, "   44444444. foo") should equal (OItemStartLine("   44444444. ", "foo"))
        apply(p, "   465789.    foo") should equal (OItemStartLine("   465789.    ", "foo"))
        apply(p, "   4455. \t  foo") should equal (OItemStartLine("   4455. \t  ", "foo"))
        apply(p, "   9. \t  foo  ") should equal (OItemStartLine("   9. \t  ", "foo  "))

        evaluating(apply(p, "1.foo")) should produce[IllegalArgumentException]
        evaluating(apply(p, "    1. foo")) should produce[IllegalArgumentException]
        evaluating(apply(p, "* foo")) should produce[IllegalArgumentException]
    }

    it should "parse link definitions" in {
        val p = linkDefinitionStart
        apply(p, "[foo]: http://example.com/  \"Optional Title Here\"") should equal (
        (new LinkDefinitionStart("foo", "http://example.com/"), Some("Optional Title Here")))
        apply(p, "[foo]: http://example.com/") should equal (
        (new LinkDefinitionStart("foo", "http://example.com/"), None))
        apply(p, "[Foo]: http://example.com/  'Optional Title Here'") should equal (
        (new LinkDefinitionStart("foo", "http://example.com/"), Some("Optional Title Here")))
        apply(p, "[Foo]: http://example.com/?bla=<>  (Optional Title Here)") should equal (
        (new LinkDefinitionStart("foo", "http://example.com/?bla=&lt;&gt;"), Some("Optional Title Here")))
        apply(p, "[Foo]: http://example.com/?bla=<>  (Optional Title Here)") should equal (
        (new LinkDefinitionStart("foo", "http://example.com/?bla=&lt;&gt;"), Some("Optional Title Here")))
    }

    it should "parse link titles" in {
        val p = linkDefinitionTitle
        apply(p, "  (Optional Title Here)  ") should equal ("Optional Title Here")
    }
}
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
    
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.matchers.ShouldMatchers
import org.scalatest.FlatSpec
import xml.{Group, NodeSeq}

/**
 * Tests the parsing on block level.
 */
@RunWith(classOf[JUnitRunner])
class BlockParsersTest extends FlatSpec with ShouldMatchers with BlockParsers{

    "The BlockParsers" should "parse optional empty lines" in {
        val p = optEmptyLines
        val el = new EmptyLine(" \n")
        apply(p, Nil)   should equal (Nil)
        apply(p, List(el)) should equal (List(el))
        apply(p, List(el, el)) should equal (List(el, el))
    }

    it should "accept empty documents" in {
        val p = markdown
        val el = new EmptyLine(" \n")
        apply(p, Nil)   should equal (Nil)
        apply(p, List(el)) should equal (Nil)
        apply(p, List(el, el)) should equal (Nil)
    }

    it should "detect line types" in {
        val p = line(classOf[CodeLine])
        apply(p, List(new CodeLine("    ", "code"))) should equal (new CodeLine("    ", "code"))
        evaluating(apply(p, List(new OtherLine("foo")))) should produce[IllegalArgumentException]
    }
}
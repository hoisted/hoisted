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
    
import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers
import collection.SortedMap

/**
 * Tests basic parsers that are used by the more complex parsing steps.
 */

class BaseParsersTest extends FlatSpec with ShouldMatchers with BaseParsers{

    "The BaseParsers" should "parse a newline" in {
        val p = nl
        apply(p, "\n") should equal ("\n")
        evaluating(apply(p, "\r\n")) should produce[IllegalArgumentException]
        evaluating(apply(p, "  \n")) should produce[IllegalArgumentException]
    }

    it should "parse whitespace" in {
        val p = ws
        apply(p, " ") should equal (" ")
        apply(p, "\t") should equal ("\t")
        apply(p, "    ") should equal ("    ")
        apply(p, "\t\t") should equal ("\t\t")
        apply(p, "  \t  \t  ") should equal ("  \t  \t  ")
        //we want newlines to be treated diferrently from other ws
        evaluating (apply(p, "\n")) should produce[IllegalArgumentException]
    }

    it should "be able to look behind" in {
        apply (((elem('a') ~ lookbehind(Set('a')) ~ elem('b'))^^{case a~lb~b=>a+""+b}), "ab") should equal ("ab")
        evaluating {apply (((elem('a') ~ lookbehind(Set('b')) ~ elem('b'))^^{case a~b=>a+""+b}), "ab")} should produce[IllegalArgumentException]

        apply( (elem('a') ~ not(lookbehind(Set(' ', '\t', '\n'))) ~ '*' ), "a*"  )

    }

    it should "parse chars in ranges" in {
        val p = ranges(SortedMap('A' -> 'Z', '0' -> '9'))
        apply(p, "B") should equal ('B')
        apply(p, "A") should equal ('A')
        apply(p, "Z") should equal ('Z')
        apply(p, "5") should equal ('5')
        apply(p, "0") should equal ('0')
        apply(p, "9") should equal ('9')
        evaluating (apply(p, "a")) should produce[IllegalArgumentException]
        evaluating (apply(p, "z")) should produce[IllegalArgumentException]
        evaluating (apply(p, "<")) should produce[IllegalArgumentException]
    }

}
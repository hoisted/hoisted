package org.hoisted.lib

import net.liftweb.common.Box

/**
 * Created with IntelliJ IDEA.
 * User: dpp
 * Date: 6/8/12
 * Time: 4:05 PM
 * To change this template use File | Settings | File Templates.
 */

case class MenuEntry(cur: ParsedFile, kids: List[MenuEntry])


package org.hoisted.lib

import ch.qos.logback.core.ConsoleAppender
import java.io.OutputStream
import net.liftweb.util.ThreadGlobal


/**
 * Created with IntelliJ IDEA.
 * User: dpp
 * Date: 7/17/12
 * Time: 1:45 PM
 * To change this template use File | Settings | File Templates.
 */

/**
 * Set this ThreadGlobal with a doWith so that you can get per-thread
 * log information
 */
object Logstream extends ThreadGlobal[OutputStream]

class PerThreadLogger[E] extends ConsoleAppender[E] {
  private class ProxyOutputStream(inner: OutputStream) extends OutputStream {
    private def choose: OutputStream = Logstream.box openOr inner

    def write(i: Int) {choose.write(i)}

    override def write(bytes: Array[Byte]) {choose.write(bytes)}

    override def write(bytes: Array[Byte], i: Int, i1: Int) {choose.write(bytes, i, i1)}

    override def flush() {choose.flush()}

    override def close() {choose.close()}
  }


  override def start() {
    super.start()
    val os = getOutputStream()
    setOutputStream(new ProxyOutputStream(os))
  }
}

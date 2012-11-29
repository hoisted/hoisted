package org.hoisted.lib


/*
 * Copyright 2012 The Netty Project
 *
 * The Netty Project licenses this file to you under the Apache License,
 * version 2.0 (the "License"); you may not use this file except in compliance
 * with the License. You may obtain a copy of the License at:
 *
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
* Unless required by applicable law or agreed to in writing, software
* distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
* WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
* License for the specific language governing permissions and limitations
* under the License.
*/

import java.net.InetSocketAddress;
import java.util.concurrent.Executors;

import org.jboss.netty.bootstrap.ServerBootstrap;
import org.jboss.netty.channel.socket.nio.NioServerSocketChannelFactory
import net.liftweb.http.{Req}
import java.io.File
import org.jboss.netty.buffer.ChannelBuffer
import org.jboss.netty.channel.ChannelFuture
import net.liftweb.util.Helpers
import net.liftweb.common.Box
;

class HttpStaticFileServer(port: Int) {

  def run(rootDir: File, setup: EnvironmentManager => EnvironmentManager) {
    // Configure the server.
    val bootstrap = new ServerBootstrap(
      new NioServerSocketChannelFactory(
        Executors.newCachedThreadPool(),
        Executors.newCachedThreadPool()));

    // Set up the event pipeline factory.
    bootstrap.setPipelineFactory(new HttpStaticFileServerPipelineFactory(rootDir, setup));

    // Bind and start to accept incoming connections.
    bootstrap.bind(new InetSocketAddress(port));
  }
}


import org.jboss.netty.buffer.ChannelBuffers;
import org.jboss.netty.channel.ChannelFutureListener;
import org.jboss.netty.channel.ChannelHandlerContext;
import org.jboss.netty.channel.ExceptionEvent;
import org.jboss.netty.channel.MessageEvent;
import org.jboss.netty.channel.SimpleChannelUpstreamHandler;
import org.jboss.netty.handler.codec.frame.TooLongFrameException;
import org.jboss.netty.handler.codec.http.DefaultHttpResponse;
import org.jboss.netty.handler.codec.http.HttpRequest;
import org.jboss.netty.handler.codec.http.HttpResponse;
import org.jboss.netty.handler.codec.http.HttpResponseStatus;
import org.jboss.netty.util.CharsetUtil;

import javax.activation.MimetypesFileTypeMap;
import java.io.File;
import java.io.UnsupportedEncodingException;
import java.net.URLDecoder;
import java.text.SimpleDateFormat;
import java.util.Calendar;
import java.util.Date;
import java.util.GregorianCalendar;
import java.util.Locale;
import java.util.TimeZone;

import org.jboss.netty.handler.codec.http.HttpHeaders.Names._;
import org.jboss.netty.handler.codec.http.HttpHeaders._;
import org.jboss.netty.handler.codec.http.HttpMethod._;
import org.jboss.netty.handler.codec.http.HttpResponseStatus._;
import org.jboss.netty.handler.codec.http.HttpVersion._;

/**
 * A simple handler that serves incoming HTTP requests to send their respective
 * HTTP responses.  It also implements {@code 'If-Modified-Since'} header to
 * take advantage of browser cache, as described in
 * <a href="http://tools.ietf.org/html/rfc2616#section-14.25">RFC 2616</a>.
 *
 * <h3>How Browser Caching Works</h3>
 *
 * Web browser caching works with HTTP headers as illustrated by the following
 * sample:
 * <ol>
 * <li>Request #1 returns the content of <code>/file1.txt</code>.</li>
 * <li>Contents of <code>/file1.txt</code> is cached by the browser.</li>
 * <li>Request #2 for <code>/file1.txt</code> does return the contents of the
 * file again. Rather, a 304 Not Modified is returned. This tells the
 * browser to use the contents stored in its cache.</li>
 * <li>The server knows the file has not been modified because the
 * <code>If-Modified-Since</code> date is the same as the file's last
 * modified date.</li>
 * </ol>
 *
 * <pre>
 * Request #1 Headers
 * ===================
 * GET /file1.txt HTTP/1.1
 *
 * Response #1 Headers
 * ===================
 * HTTP/1.1 200 OK
 * Date:               Tue, 01 Mar 2011 22:44:26 GMT
 * Last-Modified:      Wed, 30 Jun 2010 21:36:48 GMT
 * Expires:            Tue, 01 Mar 2012 22:44:26 GMT
 * Cache-Control:      private, max-age=31536000
 *
 * Request #2 Headers
 * ===================
 * GET /file1.txt HTTP/1.1
 * If-Modified-Since:  Wed, 30 Jun 2010 21:36:48 GMT
 *
 * Response #2 Headers
 * ===================
 * HTTP/1.1 304 Not Modified
 * Date:               Tue, 01 Mar 2011 22:44:28 GMT
 *
 * </pre>
 */


private[lib] object GlobalCache {

  var filter: ParsedFile => Boolean = null
  var pipeline: Box[List[ParsedFile]] => Box[List[ParsedFile]] = null

  var curPages: Map[PathAndSuffix, ParsedFile] = Map.empty
}

class HttpStaticFileServerHandler(rootDir: File, setup: EnvironmentManager => EnvironmentManager) extends SimpleChannelUpstreamHandler with
LazyLoggableWithImplicitLogger{

  def HTTP_DATE_FORMAT = "EEE, dd MMM yyyy HH:mm:ss zzz";

  def HTTP_DATE_GMT_TIMEZONE = "GMT";

  def HTTP_CACHE_SECONDS = 60;



  override def messageReceived(ctx: ChannelHandlerContext, e: MessageEvent): Unit = GlobalCache.synchronized {
    val request = e.getMessage().asInstanceOf[HttpRequest];
    if (request.getMethod() != GET) {
      sendError(ctx, METHOD_NOT_ALLOWED);
      return;
    }

    val path = request.getUri;
    if (path == null) {
      sendError(ctx, FORBIDDEN);
      return;
    }

    if (GlobalCache.pipeline eq null) GlobalCache.pipeline =   RenderPipeline.buildPreloaded(loadExternalSites = false,
      filterPages = x => GlobalCache.filter(x))

    val em = setup(new EnvironmentManager)

    Helpers.logTime("Whole render") {
    HoistedEnvironmentManager.doWith(em) {

      val files =  Helpers.logTime("Loading files") {
        LoadFiles(PathAndSuffix.onlyCurrent(GlobalCache.curPages)).apply(rootDir)
      }

      files.foreach(f => GlobalCache.curPages = PathAndSuffix.buildMap(f))

      val parsePath = Req.parsePath(path)

      def testPath(pf: ParsedFile): Boolean = {
        val ret =
          pf.fileInfo.pathAndSuffix.path == parsePath.partPath ||
            pf.fileInfo.pathAndSuffix.display == parsePath.wholePath.mkString("/", "/", "")

        if (ret) logger.info("Testing " + pf.fileInfo.pathAndSuffix.path + " against " + parsePath + " res " + ret)
        ret

      }

      GlobalCache.filter = testPath(_)

      val filtered = files.toList.flatten.filter(testPath(_)).filter{
        case o: HasHtml =>
          logger.info("Oh crap, Mr. Yak "+o.getClass.getName)
          false
        case x => true
      }

      logger.info("Filtered len is "+filtered.length)


      val pages =
      Helpers.logTime("Running render") {
        if (filtered.isEmpty) (GlobalCache.pipeline(files) openOr Nil) else {
        logger.info("Yay... no rendering")
        filtered
      }
      }
      val show = pages.filter(testPath(_)).headOption


      (show, show.flatMap(_.bytes)) match {
        case (None, _) => sendError(ctx, NOT_FOUND);
        case (_, None) => sendError(ctx, NOT_FOUND);
        case (Some(file), Some(bytes)) =>

          val response = new DefaultHttpResponse(HTTP_1_1, OK);
          setContentLength(response, bytes.length);

          logger.info("Writing " + bytes.length + " bytes ")

          val ch = e.getChannel();

          // Write the initial line and the header.
          ch.write(response);
          val cb = ChannelBuffers.copiedBuffer(bytes)
          val fut = ch.write(cb)
          logger.info("Wrote")
          fut.addListener(new ChannelFutureListener {
            def operationComplete(future: ChannelFuture) {
              logger.info("It's done")
            }
          })
      }
    }
    }
  }

  override def exceptionCaught(ctx: ChannelHandlerContext, e: ExceptionEvent) {
    val ch = e.getChannel();
    val cause = e.getCause();
    if (cause.isInstanceOf[TooLongFrameException]) {
      sendError(ctx, BAD_REQUEST);
      return;
    }

    cause.printStackTrace();
    if (ch.isConnected()) {
      sendError(ctx, INTERNAL_SERVER_ERROR);
    }
  }

  private def sanitizeUri(_uri: String): String = {
    var uri = _uri

    // Decode the path.
    try {
      uri = URLDecoder.decode(uri, "UTF-8");
    } catch {
      case e: UnsupportedEncodingException =>
        try {
          uri = URLDecoder.decode(uri, "ISO-8859-1");
        } catch {
          case e1: UnsupportedEncodingException =>
            throw new Error();
        }
    }

    // Convert file separators.
    uri = uri.replace('/', File.separatorChar);

    // Simplistic dumb security check.
    // You will have to do something serious in the production environment.
    if (uri.contains(File.separator + '.') ||
      uri.contains('.' + File.separator) ||
      uri.startsWith(".") || uri.endsWith(".")) {
      return null;
    }

    // Convert to absolute path.
    return System.getProperty("user.dir") + File.separator + uri;
  }

  private def sendError(ctx: ChannelHandlerContext, status: HttpResponseStatus) {
    val response = new DefaultHttpResponse(HTTP_1_1, status);
    response.setHeader(CONTENT_TYPE, "text/plain; charset=UTF-8");
    response.setContent(ChannelBuffers.copiedBuffer(
      "Failure: " + status.toString() + "\r\n",
      CharsetUtil.UTF_8));

    // Close the connection as soon as the error message is sent.
    ctx.getChannel().write(response).addListener(ChannelFutureListener.CLOSE);
  }

  /**
   * When file timestamp is the same as what the browser is sending up, send a "304 Not Modified"
   *
   * @param ctx
      * Context
   */
  private def sendNotModified(ctx: ChannelHandlerContext) {
    val response = new DefaultHttpResponse(HTTP_1_1, NOT_MODIFIED);
    setDateHeader(response);

    // Close the connection as soon as the error message is sent.
    ctx.getChannel().write(response).addListener(ChannelFutureListener.CLOSE);
  }

  /**
   * Sets the Date header for the HTTP response
   *
   * @param response
      * HTTP response
   */
  private def setDateHeader(response: HttpResponse) {
    val dateFormatter = new SimpleDateFormat(HTTP_DATE_FORMAT, Locale.US);
    dateFormatter.setTimeZone(TimeZone.getTimeZone(HTTP_DATE_GMT_TIMEZONE));

    val time = new GregorianCalendar();
    response.setHeader(DATE, dateFormatter.format(time.getTime()));
  }

  /**
   * Sets the Date and Cache headers for the HTTP Response
   *
   * @param response
      * HTTP response
   * @param fileToCache
      * file to extract content type
   */
  private def setDateAndCacheHeaders(response: HttpResponse, fileToCache: File) {
    val dateFormatter = new SimpleDateFormat(HTTP_DATE_FORMAT, Locale.US);
    dateFormatter.setTimeZone(TimeZone.getTimeZone(HTTP_DATE_GMT_TIMEZONE));

    // Date header
    val time = new GregorianCalendar();
    response.setHeader(DATE, dateFormatter.format(time.getTime()));

    // Add cache headers
    time.add(Calendar.SECOND, HTTP_CACHE_SECONDS);
    response.setHeader(EXPIRES, dateFormatter.format(time.getTime()));
    response.setHeader(CACHE_CONTROL, "private, max-age=" + HTTP_CACHE_SECONDS);
    response.setHeader(
      LAST_MODIFIED, dateFormatter.format(new Date(fileToCache.lastModified())));
  }

  /**
   * Sets the content type header for the HTTP Response
   *
   * @param response
      * HTTP response
   * @param file
      * file to extract content type
   */
  private def setContentTypeHeader(response: HttpResponse, file: File) {
    val mimeTypesMap = new MimetypesFileTypeMap();
    response.setHeader(CONTENT_TYPE, mimeTypesMap.getContentType(file.getPath()));
  }

}

/*
 * Copyright 2012 The Netty Project
 *
 * The Netty Project licenses this file to you under the Apache License,
 * version 2.0 (the "License"); you may not use this file except in compliance
 * with the License. You may obtain a copy of the License at:
 *
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
* Unless required by applicable law or agreed to in writing, software
* distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
* WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
* License for the specific language governing permissions and limitations
* under the License.
*/

import org.jboss.netty.channel.ChannelPipeline;
import org.jboss.netty.channel.ChannelPipelineFactory;
import org.jboss.netty.handler.codec.http.HttpChunkAggregator;
import org.jboss.netty.handler.codec.http.HttpRequestDecoder;
import org.jboss.netty.handler.codec.http.HttpResponseEncoder;
import org.jboss.netty.handler.stream.ChunkedWriteHandler;

class HttpStaticFileServerPipelineFactory(rootDir: File,
                                          setup: EnvironmentManager => EnvironmentManager) extends ChannelPipelineFactory {
  def getPipeline: ChannelPipeline = {
    val pipeline: ChannelPipeline = org.jboss.netty.channel.Channels.pipeline();


    pipeline.addLast("decoder", new HttpRequestDecoder());
    pipeline.addLast("aggregator", new HttpChunkAggregator(65536));
    pipeline.addLast("encoder", new HttpResponseEncoder());
    pipeline.addLast("chunkedWriter", new ChunkedWriteHandler());

    pipeline.addLast("handler", new HttpStaticFileServerHandler(rootDir, setup));
    pipeline;
  }
}
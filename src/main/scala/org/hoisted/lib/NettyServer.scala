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


import org.jboss.netty.channel.ChannelPipeline;
import org.jboss.netty.channel.ChannelPipelineFactory;
import org.jboss.netty.handler.codec.http.HttpChunkAggregator;
import org.jboss.netty.handler.codec.http.HttpRequestDecoder;
import org.jboss.netty.handler.codec.http.HttpResponseEncoder;
import org.jboss.netty.handler.stream.ChunkedWriteHandler;

import java.net.InetSocketAddress;
import java.util.concurrent.Executors;

import org.jboss.netty.bootstrap.ServerBootstrap;
import org.jboss.netty.channel.socket.nio.NioServerSocketChannelFactory
import net.liftweb.http.{Req}

import org.jboss.netty.channel.ChannelFuture
import net.liftweb.util.Helpers
import net.liftweb.common.{Full, Empty, Box}

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

import org.jboss.netty.handler.codec.http.HttpHeaders.Names._
import org.jboss.netty.handler.codec.http.HttpHeaders._
import org.jboss.netty.handler.codec.http.HttpMethod._
import org.jboss.netty.handler.codec.http.HttpResponseStatus._
import org.jboss.netty.handler.codec.http.HttpVersion._

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


private[lib] object GlobalCache {

  var filter: ParsedFile => Boolean = null
  var pipeline: Box[List[ParsedFile]] => Box[List[ParsedFile]] = null

  var curPages: Map[String, ParsedFile] = Map.empty

  var templateDir: Box[File] = Empty
}

class HttpStaticFileServerHandler(rootDir: File, setup: EnvironmentManager => EnvironmentManager) extends SimpleChannelUpstreamHandler with
LazyLoggableWithImplicitLogger {

  def HTTP_DATE_FORMAT = "EEE, dd MMM yyyy HH:mm:ss zzz";

  def HTTP_DATE_GMT_TIMEZONE = "GMT";

  def HTTP_CACHE_SECONDS = 60;


  override def messageReceived(ctx: ChannelHandlerContext, e: MessageEvent): Unit = GlobalCache.synchronized {
    val request = e.getMessage().asInstanceOf[HttpRequest];
    if (request.getMethod() != GET) {
      sendError(ctx, METHOD_NOT_ALLOWED);
      return;
    }

    val path = request.getUri.toLowerCase;
    if (path == null) {
      sendError(ctx, FORBIDDEN);
      return;
    }

    if (GlobalCache.pipeline eq null) GlobalCache.pipeline = RenderPipeline.buildPreloaded(loadExternalSites = false,
      filterPages = x => GlobalCache.filter(x))

    val em = setup(new EnvironmentManager)

    em.templateDir = GlobalCache.templateDir

    Helpers.logTime("Time to render: " + path) {
      HoistedEnvironmentManager.doWith(em) {

        val _files =
          LoadFiles(PathAndSuffix.onlyCurrent(GlobalCache.curPages)).apply(rootDir)

        _files.foreach(f => GlobalCache.curPages = PathAndSuffix.buildMap(f))

        val files = GlobalCache.templateDir match {
          case Full(file) => _files.map(f => HoistedUtil.loadFilesFrom(file, Map.empty).toList.flatten ::: f)
          case _ => _files
        }



        val parsePath = Req.parsePath(path)

        def testPath(pf: ParsedFile): Boolean = {
          val ret =
            pf.fileInfo.pathAndSuffix.path == parsePath.partPath ||
              pf.fileInfo.pathAndSuffix.display == parsePath.wholePath.mkString("/", "/", "")
          ret

        }

        GlobalCache.filter = testPath(_)

        val _filtered = files.toList.flatten.filter(testPath(_)).filter {
          case o: HasHtml => false
          case x => true
        }

        val filtered = _filtered.filter(f => f.fileInfo.pathAndSuffix.path == parsePath.partPath &&
          f.fileInfo.suffix == Some(parsePath.suffix).filter(_.length > 0)).headOption.map(v => List(v)) getOrElse _filtered

        val pages =
          if (filtered.isEmpty) (GlobalCache.pipeline(files) openOr Nil)
          else {
            filtered
          }

        val show = pages.filter(testPath(_)).headOption

        em.templateDir.foreach(d => GlobalCache.templateDir = Full(d))

        (show, show.flatMap(_.bytes)) match {
          case (None, _) => sendError(ctx, NOT_FOUND);
          case (_, None) => sendError(ctx, NOT_FOUND);
          case (Some(file), Some(bytes)) =>

            val response = new DefaultHttpResponse(HTTP_1_1, OK);
            setContentLength(response, bytes.length);
            file.fileInfo.file.foreach(f => {
              setContentTypeHeader(response, f)
            })

            val ch = e.getChannel();

            // Write the initial line and the header.
            ch.write(response);
            val cb = ChannelBuffers.copiedBuffer(bytes)
            val fut = ch.write(cb)
            fut.addListener(new ChannelFutureListener {
              def operationComplete(future: ChannelFuture) {

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
    if (file.getPath.toLowerCase().endsWith(".css")) {
      response.setHeader(CONTENT_TYPE, "text/css")

    } else {
      val filePath = file.getPath().toLowerCase
      var theType = mimeTypesMap.getContentType(filePath);

      if (theType == "application/octet-stream") {
        if (filePath.endsWith(".md") || filePath.endsWith(".html")) {
          theType = "text/html"
        } else if (filePath.endsWith(".js")) {
          theType = "application/x-javascript"
        }
      }

      response.setHeader(CONTENT_TYPE, theType);
    }
  }

}

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
package scrawla

import java.io.ByteArrayInputStream
import java.nio.charset.StandardCharsets
import scala.xml.Node
import scala.xml.parsing.NoBindingFactoryAdapter
import org.xml.sax.InputSource
import org.ccil.cowan.tagsoup.jaxp.SAXFactoryImpl
import dispatch._
import Defaults._

trait HtmlResult {
  def requestUri: String
}

/**
 * ADT for an invalid result (404, 403, 401, etc)
 */
case class InvalidResponse (requestUri: String) extends HtmlResult

/**
 * ADT for a successfully loaded result (200)
 *
 * @param content
 * @param requestUri
 */
case class OkResponse (content: Node, requestUri: String) extends HtmlResult

/**
 *
 * ADT for a malformed url. This should be done synchronously though (below) but for now it isn't
 *
 * @param requestUri
 */
case class MalformedUrl (requestUri: String) extends HtmlResult

case class RequestTimeout (requestUri: String) extends HtmlResult

/**
 * Static namespace to load HTML as XML
 */
object HtmlLoader {

  lazy val adapter = new NoBindingFactoryAdapter
  //parser to convert HTML to well-formed XML
  lazy val parser = (new SAXFactoryImpl).newSAXParser

  /**
   * Connects to a uri (assumed to be over http) and loads HTML into a well-formed {scala.xml.Node}
   *
   * @param uri
   * @return the node to return
   */
  def get(uri: String): Future[HtmlResult] = {

    try {

      val http = Http.configure(
        _.setFollowRedirects(true).setIdleConnectionTimeoutInMs(7000).setRequestTimeoutInMs(7000).setConnectionTimeoutInMs(7000))

      val request = http(url(uri) OK as.String)

      for(r <- request) yield {
        r match {
          case s:String =>
            val inputStream = new ByteArrayInputStream(s.getBytes(StandardCharsets.UTF_8))
            OkResponse(adapter.loadXML(new InputSource(inputStream), parser), uri)
          case _ =>
            InvalidResponse(uri)
        }
      }
    } catch {
      case _:Throwable =>
        println("badness")
        Future{MalformedUrl(uri)}
    }
  }
}
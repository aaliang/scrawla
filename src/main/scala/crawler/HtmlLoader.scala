package crawler

import java.io.ByteArrayInputStream
import java.nio.charset.StandardCharsets
import scala.xml.Node
import scala.xml.parsing.NoBindingFactoryAdapter
import org.xml.sax.InputSource
import org.ccil.cowan.tagsoup.jaxp.SAXFactoryImpl
import dispatch._
import Defaults._

trait HtmlResult {
  def content: Node
  def requestUri: String
}

/**
 * ADT for an invalid result (404, 403, 401, etc)
 */
case class InvalidResponse (requestUri: String) extends HtmlResult {
  def content:Node = null
}

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
case class MalformedUrl (requestUri: String) extends HtmlResult {
  def content:Node = null
}

/**
 * Static namespace to load HTML as XML
 */
object HtmlLoader {

  lazy val adapter = new NoBindingFactoryAdapter
  //parser to convert HTML to well-formed XML
  lazy val parser = (new SAXFactoryImpl).newSAXParser

  val http = Http.configure(
    _.setFollowRedirects(true)
    .setAllowPoolingConnection(true).setRequestTimeoutInMs(15000).setConnectionTimeoutInMs(15000))

  /**
   * Connects to a uri (assumed to be over http) and loads HTML into a well-formed {scala.xml.Node}
   *
   * @param uri
   * @return the node to return
   */
  def get(uri: String): Future[HtmlResult] = {

    try {

      val request = http(url(uri) OK as.String).option

      (for (c <- request) yield {
        c match {
          case Some(s) =>
            val inputStream = new ByteArrayInputStream(s.getBytes(StandardCharsets.UTF_8))
            OkResponse(adapter.loadXML(new InputSource(inputStream), parser), uri)

          case None =>
            InvalidResponse(uri)
        }
      })
    } catch {
      case e:Throwable => Future{MalformedUrl(uri)}
    }
  }

}
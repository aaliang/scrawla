package crawler

import java.io.ByteArrayInputStream
import java.nio.charset.StandardCharsets
import scala.xml.Node
import scala.xml.parsing.NoBindingFactoryAdapter
import org.xml.sax.InputSource
import org.ccil.cowan.tagsoup.jaxp.SAXFactoryImpl
import dispatch._
import Defaults._

trait HtmlResult {def content: Node}

/**
 * ADT for an invalid result (404, 403, 401, etc)
 */
case class InvalidResult () extends HtmlResult { def content:Node = null}

/**
 * ADT for a successfully loaded result (200)
 *
 * @param content
 */
case class ValidResult (content: Node) extends HtmlResult

/**
 *
 * ADT for a redirected result (301, 303, etc)
 *
 * @param content
 * @param redirectTo
 */
case class RedirectResult (content: Node, redirectTo: String ) extends HtmlResult

/**
 * Static namespace to load HTML as XML
 */
object HtmlLoader {

  lazy val adapter = new NoBindingFactoryAdapter
  //parser to convert HTML to well-formed XML
  lazy val parser = (new SAXFactoryImpl).newSAXParser

//  /**
//   * Connects to a uri (assumed to be over http) and loads HTML into a well-formed {scala.xml.Node}
//   *
//   * @param uri
//   * @return the node to return
//   */
//  def get(uri: String): Future[HtmlResult] = {
//    //FYI - This completely blows by redirects (301, 302)
//    val request = http.configure(_.setFollowRedirects(true))(url(uri) OK as.String)
//
//    (for (c <- request) yield {
//      val inputStream = new ByteArrayInputStream(c.getBytes(StandardCharsets.UTF_8))
//      ValidResult(adapter.loadXML(new InputSource(inputStream), parser))
//    }).recover {
//      case _ => InvalidResult()
//    }
//  }

  val http = Http.configure(_.setAllowPoolingConnection(true)
    .setFollowRedirects(true))

  /**
   * Connects to a uri (assumed to be over http) and loads HTML into a well-formed {scala.xml.Node}
   *
   * @param uri
   * @return the node to return
   */
  def get(uri: String): HtmlResult = {

    val request = http(url(uri) OK as.String)

    try {
      val res = request()
      val inputStream = new ByteArrayInputStream(res.getBytes(StandardCharsets.UTF_8))
      ValidResult(adapter.loadXML(new InputSource(inputStream), parser))
    } catch {
      case _:Throwable => InvalidResult()
    }

  }

}
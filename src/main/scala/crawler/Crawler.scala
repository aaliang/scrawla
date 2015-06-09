package crawler

import akka.actor.{ActorContext, Props, ActorSystem}
import akka.util.Timeout
import com.typesafe.config.ConfigFactory
import scala.concurrent.duration._
import scala.xml.Node
import akka.pattern.{AskTimeoutException, ask}
import scala.concurrent.ExecutionContext.Implicits.global

/**
 * Datatype representing an html page
 *
 * @param uri the path to the page, uniquely identified
 * @param anchors edges pointing to other pages, identified by their own uris
 * @param links static content <link> tags e.g. external css stylesheets
 * @param scripts static content <script> tags e.g. text/javascript
 * @param images static content <image> tags
 */
case class Page(uri: String, anchors: List[String], links: List[String], scripts: List[String], images: List[String])

trait PathType

case class RelPath(path: String, fromPath:String) extends PathType

case class AbsPath(path: String) extends PathType

case object NonePath extends PathType

/**
 * Currently the only crawler type. Will only follow links on the same domain
 * @param baseUrl
 * @param protocol
 */
class DefaultCrawler (baseUrl: String, protocol: String = "http://") {

  //child actor system
  val system = ActorSystem("crawlerSystem", ConfigFactory.load.getConfig("crawler"))

  //implicits
  implicit val timeout = Timeout(8 seconds)

  //immutables
  val httpRequester = system.actorOf(Props[RequesterActor].withDispatcher("request-dispatcher"), name = "requester")
  val OUTBOUND_LIMIT = 6

  //mutable
  val nodeMap = scala.collection.mutable.Map[String, Page]()
  val outboundRequests = scala.collection.mutable.Set[String]()
  //for now, the mutables+refs are done oo style.

  //mutable references
  var queued = scala.collection.mutable.LinkedHashSet[String]()

  /**
   * Delegates outbound http requests (up to the outbound limit quota) to the {httpRequester}
   */
  def delegateRequestsUpToQuota {
    val (take, remainder) = queued.splitAt(OUTBOUND_LIMIT - outboundRequests.size)
    queued = remainder
    take.foreach(s => visitPage(UrlUtils.getNormalizedCandidates(s)))
  }

  /**
   * Delegates visiting a {uri} to the {httpRequester}
   *
   * @param fallbacks the uri to visit
   */
  def visitPage(fallbacks: List[String]) {
    val fut = httpRequester ? HttpRequest(fallbacks)

    val requestingUrl = fallbacks.head
    outboundRequests.add(requestingUrl)

//    println(outboundRequests)

    fut.onSuccess {
      /**
       * If we receive an {OkResponse}, that's great; we'll get all relevant items from the html and try to
       * visit any matched anchor links
       */
      case OkResponse(pageNode, uri) =>
        outboundRequests.remove(requestingUrl)
        queued.remove(requestingUrl)
        try {
          val page = processHtmlElements(pageNode, requestingUrl, accumulatePartial(baseUrl, requestingUrl) _)
          val notVisitedYet = page.anchors.filter(!nodeMap.contains(_)).filter(!queued.contains(_))
          nodeMap.put(requestingUrl, page)
          queued ++= notVisitedYet
        } finally {
          delegateRequestsUpToQuota
        }

      /**
       * If we receive an {InvalidResponse}, that's fine; we'll just mark it as null such that we don't try again
       */
      case BadResponse(uri, fallbacks) =>
        nodeMap.put(requestingUrl, null) //todo: need a link

        outboundRequests.remove(requestingUrl)
        queued.remove(requestingUrl)

        val newFallbacks = fallbacks.tail
        if (!newFallbacks.isEmpty){
          visitPage(newFallbacks)
        } else {
          delegateRequestsUpToQuota
        }

      case rest =>
        outboundRequests.remove(requestingUrl)
        queued.remove(requestingUrl)
        delegateRequestsUpToQuota
    }

    fut.onFailure {
      case e:AskTimeoutException =>
        println("ask timeout: " + requestingUrl)
        outboundRequests.remove(requestingUrl)
        queued.remove(requestingUrl)
        delegateRequestsUpToQuota
      case other:Throwable =>
        outboundRequests.remove(requestingUrl)
        queued.remove(requestingUrl)
        delegateRequestsUpToQuota
    }
  }

  /**
   * Default url normalization strategy
   */
  val defaultNormalizationFn = (url:String) => {
    UrlUtils.toNormalizationStrategy(url, Seq(
      UrlUtils.useLowerCase _,
      UrlUtils.ensureProtocol(protocol) _
    ))
  }

  /**
   * Starts crawling on the top level domain
   */
  def start = {
    val url = protocol + "www." + baseUrl
    visitPage(List(url))
  }

  /**
   * Given a valid xhtml node, looks for anchors, links, scripts, and images and summarizes it to a {Page} object
   *
   * @param xhtml xml.Node representation of the html
   * @param traversedUri the unique identifier corresponding to this page
   * @param accumulateOption a function which takes a string corresponsing to a resource as input and determines whether
   *                         it should be included in the page summary. If it should, it (should) normalize it, and
   *                         return it wrapped as Some(String), otherwise it returns {None}
   *
   */
  def processHtmlElements(xhtml: Node, traversedUri: String, accumulateOption: (String) => Option[String]):Page = {

    //An empty immutable Set as base case
    val init = Set.empty[String]

    // for those unfamiliar with scala, the following involves the XML processing library
    // syntactic sugar - \\ and \ are functions mimicking XPath operators, (dotting methods in Scala is implicit)

    //look for anchor nodes (<a>'s)
    val anchors = (xhtml \\ "a").foldLeft(init) { (acc, anchor) =>
      acc ++ accumulateOption((anchor \ "@href").text) }

    //for link nodes (<link>'s) canonical links and shortlinks will be considered static
    val links = (xhtml \\ "link").foldLeft(init) { (acc, link) =>
      acc ++ accumulateOption((link \ "@href").text) }
    //script tags
    val scripts = (xhtml \\ "script").foldLeft(init) { (acc, script) =>
      acc ++ accumulateOption((script \ "@src").text) }
    //image tags
    val imgs = (xhtml \\ "img").foldLeft(init) { (acc, img) =>
      acc ++ accumulateOption((img \ "@src").text) }

    Page(traversedUri, anchors.toList, links.toList, scripts.toList, imgs.toList)
  }

  /**
   * A curried function representing if a path should be traversable.
   * While it is not necessary right now for the benefits it provides, it provides
   * very interesting flexibility if I want to extend this in the future
   *
   * @param basePath base domain or path we want to consider. This can be a deep path e.g. www.nytimes.com/a/b/c
   *                 although, a back-edge (link) back to www.nytimes.com is extremely likely and can render this moot
   * @param candidatePath the path that we're considering traversing
   * @return
   */
  def accumulatePartial (basePath:String, traversedUri:String)(candidatePath:String) : Option[String] = {

    //ok, this only accepts http and http - does not allow ftp etc
    val acceptAbs = List(
      s"http://www.$basePath",
      s"https://www.$basePath"
    )

    //regexes are unmaintainable!
    normalize(candidatePath.trim match {
      case "" =>
        NonePath
      case s if (s.startsWith("javascript:")
        || s.startsWith("mailto:")
        ||s.startsWith("tel:")
        || s.startsWith("#")) =>
        NonePath
      case s if s.startsWith("//") => s.drop(2) match { //protocol agnostic
        case ss if ss.startsWith(basePath) =>
          AbsPath(ss)
        case _ =>
          NonePath
      }
      case s if (s.startsWith("/")) =>
        AbsPath(basePath + s)
      case s if (s.startsWith(s"http://$basePath")
        || s.startsWith(s"https://$basePath")
        || s.startsWith(s"www.$basePath")
        || acceptAbs.exists(s.startsWith(_))) =>
        AbsPath(s)
      case s if s.startsWith(".") =>
        RelPath(s, traversedUri)
      case s if (Seq("http://", "https://", "www.").exists(s.startsWith(_))) =>
        NonePath
      case s => RelPath(s, traversedUri)
    })
  }

  def normalize (candidate:PathType):Option[String] = candidate match {
    case AbsPath(path) =>
      Some(UrlUtils.normalize(path, defaultNormalizationFn))
    case RelPath(path, fromPath) =>
      val relPath = UrlUtils.resolveRelativeOptimistic(fromPath, path)
      Some(UrlUtils.normalize(relPath, defaultNormalizationFn))
    case NonePath => None
  }

}

object Crawler {
  /**
   * The main entry point of the crawler
   * @param args
   */
  def main(args: Array[String]) {

    //TODO: arg length checking
    val domain = args(0).trim
    val protocol = "http://" //for now, restrict initial domain to http

    val crawler = new DefaultCrawler(domain, protocol)

    crawler.start

  }
}
package crawler

import akka.actor.{ActorSystem, Props, ActorRefFactory}
import akka.util.Timeout
import com.typesafe.config.ConfigFactory
import scala.concurrent.duration._
import scala.xml.Node
import akka.pattern.{AskTimeoutException, ask}
import scala.concurrent.ExecutionContext.Implicits.global

//The following file contains traits and classes governing how a generic crawler should behave
//and interact with its minion actor(s)

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

/**
 * Common base trait for a path (uri)
 */
trait PathType

/**
 * A relative path encountered in the wild e.g. ../static/my-js.js or static/my.js
 *
 * @param path the path, how it appears to
 * @param fromPath context with which path is relative to
 */
case class RelPath(path: String, fromPath:String) extends PathType

/**
 * An absolute path. This should either fully qualified e.g. http://something.com/static/my.js or from the implied
 * root directory /static/my.js Although if the Crawler is extended, the definition might change to fit the implementer
 * @param path
 */
case class AbsPath(path: String) extends PathType

/**
 * A nonexistent path, used as an intermediary for paths we might want to ignore
 */
case object NonePath extends PathType

/**
 * Base contract for a crawler. This contains default implementation about how a directory structure should be traversed
 * as well as delegation logic to resource requesters. Scraping of html elements is also contained in here
 * They can, however be overridable by subtyping
 */
trait BaseCrawler {
  //needs a system
  val system:ActorRefFactory
  //the base domain to crawl
  val baseUrl:String
  //the initial protocol to make the requests, and to default to if ambiguously normalized
  val protocol:String

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
  def pathToFollow (basePath:String, traversedUri:String)(candidatePath:String) : Option[String]

  //implicits
  implicit val timeout = Timeout(8 seconds)

  //immutables
  lazy val httpRequester = system.actorOf(Props[RequesterActor].withDispatcher("request-dispatcher"), name = "requester")

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
   * Given a valid xhtml node, looks for anchors, links, scripts, and images and summarizes it to a {Page} object
   *
   * @param xhtml xml.Node representation of the html
   * @param traversedUri the unique identifier corresponding to this page
   * @param convertIfElligible a function which takes a string corresponsing to a resource as input and determines whether
   *                         it should be included in the page summary. If it should, it (should) normalize it, and
   *                         return it wrapped as Some(String), otherwise it returns {None}
   *
   */
  def processHtmlElements(xhtml: Node, traversedUri: String, convertIfElligible: (String) => Option[String]):Page = {

    //An empty immutable Set as base case
    val init = Set.empty[String]

    // for those unfamiliar with scala, the following involves the XML processing library
    // syntactic sugar - \\ and \ are functions mimicking XPath operators, (dotting methods in Scala is implicit)

    //look for anchor nodes (<a>'s)
    val anchors = (xhtml \\ "a").foldLeft(init) { (acc, anchor) =>
      acc ++ convertIfElligible((anchor \ "@href").text) }

    //for link nodes (<link>'s) canonical links and shortlinks will be considered static
    val links = (xhtml \\ "link").foldLeft(init) { (acc, link) =>
      acc ++ convertIfElligible((link \ "@href").text) }
    //script tags
    val scripts = (xhtml \\ "script").foldLeft(init) { (acc, script) =>
      acc ++ convertIfElligible((script \ "@src").text) }
    //image tags
    val imgs = (xhtml \\ "img").foldLeft(init) { (acc, img) =>
      acc ++ convertIfElligible((img \ "@src").text) }

    Page(traversedUri, anchors.toList, links.toList, scripts.toList, imgs.toList)
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
          val page = processHtmlElements(pageNode, requestingUrl, pathToFollow(baseUrl, requestingUrl) _)
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
   * Starts crawling on the top level domain
   */
  def start = {
    //for now, always add the www (:O)
    val url = protocol + "www." + baseUrl
    visitPage(List(url))
  }
}

/**
 * Stacks a NEW ActorSystem onto the instance
 */
trait EmbeddedCrawler {
  val system = ActorSystem("crawlerSystem", ConfigFactory.load.getConfig("crawler"))
}
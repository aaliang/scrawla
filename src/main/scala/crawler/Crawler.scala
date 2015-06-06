package crawler

import scala.xml.Node


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

class Crawler (baseUrl: String, protocol: String = "http://") {

  sealed case class RelPath(path: String)
  sealed case class AbsPath(path: String)
  sealed case class NonePath()

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
  def accumulatePartial(basePath:String, traversedUri:String)(candidatePath:String) : Option[String] = {

    //ok, this only accepts http and http - does not allow ftp etc

    val acceptAbs = List(
      s"http://www.$basePath",
      s"https://www.$basePath"
    )

    val normalizationFn = (url:String) => {
      UrlPathUtils.toNormalizationStrategy(url, Seq(
        UrlPathUtils.useLowerCase _,
        UrlPathUtils.ensureProtocol(protocol) _
      ))
    }

    //regexes are unmaintainable!
    val cand = candidatePath.trim match {
      case "" => NonePath
      case s if s.startsWith("#") => NonePath
      case s if s.startsWith("javascript:") => NonePath
      case s if s.startsWith("//") => s.drop(2) match { //protocol agnostic
        case ss if ss.startsWith(basePath) => AbsPath(ss)
        case _ => NonePath }
      case s if (s.startsWith("/")) => AbsPath(basePath + s)
      case s if (s.startsWith(s"http://$basePath")
        || s.startsWith(s"https://$basePath")
        || s.startsWith(s"www.$basePath")
        || acceptAbs.exists(s.startsWith(_))) => AbsPath(s)
      case s if s.startsWith(".") => RelPath(s)
      case s => s match {
        case ss if (List("http://", "https://", "www.").exists(ss.startsWith(_))) =>
          NonePath
        case ss =>
          RelPath(ss)
      }
    }

    cand match {
      case AbsPath(path) =>
        Some(UrlPathUtils.normalize(path, normalizationFn))
      case RelPath(s) =>
        val relPath = UrlPathUtils.resolveRelativeOptimistic(traversedUri, s)

        Some(UrlPathUtils.normalize(relPath, normalizationFn))
      case NonePath => None
    }
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
   * Traverses a path and continues along its edges until the dependency graph ends.
   * Discovery is done depth-first if done serially
   *
   * @param path
   */
  def traverse(path: String) = {

    //within here we have some mutable state. it is possible to do this with immutable collections and tailrec but that
    // is a pain in the ass and pretty uneffish... plus it would be impossible to follow

    val intransit = scala.collection.mutable.Set[String]()
    val stack = scala.collection.mutable.Stack[String]()
    val nodeMap = scala.collection.mutable.Map[String, Page]()

    def visitNodePromise (res:Option[Page], triedUri: String) {
      res match {
        case Some(page) =>
          val nc = page.anchors.filter(!nodeMap.contains(_))
            .filter(!intransit.contains(_))

          intransit ++= nc
          stack.pushAll(nc)

          nodeMap.put(triedUri, page)
          intransit.remove(triedUri)
        case None =>
          nodeMap.put(triedUri, null)
      }
    }

    stack.push(path)

    val MAX_CONQ = 1

    while (!stack.isEmpty) {

      val items = stack.take(MAX_CONQ).toList
      (1 to items.length).foreach(stack.pop())

      items.foreach(subpath => {
        println("visiting " + subpath)
        val stuff = visitPage(subpath)
        visitNodePromise(stuff, subpath)
      })
    }
  }

  /**
   * Visits a uri. If it successfully visits it, it will pass on the contents of the page as a {Page} instance
   * wrapped by {Some}, otherwise it will yield a {None}
   *
   * @param uri the uri to visit
   * @return {Option} wrapping {Page}
   */
  def visitPage(uri: String): Option[Page] = {

    val uriBinded = uri

    HtmlLoader.get(uri) match {
      case res:ValidResult =>
        Some(processHtmlElements(res.content, uriBinded, accumulatePartial(baseUrl, uriBinded) _))
      case res:InvalidResult =>
        None
      case _ =>
        None
    }
  }

  /**
   * Kicks off crawling on the instance
   */
  def traverseHead = {
    traverse(protocol + "www." + baseUrl)
  }
}

object Crawler {
  /**
   * The main entry point of the crawler
   * @param args
   */
  def main(args: Array[String]) {
    val vertex = (new Crawler("digitalocean.com", "https://")).traverseHead
  }
}
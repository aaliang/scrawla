package scrawla

/**
 * Default Crawler implementation. This will only follow links pointing to the same domain
 *
 * @param url
 * @param prtcl
 */
class DefaultCrawler(url: String, prtcl: String) extends BaseCrawler with EmbeddedCrawler {

  val baseUrl = url
  val protocol = prtcl

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
  def pathToFollow (basePath:String, traversedUri:String) (candidatePath:String) : Option[String] = {

    //ok, this only accepts http and http - does not allow ftp etc
    val acceptAbs = Seq(
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

  /**
   * Default url normalization strategy
   */
  val defaultNormalizationFn = (url:String) => {
    UrlUtils.toNormalizationStrategy(url, Seq(
      UrlUtils.useLowerCase _,
      UrlUtils.ensureProtocol(protocol) _
    ))
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
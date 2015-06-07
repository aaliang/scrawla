package crawler

import scalax.file.Path

/**
 * Static namespace containing functions for dealing with url paths.
 * Adapts scalax.file.Path for use with web urls
 *
 */
object UrlUtils {

  /**
   * Splits a path into a (protocol, base) tuple
   * @param path
   * @return
   */
  def extractProtocolAndBase (path:String): (String, String) = path match {
    case s if s.startsWith("http://") => ("http://", s.drop("http://" length))
    case s if s.startsWith("https://") => ("https://", s.drop("https://" length))
  }

  /**
   * Resolves two paths. The {from} path is assumed to be an absolute url. The {to} path is relative
   * to the from url.
   *
   * @param from the base absolute e.g. "http://google.com/about"
   * @param to the relative path from the base e.g. "../maps"
   * @return
   */
  def resolveRelative (from: String, to: String): String = {

    val (protocol, base) = extractProtocolAndBase(from)

    protocol + Path.fromString(base)
                    .resolve(Path.fromString(to))
                    .normalize
                    .path

  }

  /**
   * Resolves two paths. The {from} path is assumed to be an absolute url. The {to} path is relative
   * to the from url.
   *
   * @param from the base absolute e.g. "http://google.com/about"
   * @param to the relative path from the base e.g. "../maps"
   * @return
   */
  def resolveRelativeOptimistic (from: String, to: String): String = {

    val (protocol, base) = extractProtocolAndBase(from)

    //this is a naïve approach. this will fall apart if the top-level domain happens to be any of the following
    //also, there is no way to tell if a file maps to a directory (backed by a default) without making the request
    val nbase = base match {
      case b if possiblyAFile(b) => b.reverse.dropWhile(c => c != '/').reverse
      case b => b
    }

    protocol + Path.fromString(nbase)
      .resolve(Path.fromString(to))
      .normalize
      .path

  }

  /**
   * Checks if a path is possibly mapped to a file, based purely on the name
   * @param path
   * @return
   */
  def possiblyAFile (path:String):Boolean = {
    if (path.endsWith(".html")
      || path.endsWith(".htm")
      || path.endsWith(".jsp")
      || path.endsWith(".asp")
      || path.endsWith(".aspx")) {
      true
    } else {
      false
    }
  }


  def normalize (path: String, normalizationFn: String => String): String = {
    normalize(normalizationFn(path))
  }

  /**
   * Normalizes a path using the default normalization method
   *
   * @param path the base absolute e.g. "http://google.com/./about"
   * @return
   */
  def normalize (path: String) : String = {
    val (protocol, base) = extractProtocolAndBase(path)

    protocol + Path.fromString(base)
      .normalize
      .path

  }

  def toNormalizationStrategy (url: String, f: String => String): String = f(url)

  def toNormalizationStrategy (url: String, f: Seq[String => String]): String = {
    f.foldLeft(url)((u, a) => { a(u) })
  }

  def ensureProtocol(baseProtocol: String)(path: String) = {
    path match {
      case s if List("http://", "https://").exists(s.startsWith(_)) => s
      case _ => baseProtocol + path
    }
  }

  /**
   * Returns the input as lower case
   * @param path the input
   * @return path as lower case
   */
  def useLowerCase (path: String) = {
    path.toLowerCase
  }

  /**
   * Appends a trailing backslash to a string if it does not already end with one.
   *
   * @param path
   * @return
   */
  def addTrailingBackslash (path: String) = path match {
    case s if s.endsWith("/") => s
    case s => s + "/"
  }

  /**
   * Adds www if it does not appear in the path
   * This is somewhat naïve as well, and it will do stuff like http://api.google.com -> http://www.api.google.com
   *
   * @param path assumed to start with http:// or https://
   * @return
   */
  def addWww (path:String) = path match {
    case s if (s.startsWith("http://www.") || s.startsWith("https://www.")) => s
    case s if (s.startsWith("http://")) || s.startsWith("https://") => {
      val (protocol, base) = extractProtocolAndBase(s)
      protocol + "www." + base
    }
  }

  /**
   * Removes www from a string (if it exists) otherwise returns the input
   *
   * @param path
   * @return
   */
  def removeWwwFromPathWithProtocol (path:String) = path match {
    case s if s.startsWith("http://www.") =>
      "http://" + s.drop("http://www." length)
    case s if s.startsWith("https://www.") =>
      "https://" + s.drop("https://www." length)
    case s => s
  }

  /**
   * Given a path, tries to provide normalized possibilities.
   *
   * ex.
   *
   * http://mysite.com/a
   * http://www.mysite.com/a
   * http://mysite.com/a/
   *
   * are all possible candidates which may or may not point to the same resource.
   * @param path
   */
  def getNormalizedCandidates(path:String):List[String] = {

    var list = List[String]()

    val withWww = addWww(path)
    list ::= withWww

    val withoutWww = removeWwwFromPathWithProtocol(path)
    list ::= withoutWww

    if (!possiblyAFile(path)) {
      //i think we'll only add backslashes, don't remove them if provided
      val woatb = addTrailingBackslash(withoutWww)
      if (woatb != withoutWww) {
        list ::= woatb
      }
      val watb = addTrailingBackslash(withWww)
      if (watb != withWww) {
        list ::= watb
      }
    }

    list
  }
}

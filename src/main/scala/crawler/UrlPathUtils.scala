package crawler

import scalax.file.Path

/**
 * Static namespace containing functions for dealing with url paths.
 * Adapts scalax.file.Path for use with web urls
 *
 */
object UrlPathUtils {

  /**
   * Resolves two paths. The {from} path is assumed to be an absolute url. The {to} path is relative
   * to the from url.
   *
   * @param from the base absolute e.g. "http://google.com/about"
   * @param to the relative path from the base e.g. "../maps"
   * @return
   */
  def resolveRelative (from: String, to: String): String = {

    //Path will reduce double slashes to single slashes. This is unideal
    //when dealing with web urls.
    val (protocol, base) = from match {
      case s if s.startsWith("http://") => ("http://", s.drop("http://" length))
      case s if s.startsWith("https://") => ("https://", s.drop("https://" length))
    }

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

    //Path will reduce double slashes to single slashes. This is unideal
    //when dealing with web urls.
    val (protocol, base:String) = from match {
      case s if s.startsWith("http://") => ("http://", s.drop("http://" length))
      case s if s.startsWith("https://") => ("https://", s.drop("https://" length))
    }

    //this is a naïve approach. this will fall apart if the top-level domain happens to be any of the following
    //also, there is no way to tell if a file maps to a directory (backed by a default) without making the request
    val nbase = base match {
      case b if (b.endsWith(".html")
        || b.endsWith(".htm")
        || b.endsWith(".jsp")
        || b.endsWith(".asp")
        || b.endsWith(".aspx")) => b.reverse.dropWhile(c => c != '/').reverse
      case b => b
    }

    protocol + Path.fromString(nbase)
      .resolve(Path.fromString(to))
      .normalize
      .path

  }


  def normalize (path: String, normalizationFn: String => String): String = {
    normalize(normalizationFn(path))
  }

  /**
   * Normalizes a path
   *
   * @param path the base absolute e.g. "http://google.com/./about"
   * @return
   */
  def normalize (path: String) : String = {

    //Path will reduce double slashes to single slashes. This is unideal
    //when dealing with web urls.
    val (protocol, base) = path match {
      case s if s.startsWith("http://") => ("http://", s.drop("http://" length))
      case s if s.startsWith("https://") => ("https://", s.drop("https://" length))
    }

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

  //there is no way to tell for sure if the filename part of a url refers to a directory or file without
  //actually fetching it... we can infer the type based on the file extension... but that is still a naive assumption
  //as there are no guarantees
  def addTrailingBackslash (path: String) = {
    path match {
      case s if s.endsWith("/") => s
      case s => s + "/"
    }
  }
}

package crawler

import org.scalatest.{BeforeAndAfterEach, BeforeAndAfter, FunSpec}


class CrawlerSpec extends FunSpec with BeforeAndAfter with BeforeAndAfterEach {

  val base = "mysite.com"
  val crawler = new Crawler(base, "http://")

  describe ("accumulatePartial tests") {
    val getIfElligible = crawler.accumulatePartial(base, "http://www.mysite.com/a/") _

    it("ignores #") {
      assert(getIfElligible("#") === None)
      assert(getIfElligible("#something") === None)
    }
    //TODO: speaking of #, probably want to have the Crawler ignore fragments identifiers in general

    it("ignores things that start with 'javascript:'") {
      assert(getIfElligible("javascript:") === None)
    }

    it("normalizes relative paths (same directory)") {
      assert(getIfElligible("my.js") == Some("http://www.mysite.com/a/my.js"))
      assert(getIfElligible("./my.js") == Some("http://www.mysite.com/a/my.js"))
    }

    it("normalizes relative paths (parent directory)") {
      assert(getIfElligible("../my.js") == Some("http://www.mysite.com/my.js"))
    }

    it("accepts root directories") {
      //keeping this as failed for now because there are a couple cases that will need to be resolved
      assert(getIfElligible("/my.js") == Some("http://www.mysite.com/my.js"))
    }

  }

  describe ("parses a page containing a single link and nothing else") {

    //yeah... xml is a first class citizen, deal with it
    val page =
    <html>
      <a href="/my/page"></a>
    </html>

    val accumulateOptionFn = crawler.accumulatePartial(base, "http://www.mysite.com") _
    val pageSummary = crawler.processHtmlElements(page, "http://www.mysite.com", accumulateOptionFn)

    it ("detects the correct number of resources") {
      assert(pageSummary.anchors.length === 1)
      assert(pageSummary.images.isEmpty)
      assert(pageSummary.links.isEmpty)
      assert(pageSummary.scripts.isEmpty)
    }

    it ("normalizes the anchor correctly") {
      assert(pageSummary.anchors(0) === "http://mysite.com/my/page")
    }

    it ("sets the uri to exactly what was passed in") {
      assert(pageSummary.uri === "http://www.mysite.com")
    }
  }

  it ("ignore # anchors") {

    //yeah... xml is a first class citizen, deal with it
    val page =
      <html>
        <a href="#"></a>
        <a href="#stuff"></a>
      </html>

    val accumulateOptionFn = crawler.accumulatePartial(base, "http://www.mysite.com") _
    val pageSummary = crawler.processHtmlElements(page, "http://www.mysite.com", accumulateOptionFn)

    assert(pageSummary.anchors.isEmpty)
  }

  describe ("paths pointing to same domain are normalized") {
    val page =
    <html>
      <head>
        <link href="http://notmysite.com/styles.css"></link>
        <link href="//notmysite.com/styles.css"></link>
        <link href="/mystyles1.css"></link>
        <link href="./mystyles2.css"></link>
        <link href="../static/mystyles3.css"></link>
        <link href="http://mysite.com/static/mystyles4.css"></link>
        <link href="//mysite.com/static/mystyles5.css"></link>
        <link href="www.mysite.com/mystyles6.css"></link>
        <link href="www.notmysite.com/mystyles.css"></link>
        <link href="https://www.mysite.com/mystyles7.css"></link>
        <link href="mystyles8.css"></link>
      </head>
    </html>


    val accumulateOptionFn = crawler.accumulatePartial(base, "http://www.mysite.com/something") _
    val pageSummary = crawler.processHtmlElements(page, "http://www.mysite.com/something", accumulateOptionFn)

    it ("detects the assets that live on the same domain, and normalizes them correctly") {
      assert(pageSummary.links.length === 8)

      pageSummary.links.contains("https://www.mysite.com/mystyles7.css")
      pageSummary.links.contains("http://www.mysite.com/mystyles1.css")
      pageSummary.links.contains("http://www.mysite.com/something/mystyles2.css")
      pageSummary.links.contains("http://www.mysite.com/static/mystyles3.css")
      pageSummary.links.contains("http://www.mysite.com/static/mystyles4.css")
      pageSummary.links.contains("http://www.mysite.com/static/mystyles5.css")
      pageSummary.links.contains( "http://www.mysite.com/mystyles6.css")
      pageSummary.links.contains( "http://www.mysite.com/mystyles8.css")
    }
  }

  describe("only unique items (to the page) are reported back - no duplicates") {

    val page =
      <html>
        <head>
          <script src="http://mysite.com/something/jquery.js"></script>
          <script src="//mysite.com/jquery.js"></script>
          <script src="./jquery.js"></script>
          <script src="http://mysite.com/jquery.js"></script>
        </head>
      </html>


    val accumulateOptionFn = crawler.accumulatePartial(base, "http://www.mysite.com/something") _
    val pageSummary = crawler.processHtmlElements(page, "http://www.mysite.com/something", accumulateOptionFn)

    it ("flattens duplicates (if any)") {
      assert(pageSummary.scripts.length === 3)
      assert(pageSummary.scripts.contains("http://mysite.com/something/jquery.js"))
      //imho www should be considered a different resource. shouldn't be that hard to switch certain criteria off and on
      assert(pageSummary.scripts.contains("http://www.mysite.com/something/jquery.js"))
      assert(pageSummary.scripts.contains("http://mysite.com/jquery.js"))
    }
  }


}

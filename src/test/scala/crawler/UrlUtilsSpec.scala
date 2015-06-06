package crawler

import org.scalatest.{FunSpec}

//Tests the UrlUtils companion object
class UrlUtilsSpec extends FunSpec {

  describe ("Generic path resolver utils") {

    describe("can resolve paths optimistically") {

      it ("can optimistically resolve if given a .html extension") {

        val product = UrlUtils.resolveRelativeOptimistic("http://something.com/index.html", "./myDirectory")

        assert(product === "http://something.com/myDirectory")

      }

      it ("resolves for current directory") {

        val product = UrlUtils.resolveRelativeOptimistic("http://something.com", "./myDirectory")

        assert(product === "http://something.com/myDirectory")

      }

      it ("resolves for parent paths") {

        val product = UrlUtils.resolveRelativeOptimistic("http://something.com/a/b/c", "../../d/e/f")

        assert(product === "http://something.com/a/d/e/f")

      }

      it ("resolves complex paths") {

        val product = UrlUtils.resolveRelativeOptimistic("http://something.com/a/b/c", "../../d/e/f/../g")

        assert(product === "http://something.com/a/d/e/g")

      }

      it ("resolves for https") {
        val product = UrlUtils.resolveRelativeOptimistic("https://something.com/a/b/c", "../../d/e/f/../g")

        assert(product === "https://something.com/a/d/e/g")
      }

      it ("throws an exception if not provided an http or https url") {
        intercept[MatchError]{
          //well yeah, some browser will open connections to ftp... but that's not really considered the same domain
          UrlUtils.resolveRelativeOptimistic("ftp://mysecretftpserver.com:21", "../../ss")
        }
      }
    }


    describe("can resolve paths") { //same as above. just different function

      it("resolves for current directory") {

        val product = UrlUtils.resolveRelative("http://something.com", "./myDirectory")

        assert(product === "http://something.com/myDirectory")

      }

      it("resolves for parent paths") {

        val product = UrlUtils.resolveRelative("http://something.com/a/b/c", "../../d/e/f")

        assert(product === "http://something.com/a/d/e/f")

      }

      it("resolves complex paths") {

        val product = UrlUtils.resolveRelative("http://something.com/a/b/c", "../../d/e/f/../g")

        assert(product === "http://something.com/a/d/e/g")

      }

      it("resolves for https") {
        val product = UrlUtils.resolveRelative("https://something.com/a/b/c", "../../d/e/f/../g")

        assert(product === "https://something.com/a/d/e/g")
      }

      it("throws an exception if not provided an http or https url") {
        intercept[MatchError] {
          //well yeah, some browser will open connections to ftp... but that's not really considered the same domain
          UrlUtils.resolveRelative("ftp://mysecretftpserver.com:21", "../../ss")
        }
      }
    }

    describe ("can normalize paths") {
      it ("normalizes a path") {
        val p = UrlUtils.normalize("http://something.com/./hello")

        assert(p === "http://something.com/hello")
      }

      it ("normalizes for https") {
        val p = UrlUtils.normalize("https://something.com/./hello")

        assert(p === "https://something.com/hello")
      }
    }

    describe ("normalization strategies") {
      it ("can use a singleton strategy") {

        val myNormalizationStrategy = Seq(
          UrlUtils.useLowerCase _
        )

        //the following two styles should be equivalent
        assert(UrlUtils.toNormalizationStrategy("HTTP://WHATEVER.COM", myNormalizationStrategy)
            === "http://whatever.com")

        assert(UrlUtils.toNormalizationStrategy("HTTP://WHATEVER.COM", UrlUtils.useLowerCase _)
          === "http://whatever.com")

      }

      it ("can stack normalization strategies when passed a sequence of functions") {

        val myNormalizationStrategy = Seq(
          UrlUtils.useLowerCase _,
          UrlUtils.addTrailingBackslash _
        )

        val normalized = UrlUtils.toNormalizationStrategy("HTtP://WHATeVER.COM/a", myNormalizationStrategy)

        assert(normalized === "http://whatever.com/a/")
      }

      it ("stacks in order" ) {
        val myNormalizationStrategy = Seq (
          (str: String) => (str + "/a"),
          (str: String) => (str + "/b"),
          (str: String) => (str + "/c")
        )

        assert(UrlUtils.toNormalizationStrategy("http://whatever.com", myNormalizationStrategy)
          === "http://whatever.com/a/b/c")
      }

    }

    describe("getNormalizedCandidates") {
      val expected = List("http://test.com", "http://test.com/", "http://www.test.com", "http://www.test.com/")

      it ("given a normalized path, gives candidates that might be equivalent") {
        val cand = UrlUtils.getNormalizedCandidates("http://test.com")
        assert(cand.length === 4)
        assert(expected.forall(cand.contains(_)))
      }
      it ("called on, all variants of a normalized candidate yields the same result") {
        val type1 = UrlUtils.getNormalizedCandidates("http://test.com")
        assert(type1.length === 4)
        assert(expected.forall(type1.contains(_)))

        val type2 = UrlUtils.getNormalizedCandidates("http://www.test.com")
        assert(type2.length === 4)
        assert(expected.forall(type2.contains(_)))

        assert(type1.equals(type2))
      }
      it ("trailing backslashes will not be removed if they are provided") {
        val expected2 = List("http://test.com/", "http://www.test.com/")
        val type3 = UrlUtils.getNormalizedCandidates("http://test.com/")
        assert(expected2.forall(type3.contains(_)))

        val type4 = UrlUtils.getNormalizedCandidates("http://www.test.com/")
        assert(expected2.forall(type4.contains(_)))

        assert(type3.equals(type4))
      }

      it ("possibleFiles will still provide candidates, but will not add backslashes") {
        val type5 = UrlUtils.getNormalizedCandidates("http://test.com/index.html")
        assert(type5.contains("http://test.com/index.html"))
        assert(type5.contains("http://www.test.com/index.html"))
      }
    }

    describe("normalization convenience functions") {
      it ("ensuresProtocol") {
        //adds them if they do not exist
        assert(UrlUtils.ensureProtocol("http://")("www.test.com") === "http://www.test.com")
        assert(UrlUtils.ensureProtocol("https://")("www.test.com") === "https://www.test.com")

        //uses the baseProtocol if none present
        assert(UrlUtils.ensureProtocol("http://")("http://test.com") === "http://test.com")
        assert(UrlUtils.ensureProtocol("https://")("https://test.com") === "https://test.com")
      }
    }

    it ("identifies possible files") {
      assert(UrlUtils.possiblyAFile("http://test.com/index.html") == true )
      assert(UrlUtils.possiblyAFile("http://test.com/") == false )
    }

    it ("addWww adds www to http prefixed paths without www") {
      assert(UrlUtils.addWww("http://mysite.com") === "http://www.mysite.com")
    }

    it ("addWww leaves http://www prefixed paths alone") {
      assert(UrlUtils.addWww("http://www.mysite.com") === "http://www.mysite.com")
    }

    it ("removeWwwFromPathWithProtocol removes www from path with protocol prefix") {
      assert(UrlUtils.removeWwwFromPathWithProtocol("http://www.mysite.com") === "http://mysite.com")
    }

    it ("removeWwwFromPathWithProtocol leaves path without www alone") {
      assert(UrlUtils.removeWwwFromPathWithProtocol("http://mysite.com") === "http://mysite.com")
    }
  }
}

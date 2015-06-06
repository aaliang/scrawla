package crawler

import org.scalatest.{FunSpec}

//Tests the PathUtils companion object
class PathUtilsSpec extends FunSpec {

  describe ("Generic path resolver utils") {

    describe("can resolve paths optimistically") {

      it ("can optimistically resolve if given a .html extension") {

        val product = UrlPathUtils.resolveRelativeOptimistic("http://something.com/index.html", "./myDirectory")

        assert(product === "http://something.com/myDirectory")

      }

      it ("resolves for current directory") {

        val product = UrlPathUtils.resolveRelativeOptimistic("http://something.com", "./myDirectory")

        assert(product === "http://something.com/myDirectory")

      }

      it ("resolves for parent paths") {

        val product = UrlPathUtils.resolveRelativeOptimistic("http://something.com/a/b/c", "../../d/e/f")

        assert(product === "http://something.com/a/d/e/f")

      }

      it ("resolves complex paths") {

        val product = UrlPathUtils.resolveRelativeOptimistic("http://something.com/a/b/c", "../../d/e/f/../g")

        assert(product === "http://something.com/a/d/e/g")

      }

      it ("resolves for https") {
        val product = UrlPathUtils.resolveRelativeOptimistic("https://something.com/a/b/c", "../../d/e/f/../g")

        assert(product === "https://something.com/a/d/e/g")
      }

      it ("throws an exception if not provided an http or https url") {
        intercept[MatchError]{
          //well yeah, some browser will open connections to ftp... but that's not really considered the same domain
          UrlPathUtils.resolveRelativeOptimistic("ftp://mysecretftpserver.com:21", "../../ss")
        }
      }
    }


    describe("can resolve paths") { //same as above. just different function

      it("resolves for current directory") {

        val product = UrlPathUtils.resolveRelative("http://something.com", "./myDirectory")

        assert(product === "http://something.com/myDirectory")

      }

      it("resolves for parent paths") {

        val product = UrlPathUtils.resolveRelative("http://something.com/a/b/c", "../../d/e/f")

        assert(product === "http://something.com/a/d/e/f")

      }

      it("resolves complex paths") {

        val product = UrlPathUtils.resolveRelative("http://something.com/a/b/c", "../../d/e/f/../g")

        assert(product === "http://something.com/a/d/e/g")

      }

      it("resolves for https") {
        val product = UrlPathUtils.resolveRelative("https://something.com/a/b/c", "../../d/e/f/../g")

        assert(product === "https://something.com/a/d/e/g")
      }

      it("throws an exception if not provided an http or https url") {
        intercept[MatchError] {
          //well yeah, some browser will open connections to ftp... but that's not really considered the same domain
          UrlPathUtils.resolveRelative("ftp://mysecretftpserver.com:21", "../../ss")
        }
      }
    }

    describe ("can normalize paths") {
      it ("normalizes a path") {
        val p = UrlPathUtils.normalize("http://something.com/./hello")

        assert(p === "http://something.com/hello")
      }

      it ("normalizes for https") {
        val p = UrlPathUtils.normalize("https://something.com/./hello")

        assert(p === "https://something.com/hello")
      }
    }

    describe ("normalization strategies") {

      it ("can use a singleton strategy") {

        val myNormalizationStrategy = Seq(
          UrlPathUtils.useLowerCase _
        )

        //the following two styles should be equivalent
        assert(UrlPathUtils.toNormalizationStrategy("HTTP://WHATEVER.COM", myNormalizationStrategy)
            === "http://whatever.com")

        assert(UrlPathUtils.toNormalizationStrategy("HTTP://WHATEVER.COM", UrlPathUtils.useLowerCase _)
          === "http://whatever.com")

      }

      it ("can stack normalization strategies when passed a sequence of functions") {

        val myNormalizationStrategy = Seq(
          UrlPathUtils.useLowerCase _,
          UrlPathUtils.addTrailingBackslash _
        )

        val normalized = UrlPathUtils.toNormalizationStrategy("HTtP://WHATeVER.COM/a", myNormalizationStrategy)

        assert(normalized === "http://whatever.com/a/")
      }

      it ("stacks in order" ) {
        val myNormalizationStrategy = Seq (
          (str: String) => (str + "/a"),
          (str: String) => (str + "/b"),
          (str: String) => (str + "/c")
        )

        assert(UrlPathUtils.toNormalizationStrategy("http://whatever.com", myNormalizationStrategy)
          === "http://whatever.com/a/b/c")
      }

    }

    describe("normalization convenience functions") {
      it ("ensuresProtocol") {
        //adds them if they do not exist
        assert(UrlPathUtils.ensureProtocol("http://")("www.test.com") === "http://www.test.com")
        assert(UrlPathUtils.ensureProtocol("https://")("www.test.com") === "https://www.test.com")

        //uses the baseProtocol if none present
        assert(UrlPathUtils.ensureProtocol("http://")("http://test.com") === "http://test.com")
        assert(UrlPathUtils.ensureProtocol("https://")("https://test.com") === "https://test.com")
      }
    }
  }
}

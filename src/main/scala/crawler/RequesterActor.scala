package crawler
import java.net.ConnectException
import java.util.concurrent.{TimeoutException}

import akka.actor.Actor
import dispatch.StatusCode
import scala.concurrent.ExecutionContext.Implicits.global

case class HttpRequest(fallbacks: List[String])

case class BadResponse(url:String, fallbacks: List[String])


/**
 * Handles Http requests. Responds to the sender with results or errors (if any)
 */
class RequesterActor extends Actor {

  def receive = {

    /**
     * Only one type of message - a request to try
     */
    case httpRequest:HttpRequest =>

      val url = httpRequest.fallbacks.head
      val fut = HtmlLoader.get(url)
      val binded = sender

      fut.onSuccess {
        case validResult:OkResponse =>
          println("OK: " + url)
          binded ! validResult
        case InvalidResponse(badUrl) =>
          println("invalid: " + badUrl)
          binded ! BadResponse(badUrl, httpRequest.fallbacks)
        case MalformedUrl(malformed) =>
          println("malformed url: " + malformed)
          binded ! BadResponse(malformed, httpRequest.fallbacks)
          //don't try to retry (like above does)
        case RequestTimeout(timeoutUrl) =>
          println("timedout: " + timeoutUrl)
          binded ! BadResponse(timeoutUrl, httpRequest.fallbacks)
        case other =>
          println("other error: " + url)
          println(other)
          binded ! BadResponse(url, httpRequest.fallbacks)
      }

      //TODO: can collapse failures onto successes

      fut.onFailure {
        case e:TimeoutException =>
          println("timed out " + url)
          binded ! BadResponse(url, httpRequest.fallbacks)
        case exception:Throwable =>
          exception.getCause match {
            case e:TimeoutException =>
              println("timeout")
            case e:ConnectException =>
              println("connection exception")
            case e:StatusCode =>
              println(e.code + ": " + url)
            case null =>
              println("null: " + url)
            case e =>
              println("else [" + e + "]" + url)
          }
          binded ! BadResponse(url, httpRequest.fallbacks)
        case s =>
          println("other error")
          println(s)
          binded ! BadResponse(url, httpRequest.fallbacks)
      }


  }
}
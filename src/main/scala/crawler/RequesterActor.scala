package crawler

import akka.actor.Actor
import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.{Success, Failure}

case class HttpRequest(fallbacks: List[String])

case class BadResponse(url:String, fallbacks: List[String])

/**
 * Important note: this must be instantiated with a PinnedDispatcher. Otherwise the member variables may have
 * synchronization issues
 */
class RequesterActor extends Actor {

  val MAX_CONCURRENCY = 10
  var outstandingRequests = 0
  val parent = context.parent

  val queue = scala.collection.mutable.Queue[HttpRequest]()

  def shouldTryNext = {
    if (outstandingRequests < MAX_CONCURRENCY) {
      true
    }
    else {
      false
    }
  }

  def tryNextIfAble {
    outstandingRequests -= 1
    if (!queue.isEmpty) tryNext
  }

  def tryNext {
    val request = queue.dequeue

    val url = request.fallbacks.head

    outstandingRequests += 1

    println("trying: " + url)

    val fut = HtmlLoader.get(url)

    fut.onFailure {
      case s =>
        println("failed")
        tryNextIfAble
    }

    fut.onSuccess {
      case validResult:OkResponse =>
        println("visted: " + url)
        parent ! validResult
        tryNextIfAble
      case InvalidResponse(badUrl) =>
        println("invalid: " + badUrl)
        tryNextIfAble
        parent ! BadResponse(badUrl, request.fallbacks)
      case MalformedUrl(malformed) =>
        println("malformed url " + malformed)
        //don't try to retry (like above does)
        tryNextIfAble
      case s =>
        tryNextIfAble
    }
  }

  def receive = {

    case httpRequest:HttpRequest =>
      queue += httpRequest

      if (shouldTryNext) {
        tryNext
      }
  }
}
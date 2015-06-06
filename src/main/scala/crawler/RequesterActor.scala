package crawler

import akka.actor.Actor
import akka.actor.Status.{Success, Failure}
import scala.concurrent.ExecutionContext.Implicits.global

case class HttpRequest(requestUrl:String)


/**
 * Created by aliang on 6/5/15.
 */
class RequesterActor extends Actor {

  val MAX_CONCURRENCY = 6
  var outstandingRequests = 0

  val queue = scala.collection.mutable.Queue[String]()

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
    val url = queue.dequeue
    val parent = context.parent

    outstandingRequests += 1

//    println("trying " + url)

    HtmlLoader.get(url).map {
      case validResult:OkResponse =>
        println("visted: " + url)
        parent ! validResult
        tryNextIfAble
      case invalid:InvalidResponse =>
        println("invalid: " + url)
        parent ! invalid
        tryNextIfAble
      case _ =>
        println("other invalid")
        tryNextIfAble
    }
  }


  def receive = {

    case HttpRequest(url) =>
      queue += url

      if (shouldTryNext) {
        tryNext
      }
  }
}
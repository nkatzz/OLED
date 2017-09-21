package oled.non_blocking

import akka.actor.Actor

/**
  * Created by nkatz on 3/15/17.
  *
  * This is just a debugging tool.
  * It pings a set of actors to check their state
  *
  */

class PingActor(learningActorsNames: List[String]) extends Actor {

  var startTime = System.currentTimeMillis()
  val actorRefs = Utils.getOtherActors(context, learningActorsNames)

  def receive = {
    case "go" =>
      while(true) {
        val now  = System.currentTimeMillis()
        if (now - startTime > 10000) {
          actorRefs.foreach(_ ! "ping")
          startTime = System.currentTimeMillis()
        }
      }
  }
}

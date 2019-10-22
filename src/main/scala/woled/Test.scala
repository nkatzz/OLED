package woled

import akka.actor.{Actor, ActorSystem, Props}
import woled.Test.{Master, Reply, Work}

import scala.concurrent.Await
import scala.concurrent.duration._
import akka.pattern._
import akka.util.Timeout

/**
 * Created by nkatz at 17/10/19
 */

object Test {

  class Work
  class Reply(val reply: String)

  class Master extends Actor {

    def receive = {
      case _: Work =>
        println(s"Starting work at ${System.currentTimeMillis()}")
        Thread.sleep(10000)
        sender ! new Reply(s"Finished work at ${System.currentTimeMillis()}")
    }

  }

  /*class Worker extends Actor {
    def receive = {
      case "" => ???
    }
  }*/

}

object Run extends App {

  println(s"Starting execution at ${System.currentTimeMillis()}")

  val system = ActorSystem("TestActorSystem")

  implicit val timeout: Timeout = Timeout(21474800 seconds)

  val master = system.actorOf( Props[Master], name = "test-actor" )

  val future = ask(master, new Work).mapTo[Reply]

  val result = Await.result(future, Duration.Inf)

  println(result.reply)

  system.terminate()

  println("Shut down and exit")





}

/*
 * Copyright (C) 2016  Nikos Katzouris
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

package woled

import akka.actor.{Actor, ActorSystem, Props}
import woled.Test.{Master, Reply, Work}

import scala.concurrent.Await
import scala.concurrent.duration._
import akka.pattern._
import akka.util.Timeout
import logic.Clause

import scala.io.Source

/**
  * Created by nkatz at 17/10/19
  */

object NewTest extends App {

  val inputPath = "/home/nkatz/dev/MarineTraffic-Infore/events_infore.csv"

  val events = Source.fromFile(inputPath).getLines().foldLeft(Set.empty[String]) { (x, y) =>
    val split = y.split(",")
    val event = split(1)
    if (!x.contains(event)) x + event else x
  }

  println(events)

}

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

  val master = system.actorOf(Props[Master], name = "test-actor")

  val future = ask(master, new Work).mapTo[Reply]

  val result = Await.result(future, Duration.Inf)

  println(result.reply)

  system.terminate()

  println("Shut down and exit")
}

object SubsumptionTest extends App {

  val c1 = Clause.parse("pilotOps(X2,X0,X1) :- lowSpeed(X0,X1),lowSpeed(X2,X1),withinArea(X0,nearPorts,X1),withinArea(X2,nearPorts,X1)")
  val c2 = Clause.parse("pilotOps(X0,X2,X1) :- lowSpeed(X0,X1),proximity(X0,X2,X1),proximity(X2,X0,X1),withinArea(X0,nearPorts,X1),withinArea(X2,nearPorts,X1)")

  println(c1.thetaSubsumes(c2))

}

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
      while (true) {
        val now = System.currentTimeMillis()
        if (now - startTime > 10000) {
          actorRefs.foreach(_ ! "ping")
          startTime = System.currentTimeMillis()
        }
      }
  }
}

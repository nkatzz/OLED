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

package mcts.parallel

import akka.actor.{Actor, PoisonPill}
import app.runutils.Globals
import app.runutils.IOHandling.InputSource
import com.typesafe.scalalogging.LazyLogging
import logic.Examples.Example
import logic.Theory


/**
  * Created by nkatz on 9/22/17.
  */

class ScorerWorker[T <: InputSource](globals: Globals, options: T,
                                     dataFunction: T => Iterator[Example]) extends Actor with LazyLogging {

  def receive = {
    case theory: Theory =>
      logger.info(s"Scoring\n${theory.tostring}\n")
      Eval.crossVal(theory, dataFunction(options), globals)
      sender ! theory
      //self ! PoisonPill
  }

}

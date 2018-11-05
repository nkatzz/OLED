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

package oled.weightlearn.parallel

import akka.actor.ActorRef
import app.runutils.RunningOptions
import logic.Examples.Example
import logic.{Clause, Literal}

object IO {

  class MLNClauseHandlingInput(val clauses: Vector[Clause], val clauseIds: Vector[Int], val example: Example,
                               val inps: RunningOptions, val targetClass: String)

  class MLNClauseHandlingOutput(val inferredTrue: Vector[Literal], val actuallyTrue: Vector[Literal],
                                val incorrectlyTerminated: Vector[Literal], val correctlyNotTerminated: Vector[Literal],
                                val clausesWithUpdatedWeights: Vector[Clause], val totalExampleCount: Int)

  /* The splitEvery parameter is used to split clauses into smaller batches.
   * Workers is a list of actors (as many as the available cores) that have already been started. */
  class MLNClauseHandingMasterInput(val clauses: Vector[Clause], val example: Example,
                                    val inps: RunningOptions, val targetClass: String,
                                    val splitEvery: Int, val workers: Vector[ActorRef])

  class NodeDoneMessage(val sender: String)

  class FinishedBatch

  class TheoryRequestMessage

}

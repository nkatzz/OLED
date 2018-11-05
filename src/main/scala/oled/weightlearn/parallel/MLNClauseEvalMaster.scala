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

import akka.actor.{Actor, ActorRef}
import app.runutils.RunningOptions
import logic.Examples.Example
import logic.{Clause, Literal}
import oled.weightlearn.parallel.IO.{MLNClauseHandingMasterInput, MLNClauseHandlingInput, MLNClauseHandlingOutput}
import org.slf4j.LoggerFactory



class MLNClauseEvalMaster(inps: RunningOptions, targetClass: String) extends Actor {

  //private val logger = LoggerFactory.getLogger(self.path.name)

  private var counter = 0
  private var resultsVector = Vector.empty[MLNClauseHandlingOutput]
  private var clausesZippedWithIndex: Iterator[(Vector[Clause], Vector[Int])] = Iterator[(Vector[Clause], Vector[Int])]()

  private var example = Example()
  private var workers = Vector.empty[ActorRef]

  def reset() = {
    this.counter = 0
    this.resultsVector = Vector.empty[MLNClauseHandlingOutput]
    this.clausesZippedWithIndex = Iterator[(Vector[Clause], Vector[Int])]()
    this.example = Example()
    this.workers = Vector.empty[ActorRef]
  }

  def receive = {
    case x: MLNClauseHandingMasterInput =>

      reset()

      example = x.example


      val clauseBatches = x.clauses.grouped(x.splitEvery).toVector
      val clauseIds = (1 to x.clauses.length).toVector.grouped(x.splitEvery).toVector
      val zipped = clauseBatches zip clauseIds

      /*
      if (workers.length > clauseBatches.length) {
        throw new RuntimeException("Workers > clause batches. I need to handle this dynamically...")
        System.exit(-1)
      }
      */

      workers = x.workers.take(clauseBatches.length)

      this.counter = zipped.length
      this.clausesZippedWithIndex = zipped.toIterator

      // Send the first batches to the worker actors
      workers foreach { worker =>
        val batch = clausesZippedWithIndex.next()
        val (clauses, ids) = (batch._1, batch._2)
        val workerInput = new MLNClauseHandlingInput(clauses, ids, example, inps, targetClass)
        worker ! workerInput
      }

      // send clause batches to workers in a round-robin manner
      /*
      var i = 0
      zipped foreach { pair =>
        val (clauseBatch, batchIds) = (pair._1, pair._2)
        if ( i == workers.length) i = 0
        val worker = workers(i)
        val input = new MLNClauseHandlingInput(clauseBatch, batchIds, x.example, x.inps, x.targetClass, x.jep)
        worker ! input
        i += 1
      }
      */

    case x: MLNClauseHandlingOutput =>

      counter -= 1
      resultsVector = resultsVector :+ x

      //logger.info(s"Remaining: $counter")

      if (counter == 0) {

        // check the total examples count, just to be on the safe side.
        // All counts returned by each worker should be equal.
        val (totalExmplCount, allCountsEqual) =
        resultsVector.foldLeft( resultsVector.head.totalExampleCount, true ) { (a, y) =>
          val (previousCount, areCountsEqualSoFar) = (a._1, a._2)
          val currentExmplCount = y.totalExampleCount
          (currentExmplCount, areCountsEqualSoFar && previousCount == currentExmplCount)
        }

        if (!allCountsEqual) {
          //val stop = "stop"
          throw new RuntimeException("Example counts returned from multiple workers are not equal.")
          System.exit(-1)
        }

        val (inferredTrue, actuallyTrue, incorrectlyTerminated, correctlyNotTerminated, clausesWithUpdatedWeights) =
          this.resultsVector.foldLeft(Vector.empty[Literal], Vector.empty[Literal], Vector.empty[Literal],
            Vector.empty[Literal], Vector.empty[Clause]) { (accum, y) =>

          val _inferredTrue = accum._1 ++ y.inferredTrue
          val _actuallyTrue = accum._2 ++ y.actuallyTrue
          val _incorrectlyTerminated = accum._3 ++ y.incorrectlyTerminated
          val _correctlyNotTerminated = accum._4 ++ y.correctlyNotTerminated
          val _clausesWithUpdatedWeights = accum._5 ++ y.clausesWithUpdatedWeights

          (_inferredTrue, _actuallyTrue, _incorrectlyTerminated, _correctlyNotTerminated, _clausesWithUpdatedWeights)
        }
        context.parent ! new MLNClauseHandlingOutput(inferredTrue,
          actuallyTrue, incorrectlyTerminated, correctlyNotTerminated,
          clausesWithUpdatedWeights, totalExmplCount) // the parent actor here should be a WeightedTheoryLearner

      } else {

        // send the next batch to the sender for processing

        if (clausesZippedWithIndex.nonEmpty) {
          val nextBatch = clausesZippedWithIndex.next()
          val (clauses, ids) = (nextBatch._1, nextBatch._2)
          val workerInput = new MLNClauseHandlingInput(clauses, ids, example, inps, targetClass)
          sender ! workerInput
        }


      }


  }



}

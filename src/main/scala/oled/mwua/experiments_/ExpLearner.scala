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

package oled.mwua.experiments_

import app.runutils.IOHandling.InputSource
import app.runutils.{Globals, RunningOptions}
import com.typesafe.scalalogging.LazyLogging
import logic.{Clause, Theory}
import logic.Examples.Example
import oled.mwua.{ExpertAdviceFunctions, StateHandler}
import org.slf4j.LoggerFactory

import scala.util.matching.Regex

/**
  * Created by nkatz at 31/3/2019
  */
class ExpLearner[T <: InputSource](
    val inps: RunningOptions,
    val trainingDataOptions: T,
    val testingDataOptions: T,
    val trainingDataFunction: T => Iterator[Example],
    val testingDataFunction: T => Iterator[Example],
    val writeExprmtResultsTo: String = "") extends LazyLogging {

  val learningRate: Double = Globals.sleepingExpertsLearningRate

  val receiveFeedbackBias: Double = Globals.sleepingExpertsFeedBackBias

  val withInertia: Boolean = Globals.hedgeInertia

  val epsilon = 0.9 // used in the randomized version

  val randomizedPrediction = false

  // If this is false, some non-determinism is introduced (number of mistakes may vary slightly from round to round)
  val specializeAllAwakeRulesOnFPMistake = false

  // This is either 'winnow' or 'hedge'
  val weightUpdateStrategy = "hedge" //"winnow"

  val conservativeRuleGeneration = false

  // A rule must make this much % of the total FPs before it is specialized
  val percentOfMistakesBeforeSpecialize = 0

  // have this set to "" for a regular run without an input theory
  //val inputTheoryFile = "/home/nkatz/Desktop/theory"
  val inputTheoryFile = ""

  val inputTheory: List[Clause] = {
      def matches(p: Regex, str: String) = p.pattern.matcher(str).matches
    if (inputTheoryFile == "") {
      Nil
    } else {
      val rules = scala.io.Source.fromFile(inputTheoryFile).getLines.toList.filter(line => !matches("""""".r, line) && !line.startsWith("%"))
      val rulesParsed = rules.map(r => Clause.parse(r))
      rulesParsed
    }
  }

  val stateHandler: StateHandler = {
    val stateHandler = new StateHandler
    if (inputTheory.isEmpty) {
      stateHandler
    } else {
      val (inputInitRules, inputTermRules) = inputTheory.foldLeft(List.empty[Clause], List.empty[Clause]){ (x, y) =>
        if (y.head.functor.contains("initiated")) (x._1 :+ y, x._2) else (x._1, x._2 :+ y)
      }
      stateHandler.ensemble.initiationRules = inputInitRules
      stateHandler.ensemble.terminationRules = inputTermRules
      stateHandler
    }
  }

  //private val logger = LoggerFactory.getLogger(self.path.name)

  private val withec = true

  // Control learning iterations over the data
  private var repeatFor = inps.repeatFor

  // Used to count examples for holdout evaluation
  private var exampleCounter = 0

  // Local data variable. Cleared at each iteration (in case repfor > 1).
  private var data = Iterator[Example]()

  // This is optional. A testing set (for holdout evaluation) may not be provided.
  private var testingData = Iterator[Example]()

  // Counts the number of precessed batches. Used to determine when to
  // perform holdout evaluation on the test set. Incremented whenever a
  // new batch is fetched (see the getNextBatch() method)
  private var batchCounter = 0

  private var startTime = System.nanoTime()

  private var endTime = System.nanoTime()

  // Get the training data from the current inout source
  private def getTrainData = trainingDataFunction(trainingDataOptions)

  private def getTestingData = testingDataFunction(testingDataOptions)

  private def getNextBatch(lleNoise: Boolean = false) = {
    this.batchCounter += 1
    if (data.isEmpty) {
      Example()
    } else {
      if (!lleNoise) {
        data.next()
      } else {
        val currentBatch = data.next()
        val noisyNarrative = {
          currentBatch.narrative map { x =>
            x.replaceAll("active", "active_1")
          }
        }
        Example(annot = currentBatch.annotation, nar = noisyNarrative, _time = currentBatch.time)
      }
    }
  }

  def run() = {
    data = getTrainData

    if (this.data.isEmpty) {
      logger.error(s"Input source ${inps.train} is empty.")
      System.exit(-1)
    }

    var done = false

    var out = (0, 0, 0, 0.0, Vector.empty[Double], Vector.empty[Double])

    while (!done) {

      val nextBatch = getNextBatch(lleNoise = false)
      logger.info(s"Processing batch $batchCounter")
      if (nextBatch.isEmpty) {
        logger.info(s"Finished the data.")
        endTime = System.nanoTime()
        logger.info("Done.")
        //workers foreach(w => w ! PoisonPill)
        out = wrapUp()
        done = true
      } else {
        val trueLabels = nextBatch.annotation.toSet
        if (inputTheory.isEmpty) {
          ExpertAdviceFunctions.process(nextBatch, nextBatch.annotation.toSet, inps,
                                        stateHandler, trueLabels, learningRate, epsilon, randomizedPrediction,
                                        batchCounter, percentOfMistakesBeforeSpecialize, specializeAllAwakeRulesOnFPMistake,
                                        receiveFeedbackBias, conservativeRuleGeneration, weightUpdateStrategy, withInertia)
        } else {
          ExpertAdviceFunctions.process(nextBatch, nextBatch.annotation.toSet, inps,
                                        stateHandler, trueLabels, learningRate, epsilon, randomizedPrediction,
                                        batchCounter, percentOfMistakesBeforeSpecialize, specializeAllAwakeRulesOnFPMistake,
                                        receiveFeedbackBias, conservativeRuleGeneration, weightUpdateStrategy, withInertia, inputTheory = Some(inputTheory))
        }
      }
    }
    out
  }

  def wrapUp() = {

    if (trainingDataOptions != testingDataOptions) {

      // show the info so far:
      wrapUp_NO_TEST()

      // and do the test
      logger.info("\n\nEvaluating on the test set\n\n")

      val _stateHandler = new StateHandler

      _stateHandler.ensemble = {
        val ensemble = stateHandler.ensemble
        val init = ensemble.initiationRules.filter(x => x.body.nonEmpty)
        val term = ensemble.terminationRules.filter(x => x.body.nonEmpty)
        ensemble.initiationRules = init
        ensemble.terminationRules = term
        ensemble
      }

      val testData = testingDataFunction(testingDataOptions)
      val _receiveFeedbackBias = 0.0 // Give no supervision for training, we're only testing
      testData foreach { batch =>
        val trueLabels = batch.annotation.toSet
        ExpertAdviceFunctions.process(batch, batch.annotation.toSet, inps,
                                      _stateHandler, trueLabels, learningRate, epsilon, randomizedPrediction,
                                      batchCounter, percentOfMistakesBeforeSpecialize, specializeAllAwakeRulesOnFPMistake, _receiveFeedbackBias,
                                      conservativeRuleGeneration, weightUpdateStrategy)
      }
      logger.info(s"Prequential error vector:\n${_stateHandler.perBatchError.mkString(",")}")
      logger.info(s"Prequential error vector (Accumulated Error):\n${_stateHandler.perBatchError.scanLeft(0.0)(_ + _).tail}")
      logger.info(s"Total TPs: ${_stateHandler.totalTPs}, Total FPs: ${_stateHandler.totalFPs}, Total FNs: ${_stateHandler.totalFNs}, Total TNs: ${_stateHandler.totalTNs}")
      if (_receiveFeedbackBias != 1.0) {
        logger.info(s"\nReceived feedback on ${_stateHandler.receivedFeedback} rounds")
      }

      val tps = _stateHandler.totalTPs
      val fps = _stateHandler.totalFPs
      val fns = _stateHandler.totalFNs
      (tps, fps, fns, 0.0, Vector.empty[Double], Vector.empty[Double])
    } else {
      wrapUp_NO_TEST()
    }

  }

  def wrapUp_NO_TEST() = {

      def show(in: List[Clause]) = {
        in.sortBy(x => -x.w_pos).
          map(x => x.showWithStats + "\n" + x.refinements.sortBy(x => -x.w_pos).map(x => x.showWithStats).mkString("\n      ")).mkString("\n")
      }

    //logger.info(show(stateHandler.ensemble.initiationRules))
    //logger.info(show(stateHandler.ensemble.terminationRules))

    logger.info(Theory(stateHandler.ensemble.initiationRules.sortBy(x => -x.w_pos)).showWithStats)
    logger.info(Theory(stateHandler.ensemble.terminationRules.sortBy(x => -x.w_pos)).showWithStats)

    logger.info(s"Prequential error vector:\n${stateHandler.perBatchError.mkString(",")}")

    val accumError = stateHandler.perBatchError.scanLeft(0.0)(_ + _).tail

    logger.info(s"Prequential error vector (Accumulated Error):\n${stateHandler.perBatchError.scanLeft(0.0)(_ + _).tail}")
    logger.info(s"Prequential F1-score:\n${stateHandler.runningF1Score}")
    logger.info(s"Average prequential F1-score: ${stateHandler.runningF1Score.sum / stateHandler.runningF1Score.length}")
    logger.info(s"Total TPs: ${stateHandler.totalTPs}, Total FPs: ${stateHandler.totalFPs}, Total FNs: ${stateHandler.totalFNs}, Total TNs: ${stateHandler.totalTNs}")
    logger.info(s"Total time: ${(endTime - startTime) / 1000000000.0}")

    if (randomizedPrediction) {
      logger.info(s"\nPredicted with initiation rules: ${stateHandler.predictedWithInitRule} times")
      logger.info(s"\nPredicted with terminated rules: ${stateHandler.predictedWithTermRule} times")
      logger.info(s"\nPredicted with inertia: ${stateHandler.predictedWithInertia} times")
    }

    //logger.info(s"Predictions vector:\n${stateHandler.predictionsVector}")

    logger.info(s"Total number of rounds: ${stateHandler.totalNumberOfRounds}")

    ///*
    val tps = stateHandler.totalTPs
    val fps = stateHandler.totalFPs
    val fns = stateHandler.totalFNs

    val microPrecision = tps.toDouble / (tps.toDouble + fps.toDouble)
    val microRecall = tps.toDouble / (tps.toDouble + fns.toDouble)
    val microFscore = (2 * microPrecision * microRecall) / (microPrecision + microRecall)
    //println(s"Micro F1-score: $microFscore")
    //*/

    if (receiveFeedbackBias != 1.0) {
      logger.info(s"\nReceived feedback on ${stateHandler.receivedFeedback} rounds")
    }
    (tps, fps, fns, microFscore, accumError, stateHandler.runningF1Score)
  }

}

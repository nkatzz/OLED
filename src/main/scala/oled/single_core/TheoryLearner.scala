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

package oled.single_core

import akka.actor.Actor
import app.runutils.IOHandling.InputSource
import app.runutils.{Globals, RunningOptions}
import logic.Examples.Example
import logic.Theory
import org.slf4j.LoggerFactory
import utils.Utils
import oled.functions.SingleCoreOLEDFunctions._


/**
  * Created by nkatz on 27/2/2016.
  *
  */


/*
*
* This is the old version of the core OLED learner, where (in case we're
* learning with the Event Calculus) initiation and termination rules are
* learnt separately and in parallel.
*
* */


class TheoryLearner[T <: InputSource](val inps: RunningOptions,
                                      val trainingDataOptions: T,
                                      val testingDataOptions: T,
                                      val trainingDataFunction: T => Iterator[Example],
                                      val testingDataFunction: T => Iterator[Example],
                                      val targetClass: String) extends Actor {

  private val logger = LoggerFactory.getLogger(self.path.name)
  private var totalBatchProcessingTime = 0.0
  private var totalRuleScoringTime = 0.0
  private var totalNewRuleTestTime = 0.0
  private var totalCompressRulesTime = 0.0
  private var totalExpandRulesTime = 0.0
  private var totalNewRuleGenerationTime = 0.0


  def receive = {
    case "go" => sender ! run
  }

  val initorterm: String =
    if(targetClass=="initiated") "initiatedAt"
    else if (targetClass=="terminated") "terminatedAt"
    else inps.globals.MODEHS.head.varbed.tostring

  //private val withInertia = Globals.glvalues("with-inertia").toBoolean

  def run: (Theory,Double) = {

    def runOnce(inTheory: Theory): Theory = {
      val trainingData = trainingDataFunction(trainingDataOptions)
      if (trainingData.isEmpty) {
        logger.error(s"DB ${inps.train} is empty.")
        System.exit(-1)
      }
      trainingData.foldLeft(inTheory){ (topTheory, newExample) =>

        if (inps.showStats) println(newExample.time)

        val res = Utils.time {
          val t =
            if (Globals.glvalues("with-ec").toBoolean) processExample(topTheory, newExample, targetClass, inps, logger)
            else processExampleNoEC(topTheory, newExample, inps, logger)

          val th = t._1

          totalRuleScoringTime += t._2
          totalNewRuleTestTime += t._3
          totalCompressRulesTime += t._4
          totalExpandRulesTime += t._5
          totalNewRuleGenerationTime += t._6


          // This is used only when learning with inertia. But I think
          // its ok to keep it so that I can print out stats for the current
          // joint theory (for debugging).
          //if (withInertia) updateGlobalTheoryStore(t, initorterm, inps.globals)
          if (Globals.glvalues("with-ec").toBoolean) updateGlobalTheoryStore(th, initorterm, inps.globals)

          th
        }
        if (inps.showStats) logger.info(s"Total batch process time: ${res._2}")
        this.totalBatchProcessingTime += res._2
        res._1
      }
    }

    logger.info(s"Starting learning for $targetClass")
    val _finalTheory = Utils.time{ (1 to inps.repeatFor).foldLeft(Theory())( (t,_) =>  runOnce(t)) }
    val (finalTheory,time) = (_finalTheory._1,_finalTheory._2)
    logger.info(s"\nTraining time for $targetClass: $time")
    val output = inps.withPostPruning match {
      case true =>
        val data = trainingDataFunction(trainingDataOptions)
        reScoreAndPrune(inps, data, finalTheory, initorterm, logger)
      case _ =>
        // no re-scoring
        val pruned = finalTheory.clauses.filter(x => x.score > inps.pruneThreshold && x.seenExmplsNum > inps.minEvalOn )
        logger.info(s"\nLearnt hypothesis (non-pruned):\n${finalTheory.showWithStats}")
        Theory(pruned)
    }
    logger.debug(s"\n$targetClass theory found:\n${output.tostring}")
    logger.info(s"Total batch processing time: $totalBatchProcessingTime")
    logger.info(s"Total rule scoring time: $totalRuleScoringTime")
    logger.info(s"Total rule expansion time: $totalExpandRulesTime")
    logger.info(s"Total rule compression time: $totalCompressRulesTime")
    logger.info(s"Total testing for new rule generation time: $totalNewRuleTestTime")
    logger.info(s"Total new rule generation  time: $totalNewRuleGenerationTime")
    (output,time)
  }



}

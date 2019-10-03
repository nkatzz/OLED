package woled

import akka.actor.{Actor, ActorRef, Props}
import app.runutils.IOHandling.Source
import app.runutils.RunningOptions
import logic.Examples.Example
import logic.{Clause, PosLiteral, Theory}
import oled.functions.SingleCoreOLEDFunctions.generateNewRules
import oled.weightlearn.parallel.IO.FinishedBatch
import oled.weightlearn.parallel.WeightedTheoryLearner

import scala.io.Source

class Learner[T <: app.runutils.IOHandling.Source](inps: RunningOptions, trainingDataOptions: T,
                           testingDataOptions: T, trainingDataFunction: T => Iterator[Example],
                           testingDataFunction: T => Iterator[Example],
                           targetClass: String) extends
  WeightedTheoryLearner(inps, trainingDataOptions, testingDataOptions, trainingDataFunction, testingDataFunction, targetClass) {

  import context.become

  var debug = 0

  /* Use a hand-crafted theory for debugging */
  /*val source = Source.fromFile("/home/nkatz/dev/BKExamples/BK-various-taks/WeightLearning/Caviar/fragment/meeting/ASP/asp-rules-test")
  val list = source.getLines
  val rulesList = list.map(x => Clause.parse(x)).toList
  source.close
  inps.globals.state.updateRules(rulesList)*/


  def inferenceState: Receive = {

    ???

  }

  override def processingState: Receive = {

    case e: Example =>

      if (inps.globals.state.getTheory().isEmpty) {
        if (e.annotation.nonEmpty) {

          // Try to generate a set of bottom rules that explain the data
          val newRules = WoledUtils.generateNewRules(Theory(), e, inps.globals)
          inps.globals.state.updateRules(newRules)
        }

        // continue to next batch
        become(normalState)
        self ! new FinishedBatch

      } else {

        // In parallel, get a MAP-inferred state and compute the rules' groundings for comparison.

        println(s"\n *** BATCH $debug *** ")

        if (debug == 25) {
          val stop = "stop"
        }

        // Infer (change for experts and OLED)
        val rules = inps.globals.state.getAllRules(inps.globals)
        val inferredState = WoledUtils.getInferredState(rules, e, "MAP", inps)

        // Get batch error (change for experts and OLED)
        val (predictionsPerRuleMap, ruleIdsMap) = WoledUtils.getTrueRulesGroundings(e, inps.globals.state.getAllRules(inps.globals).toVector, inps)

        val (batchTPs, batchFPs, batchFNs) = WoledUtils.getRulesMistakes(inferredState.keySet, predictionsPerRuleMap, ruleIdsMap, e, inps)



        debug += 1




        become(normalState)
        self ! new FinishedBatch
      }
  }



}

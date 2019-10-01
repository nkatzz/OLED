package woled

import akka.actor.{Actor, ActorRef, Props}
import app.runutils.IOHandling.Source
import app.runutils.RunningOptions
import logic.Examples.Example
import logic.{Clause, PosLiteral, Theory}
import oled.functions.SingleCoreOLEDFunctions.generateNewRules
import oled.weightlearn.parallel.IO.FinishedBatch
import oled.weightlearn.parallel.WeightedTheoryLearner

class Learner[T <: Source](inps: RunningOptions, trainingDataOptions: T,
                           testingDataOptions: T, trainingDataFunction: T => Iterator[Example],
                           testingDataFunction: T => Iterator[Example],
                           targetClass: String) extends
  WeightedTheoryLearner(inps, trainingDataOptions, testingDataOptions, trainingDataFunction, testingDataFunction, targetClass) {

  import context.become


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


        println("MAP INFERENCE")

        val inferredState = WoledUtils.getInferredState(inps.globals.state.getAllRules(inps.globals), e, "MAP", inps)

        println(inferredState)

        val (predictionsPerRuleMap, ruleIdsMap) = WoledUtils.generateSolveASPGroundingsMetaProgram(e, inps.globals.state.getAllRules(inps.globals).toVector, inps)

        val test = ""






        become(normalState)
        self ! new FinishedBatch
      }
  }



}

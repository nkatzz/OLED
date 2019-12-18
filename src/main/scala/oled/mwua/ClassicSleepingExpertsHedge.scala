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

package oled.mwua

import app.runutils.RunningOptions
import logic.{Clause, Theory}
import logic.Examples.Example
import oled.functions.SingleCoreOLEDFunctions
import oled.mwua.AuxFuncs.{increaseWeights, reduceWeights, updateRulesScore}
import oled.mwua.ExpertAdviceFunctions._
import oled.mwua.HelperClasses.AtomTobePredicted

/**
  * Created by nkatz at 20/3/2019
  */

/* This is the classical implementation of Hedge with sleeping experts from
 * "Using and Combining Predictors which Specialize". Predictions are made
 * either with weighted majority, as in "Context-Sensitive Learning Methods for Text Categorization"
 * or with randomization, although the latter does not make much sense in the classification context
 *
 * */

object ClassicSleepingExpertsHedge {

  def splitAwakeAsleep(rulesToSplit: List[Clause], awakeIds: Set[String]) = {
    val rulesToSplitIds = rulesToSplit.map(_##).toSet
    val (topLevelAwakeRules, topLevelAsleepRules) = rulesToSplit.foldLeft(Vector.empty[Clause], Vector.empty[Clause]) { (x, rule) =>
      val isAwake = awakeIds.contains(rule.##.toString)
      val isTopLevel = rulesToSplitIds.contains(rule.##)
      if (isAwake) if (isTopLevel) (x._1 :+ rule, x._2) else (x._1, x._2) // then it's a refinement rule
      else if (isTopLevel) (x._1, x._2 :+ rule) else (x._1, x._2) // then it's a refinement rule
    }
    (topLevelAwakeRules, topLevelAsleepRules)
  }

  def updateStructure_NEW_HEDGE(
      atom: AtomTobePredicted,
      previousTime: Int,
      markedMap: Map[String, Clause],
      predictedLabel: String,
      feedback: String,
      batch: Example,
      currentAtom: String,
      inps: RunningOptions,
      logger: org.slf4j.Logger,
      stateHandler: StateHandler,
      percentOfMistakesBeforeSpecialize: Int,
      randomizedPrediction: Boolean,
      selected: String,
      specializeAllAwakeOnMistake: Boolean,
      conservativeRuleGeneration: Boolean,
      generateNewRuleFlag: Boolean) = {

      def getAwakeBottomRules(what: String) = {
        if (what == "initiatedAt") atom.initiatedBy.filter(x => markedMap(x).isBottomRule)
        else atom.terminatedBy.filter(x => markedMap(x).isBottomRule)
      }

    var updatedStructure = false

    if (is_FP_mistake(predictedLabel, feedback)) {
      val awakeBottomRules = getAwakeBottomRules("terminatedAt")
      if (generateNewRuleFlag) { //&& awakeBottomRules.isEmpty //atom.terminatedBy.isEmpty
        // If we leave the if (stateHandler.inertiaExpert.knowsAbout(atom.fluent)) clause here
        // we get many more mistakes. On the other hand, it seems more reasonable to generate
        // termination rules only when the fluent holds by inertia... (don't know what to do)
        //if (stateHandler.inertiaExpert.knowsAbout(atom.fluent)) {

        // Use the rest of the awake termination rules to compare the new
        // ones to and make sure that no redundant rules are generated:
        val awakeTerminationRules = atom.terminatedBy.map(x => markedMap(x))

        // This generates bottom clause heads with adbuction
        /*
          updatedStructure =
            generateNewRule(batch, currentAtom, inps, "FP", logger,
              stateHandler, "terminatedAt", 1.0, otherAwakeExperts = awakeTerminationRules)
           */

        // This generates a bottom rule head by simply generating an initiation/termination atom
        // for a fluent from the previous time point from which a mistake is made (no abduction).
        // TO USE THIS THE streaming VARIABLE AT THE BEGINNING OF THE PROCESS METHOD NEEDS TO BE SET TO true
        ///*
        updatedStructure =
          generateNewExpert_NEW(batch, atom, previousTime, inps, "FP", logger,
                                stateHandler, "terminatedAt", 1.0, otherAwakeExperts = awakeTerminationRules)
        //*/
        //}
      }
      // Also, in the case of an FP mistake we try to specialize awake initiation rules.
      if (atom.initiatedBy.nonEmpty) {
        // We are doing this after each batch
        ///*
        val (topLevelAwakeRules, topLevelAsleepRules) = splitAwakeAsleep(stateHandler.ensemble.initiationRules, atom.initiatedBy.toSet)
        val expandedInit = SingleCoreOLEDFunctions.
          expandRules(Theory(topLevelAwakeRules.toList.filter(x => x.refinements.nonEmpty)), inps, logger)
        if (expandedInit._2) {
          stateHandler.ensemble.initiationRules = expandedInit._1.clauses ++ topLevelAsleepRules
          updatedStructure = true
        }
        //*/
      }
    }

    if (is_FN_mistake(predictedLabel, feedback)) {
      val awakeBottomRules = getAwakeBottomRules("initiatedAt")
      if (generateNewRuleFlag) { // atom.initiatedBy.isEmpty
        // We don't have firing initiation rules. Generate one.
        if (awakeBottomRules.isEmpty) {

          // Use the rest of the awake termination rules to compare the new
          // ones to and make sure that no redundant rules are generated:
          val awakeInitiationRules = atom.initiatedBy.map(x => markedMap(x))

          // This generates bottom clause heads with adbuction
          /*
          updatedStructure =
            generateNewRule(batch, currentAtom, inps, "FN", logger,
              stateHandler, "initiatedAt", 1.0, otherAwakeExperts = awakeInitiationRules)
          */

          // This generates a bottom rule head by simply generating an initiation/termination atom
          // for a fluent from the previous time point from which a mistake is made (no abduction).
          // TO USE THIS THE streaming VARIABLE AT THE BEGINNING OF THE PROCESS METHOD NEEDS TO BE SET TO true
          ///*
          updatedStructure =
            generateNewExpert_NEW(batch, atom, previousTime, inps, "FN", logger,
                                  stateHandler, "initiatedAt", 1.0, otherAwakeExperts = awakeInitiationRules)
          //*/
        }
      } else {
        if (!conservativeRuleGeneration) {
          // If we are not in conservative mode we try to generate new initiation rules even if awake initiation
          // rules already exist. We only do so if the current example has not already been compressed into an existing
          // bottom rule.
          if (awakeBottomRules.isEmpty) {
            updatedStructure = generateNewRule_1(batch, currentAtom, inps, logger, stateHandler, "initiatedAt", 1.0)
          }
        }
      }
      // Also, in the case of an FP mistake we try to specialize awake termination rules.
      if (atom.terminatedBy.nonEmpty) {
        // We are doing this after each batch
        ///*
        val (topLevelAwakeRules, topLevelAsleepRules) = splitAwakeAsleep(stateHandler.ensemble.terminationRules, atom.terminatedBy.toSet)
        val expandedInit = SingleCoreOLEDFunctions.
          expandRules(Theory(topLevelAwakeRules.toList.filter(x => x.refinements.nonEmpty)), inps, logger)
        if (expandedInit._2) {
          stateHandler.ensemble.terminationRules = expandedInit._1.clauses ++ topLevelAsleepRules
          updatedStructure = true
        }
        //*/
      }
    }
    updatedStructure
  }

  def predictHedge_NO_INERTIA(a: AtomTobePredicted, stateHanlder: StateHandler, markedMap: Map[String, Clause]) = {

    // Here we assume that initiation rules predict '1' and termination rules predict '0'.
    // The prediction is a number in [0,1] resulting from the weighted avegage of the experts predictions:
    // prediction = (Sum_{weight of awake init rules} + inertia_weight) / (Sum_{weight of awake term rules} + Sum_{weight of awake init rules} + inertia_weight)
    // if prediction > threshold (e.g. 0.5) then we predict holds, else false

    val (_, _, awakeInit, awakeTerm, currentFluent) = (a.atom, a.time, a.initiatedBy, a.terminatedBy, a.fluent)

    val initWeightSum = if (awakeInit.nonEmpty) awakeInit.map(x => markedMap(x).w_pos).sum else 0.0
    val termWeightSum = if (awakeTerm.nonEmpty) awakeTerm.map(x => markedMap(x).w_pos).sum else 0.0

    val _prediction = initWeightSum / (initWeightSum + termWeightSum)

    val prediction = if (_prediction.isNaN) 0.0 else _prediction

    (prediction, 0.0, initWeightSum, termWeightSum)

  }

  /*
  def updateWeights_NO_INERTIA(atom: AtomTobePredicted, prediction: Double, inertiaExpertPrediction: Double,
                    initWeightSum: Double, termWeightSum: Double, predictedLabel: String,
                    markedMap: Map[String, Clause], feedback: String, stateHandler: StateHandler,
                    learningRate: Double, weightUpdateStrategy: String) = {

    val currentFluent = atom.fluent

    val hedge = weightUpdateStrategy == "hedge"

    val totalWeightBeforeUpdate = initWeightSum + termWeightSum

    def getSleeping(what: String) = {
      val awake = if (what == "initiated") atom.initiatedBy.toSet else atom.terminatedBy.toSet
      markedMap.filter(x => x._2.head.functor.contains(what) && !awake.contains(x._1))
    }

    val nonFiringInitRules = getSleeping("initiated")
    val nonFiringTermRules = getSleeping("terminated")

    def updateScore(what: String) = {
      updateRulesScore(what, atom.initiatedBy.map(x => markedMap(x)), nonFiringInitRules.values.toVector,
        atom.terminatedBy.map(x => markedMap(x)), nonFiringTermRules.values.toVector)
    }

    if (is_TP(predictedLabel, feedback)) {
      updateScore("TP")
      if (hedge) reduceWeights(atom.terminatedBy, markedMap, learningRate, "hedge")
    }

    if (is_FP_mistake(predictedLabel, feedback)) {
      if (!hedge) {
        reduceWeights(atom.initiatedBy, markedMap, learningRate)
        increaseWeights(atom.terminatedBy, markedMap, learningRate)
      } else {
        reduceWeights(atom.initiatedBy, markedMap, learningRate, "hedge")
      }
      updateScore("FP")
    }

    if (is_FN_mistake(predictedLabel, feedback)) {
      if (!hedge) {
        increaseWeights(atom.initiatedBy, markedMap, learningRate)
        reduceWeights(atom.terminatedBy, markedMap, learningRate)
      } else {
        reduceWeights(atom.terminatedBy, markedMap, learningRate, "hedge")
      }
      updateScore("FN")
    }

    if (is_TN(predictedLabel, feedback)) {
      updateScore("TN")
      if (hedge) {
        reduceWeights(atom.initiatedBy, markedMap, learningRate, "hedge")
      }
    }

    if (hedge) {

      /*
       println(s"Awake init: ${awakeInitRules.size}, Awake term: ${awakeTermRules.size}, " +
          s"total awake: ${awakeInitRules.size+awakeTermRules.size} total rules: ${markedMap.size}")
      */

      // Re-normalize weights of awake experts
      //val inertCurrentWeight = stateHandler.inertiaExpert.getWeight(currentFluent)
      val getTotalWeight = (x: Vector[Clause]) => x.map(x => x.w_pos).sum
      val updateWeight = (x: Clause, y: Double) => {

        if (x.tostring.replaceAll("\\s", "") == "initiatedAt(meeting(X0,X1),X2):-happensAt(active(X0),X2),happensAt(active(X1),X2).") {
          println(s"${x.tostring}\nw: ${x.w_pos} w_new: ${y} predicted: $predictedLabel actual: $feedback")
        }

        if (y == 0.0) {
          val stop = "stop"
        }

        x.w_pos = y
      }
      val awakeInitRules = atom.initiatedBy.map(x => markedMap(x))
      val awakeTermRules = atom.terminatedBy.map(x => markedMap(x))
      val totalInitWeightAfter = getTotalWeight(awakeInitRules)
      val totalTermWeightAfter = getTotalWeight(awakeTermRules)
      val totalWeightAfter = totalInitWeightAfter + totalTermWeightAfter

      val mult = totalWeightBeforeUpdate/totalWeightAfter

      if (!mult.isNaN) {
        awakeInitRules.foreach(x =>  updateWeight(x, mult * x.w_pos ) )
        awakeTermRules.foreach(x =>  updateWeight(x, mult * x.w_pos ) )
      }
    }
  }

   */

}

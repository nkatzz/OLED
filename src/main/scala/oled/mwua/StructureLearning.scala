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
import logic.Examples.Example
import logic.{Clause, Theory}
import oled.functions.SingleCoreOLEDFunctions
import oled.mwua.ExpertAdviceFunctions.{generateNewRule, generateNewRule_1, is_FN_mistake, is_FP_mistake}
import oled.mwua.HelperClasses.AtomTobePredicted

object StructureLearning {

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

      if (generateNewRuleFlag) {

        // Recombining low-weight Awake rules:

        // New Rules:
        if (awakeBottomRules.isEmpty) {
          if (stateHandler.inertiaExpert.knowsAbout(atom.fluent)) {
            updatedStructure = generateNewRule(batch, currentAtom, inps, "FP", logger, stateHandler, "terminatedAt", 1.0)
          }
        }

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
          // Let's try this: Discard the entire initiated ensemble generated so far
          //if (atom.initiatedBy.nonEmpty) stateHandler.ensemble.initiationRules = Nil
          updatedStructure = generateNewRule(batch, currentAtom, inps, "FN", logger, stateHandler, "initiatedAt", 1.0)
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

}

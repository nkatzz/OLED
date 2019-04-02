package oled.mwua

import logic.Clause
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

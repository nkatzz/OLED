package oled.mwua

import logic.Clause
import oled.mwua.HelperClasses.AtomTobePredicted

/**
  * Created by nkatz at 10/2/2019
  */



//val delayedUpdate = new mwua.StateHandler.DelayedUpdate(atom, prediction, inertiaExpertPrediction,
//  predictedLabel, feedback, stateHandler, epsilon, markedMap, totalWeight)

class DelayedUpdate(val atom: AtomTobePredicted, val prediction: Double, val inertiaExpertPrediction: Double,
                    val initWeightSum: Double, val termWeightSum: Double, val predictedLabel: String,
                    val markedMap: Map[String, Clause], val feedback: String, val stateHandler: StateHandler,
                    val learningRate: Double, val weightUpdateStrategy: String, val withInertia: Boolean = true,
                    val orderedTimes: Vector[Int]) {

  var generateNewRuleFlag: Boolean = false

}


class StateHandler {

  /*===============================================================================================================*/
  /* This is all helper/test code for updating weights after mini-batch prediction from all mistakes cumulatively. */
  /* ============================================ Test-helper code start ==========================================*/


  var delayedUpdates = Vector.empty[DelayedUpdate]

  def clearDelayedUpdates = delayedUpdates = Vector.empty[DelayedUpdate]

  /* ============================================ Test-helper code end ============================================*/
  /*===============================================================================================================*/

  /*------------------------------------*/
  /* Stuff related to the rule ensemble */
  /*------------------------------------*/

  var ensemble = new RuleEnsemble

  val inertiaExpert = new InertiaExpert

  def normalizeWeights(awakeExperts: Vector[Clause], currentFluent: String) = {
    val totalAwakeRulesWeight = awakeExperts.map(x => x.w_pos).sum
    val inertiaWeight = inertiaExpert.getWeight(currentFluent)
    val totalWeight = totalAwakeRulesWeight + inertiaWeight
    awakeExperts.foreach(x => x.w_pos = x.w_pos/totalWeight.toDouble)
    if (inertiaWeight > 0) inertiaExpert.updateWeight(currentFluent, inertiaWeight/totalWeight)
  }

  def addRule(rule: Clause) = {
    if (rule.head.functor.contains("initiated")) {
      ensemble.initiationRules = ensemble.initiationRules :+ rule
    } else if (rule.head.functor.contains("terminated")) {
      ensemble.terminationRules = ensemble.terminationRules :+ rule
    } else {
      ensemble.rules = ensemble.rules :+ rule
    }
  }

  def removeRule(rule: Clause) = {
    def remove(clauses: List[Clause], r: Clause) = {
      clauses.filter(x => !x.equals(r))
    }
    if (rule.head.functor.contains("initiated")) {
      ensemble.initiationRules = remove(ensemble.initiationRules, rule)
    } else if (rule.head.functor.contains("terminated")) {
      ensemble.terminationRules = remove(ensemble.terminationRules, rule)
    } else {
      ensemble.rules = remove(ensemble.rules, rule)
    }
  }

  def pruneUnderPerformingRules(weightThreshold: Double) = {

    def pruneRefinements(topRule: Clause) = {
      val goodRefs = topRule.refinements.filter(x => x.w_pos >= weightThreshold)
      topRule.refinements = goodRefs
    }

    ensemble.initiationRules.foreach(x => pruneRefinements(x))
    ensemble.terminationRules.foreach(x => pruneRefinements(x))
    val goodInitRules = ensemble.initiationRules.filter(x => x.body.isEmpty || (x.body.nonEmpty && x.w_pos >= weightThreshold) )
    ensemble.initiationRules = goodInitRules
    val goodTermRules = ensemble.terminationRules.filter(x => x.body.isEmpty || (x.body.nonEmpty && x.w_pos >= weightThreshold) )
    ensemble.terminationRules = goodTermRules
  }



  // "what" here is either "weight" of "score". If what=weight then acceptableScore
  // should be a weight threshold, e.g. 0.005. what=score then acceptableScore is a
  // threshold on the rule's precision set via the --prune parameter. Pruning with score
  // does not work, a large number of redundant rules have very good score but very low coverage.
  def pruneRules(what: String, acceptableScore: Double, logger: org.slf4j.Logger) = {

    /* Remove rules by score */
    def removeBadRules(rules: List[Clause]) = {
      rules.foldLeft(List.empty[Clause]) { (accum, rule) =>
        if (what == "score") {
          if (rule.body.length >= 2 && rule.score <= 0.5) accum else accum :+ rule
        } else {
          if (rule.body.length >= 3 && rule.w_pos < acceptableScore) {
            logger.info(s"\nRemoved rule (weight threshold is $acceptableScore)\n${rule.showWithStats}")
            accum
          } else {
            accum :+ rule
          }
        }

      }
    }
    ensemble.initiationRules = removeBadRules(ensemble.initiationRules)
    ensemble.terminationRules = removeBadRules(ensemble.terminationRules)
  }



  /*-----------------------------*/
  /* Grounding-related variables */
  /*-----------------------------*/
  var groundingTimes: Vector[Double] = Vector[Double]()

  def updateGrndsTimes(t: Double) = { groundingTimes = groundingTimes :+ t }

  /*-----------------*/
  /* Stats variables */
  /*-----------------*/
  var totalTPs = 0
  var totalFPs = 0
  var totalFNs = 0
  var totalTNs = 0

  var batchCounter = 0

  var perBatchError: Vector[Int] = Vector.empty[Int]

  var runningF1Score: Vector[Double] = Vector.empty[Double]

  var runningRulesNumber: Vector[Int] = Vector.empty[Int]

  def updateRunningF1Score = {
    val currentPrecision = totalTPs.toDouble/(totalTPs+totalFPs)
    val currentRecall = totalTPs.toDouble/(totalTPs+totalFNs)
    val _currentF1Score = 2*currentPrecision*currentRecall/(currentPrecision+currentRecall)
    val currentF1Score = if (_currentF1Score.isNaN) 0.0 else _currentF1Score
    runningF1Score = runningF1Score :+ currentF1Score
  }

  var receivedFeedback = 0

  var totalNumberOfRounds = 0

  var totalAtoms = 0

  var predictedWithInitRule = 0
  var predictedWithTermRule = 0
  var predictedWithInertia = 0

}

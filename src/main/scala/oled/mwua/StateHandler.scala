package oled.mwua

import logic.Clause

/**
  * Created by nkatz at 10/2/2019
  */

class StateHandler {

  /*------------------------------------*/
  /* Stuff related to the rule ensemble */
  /*------------------------------------*/

  var ensemble = new RuleEnsemble

  val inertiaExpert = new InertiaExpert

  def normalizeWeights(awakeExperts: Vector[Clause], currentFluent: String) = {
    val totalAwakeRulesWeight = awakeExperts.map(x => x.w).sum
    val inertiaWeight = inertiaExpert.getWeight(currentFluent)
    val totalWeight = totalAwakeRulesWeight + inertiaWeight
    awakeExperts.foreach(x => x.w = x.w/totalWeight.toDouble)
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

  /*-----------------------------*/
  /* Grounding-related variables */
  /*-----------------------------*/
  var groundingTimes: Vector[Double] = Vector[Double]()

  def updateGrndsTimes(t: Double) = {
    groundingTimes = groundingTimes :+ t
  }

  /*-----------------*/
  /* Stats variables */
  /*-----------------*/
  var totalTPs = 0
  var totalFPs = 0
  var totalFNs = 0
  var totalTNs = 0

  var perBatchError: Vector[Int] = Vector[Int]()

}

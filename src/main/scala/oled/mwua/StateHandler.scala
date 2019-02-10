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

  def normalizeWeights = ensemble.normalizeWeights

  def updateEnsemble(rule: Clause) = {
    ???
  }

  def updateEnsemble(rules: List[Clause]) = {
    ???
  }

  /*----------------*/
  /* Inertia Expert */
  /*----------------*/
  var inertiaExpert = scala.collection.mutable.Map[String, Double]()

  def getInertiaExpertPrediction(fluent: String) = {
    if (inertiaExpert.keySet.contains(fluent)) inertiaExpert(fluent) else 0.0
  }

  /*-----------------------------*/
  /* Grounding-related variables */
  /*-----------------------------*/
  var groundingTimes = Vector[Double]()

  def updateGrndsTimes(t: Double) = {
    groundingTimes = groundingTimes :+ t
  }

  /*-----------------*/
  /* Stats variables */
  /*-----------------*/
  var totalTPs = Set[String]()
  var totalFPs = Set[String]()
  var totalFNs = Set[String]()
  var totalTNs = Set[String]()

}

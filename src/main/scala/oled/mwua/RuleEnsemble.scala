package oled.mwua

import app.runutils.RunningOptions
import logic.{Clause, Theory}

/**
  * Created by nkatz at 10/2/2019
  */

class RuleEnsemble {

  var initiationRules: List[Clause] = List[Clause]()
  var terminationRules: List[Clause] = List[Clause]()

  def removeZeroWeights = {
    // Maybe I should remove rules with zero weight only when they have at least one literal at their body...
    val noZeroInit = initiationRules.filter(x => x.w_pos > 0.0)
    val noZeroTerm = terminationRules.filter(x => x.w_pos > 0.0)
    initiationRules = noZeroInit
    terminationRules = noZeroTerm
  }

  /*
  // From each subsumption lattice return the rule with the highest weight. This is
  // used in a cross-validation setting
  def outputRules = {

    def findBest(c: Clause) = {
      val _bestRef = c.refinements.sortBy(x => -x.w_pos)
      if (_bestRef.nonEmpty) {
        val bestRef = _bestRef.head
        if (c.w_pos > bestRef.w_pos) c else bestRef
      } else {
        c
      }
    }

    val bestInit = initiationRules.map(x => findBest(x))
    val bestTerm = terminationRules.map(x => findBest(x))

    (bestInit, bestTerm)
  }
  */

  /*
   * "rules" is used in case we are not learning with the Event Calculus. In the opposite case this var it is empty.
   * */
  var rules: List[Clause] = List[Clause]()

  // if testHandCrafted is true we do not update weights or structure.
  def merged(inps: RunningOptions, testHandCrafted: Boolean = false) = {

    val _merged = if (rules.isEmpty) Theory(initiationRules ++ terminationRules) else Theory(rules)
    if (testHandCrafted) {
      _merged
    } else {
      _merged.clauses foreach (rule => if (rule.refinements.isEmpty) rule.generateCandidateRefs(inps.globals))
      // add the bottom rules here as well
      val mergedWithRefs = Theory( (_merged.clauses ++ _merged.clauses.flatMap(_.refinements)) ++ _merged.clauses.map(x => x.supportSet.clauses.head) )
      //val mergedWithRefs = Theory( _merged.clauses ++ _merged.clauses.flatMap(_.refinements) )
      mergedWithRefs
    }
  }

  def normalizeWeights = {
    val initWeightsSum = initiationRules.map(x => x.w_pos).sum
    val termWeightsSum = terminationRules.map(x => x.w_pos).sum
    val totalWeight = initWeightsSum + termWeightsSum
    initiationRules.foreach(x => x.w_pos = x.w_pos / totalWeight.toDouble)
    terminationRules.foreach(x => x.w_pos = x.w_pos / totalWeight.toDouble)
  }



}

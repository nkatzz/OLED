package oled.mwua

import app.runutils.RunningOptions
import logic.{Clause, Theory}

/**
  * Created by nkatz at 10/2/2019
  */

class RuleEnsemble {

  var initiationRules: List[Clause] = List[Clause]()
  var terminationRules: List[Clause] = List[Clause]()

  /*
   * "rules" is used in case we are not learning with the Event Calculus. In the opposite case this var it is empty.
   * */
  var rules: List[Clause] = List[Clause]()

  // if testHandCrafted is true we do not update weights or structure. It is
  def merged(inps: RunningOptions, testHandCrafted: Boolean = false) = {

    val _merged = if (rules.isEmpty) Theory(initiationRules ++ terminationRules) else Theory(rules)
    if (testHandCrafted) {
      _merged
    } else {
      _merged.clauses foreach (rule => if (rule.refinements.isEmpty) rule.generateCandidateRefs(inps.globals))
      val mergedWithRefs = Theory(_merged.clauses ++ _merged.clauses.flatMap(_.refinements))
      mergedWithRefs
    }
  }

  def normalizeWeights = {
    val initWeightsSum = initiationRules.map(x => x.w).sum
    val termWeightsSum = terminationRules.map(x => x.w).sum
    val totalWeight = initWeightsSum + termWeightsSum
    initiationRules.foreach(x => x.w = x.w / totalWeight)
    terminationRules.foreach(x => x.w = x.w / totalWeight)
  }



}

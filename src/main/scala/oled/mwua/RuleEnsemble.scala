package oled.mwua

import app.runutils.RunningOptions
import logic.{Clause, Theory}

/*
* Rules is used in case we are not learning with the Event Calculus,
  in the opposite case it is empty.
* */
class RuleEnsemble(val initiationRules: List[Clause],
                   val terminationRules: List[Clause], val rules: List[Clause]) {

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

}

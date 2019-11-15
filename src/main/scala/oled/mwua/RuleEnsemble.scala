package oled.mwua

import app.runutils.RunningOptions
import logic.{Clause, Theory}

/**
  * Created by nkatz at 10/2/2019
  */

class RuleEnsemble {

  var initiationRules: List[Clause] = List[Clause]()
  var terminationRules: List[Clause] = List[Clause]()

  /* The "action" variable here is either "add" or "replace" */
  def updateRules(newRules: List[Clause], action: String, inps: RunningOptions) = {
    newRules foreach { rule => if (rule.refinements.isEmpty) rule.generateCandidateRefs(inps.globals) }
    val (init, term) = newRules.partition(x => x.head.functor == "initiatedAt")
    action match {
      case "add" =>
        initiationRules = initiationRules ++ init
        terminationRules = terminationRules ++ term
      case "replace" =>
        initiationRules = init
        terminationRules = term
    }

  }

  /* Currently not used anywhere. */
  def removeZeroWeights = {
    // Maybe I should remove rules with zero weight only when they have at least one literal at their body...
    val noZeroInit = initiationRules.filter(x => x.w_pos > 0.0)
    val noZeroTerm = terminationRules.filter(x => x.w_pos > 0.0)
    initiationRules = noZeroInit
    terminationRules = noZeroTerm
  }

  val initiationRuleSets: scala.collection.mutable.Map[Int, Set[Int]] = scala.collection.mutable.Map[Int, Set[Int]]()
  val terminationRuleSets: scala.collection.mutable.Map[Int, Set[Int]] = scala.collection.mutable.Map[Int, Set[Int]]()

  def updateRuleSets(awakeFraction:String, ruleSet: scala.collection.mutable.Map[Int, Set[Int]], markedMap: Map[String, Clause]) = {

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

  /* Currently not used anywhere. */
  def normalizeWeights = {
    val initWeightsSum = initiationRules.map(x => x.w_pos).sum
    val termWeightsSum = terminationRules.map(x => x.w_pos).sum
    val totalWeight = initWeightsSum + termWeightsSum
    initiationRules.foreach(x => x.w_pos = x.w_pos / totalWeight.toDouble)
    terminationRules.foreach(x => x.w_pos = x.w_pos / totalWeight.toDouble)
  }



}

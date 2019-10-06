package woled

import app.runutils.Globals
import logic.Clause

class State {

  var initiationRules: List[Clause] = List[Clause]()
  var terminationRules: List[Clause] = List[Clause]()

  var perBatchError: Vector[Int] = Vector.empty[Int]

  var runningRulesNumber: Vector[Int] = Vector.empty[Int]

  var batchCounter = 0
  var totalTPs = 0
  var totalFPs = 0
  var totalFNs = 0
  var totalTNs = 0

  def getTopTheory() = initiationRules ++ terminationRules

  /* The "what" variable here is either "all" or "top".
  *  "all" returns all non-empty bodied rules along with their
  *  specializations, while "top" returns the top non-empty bodied rules.
  *  */
  def getAllRules(gl: Globals, what: String) = {
    val topRules  = getTopTheory()
    what match {
      case "all" =>
        topRules.flatMap { topRule =>
          if (topRule.refinements.isEmpty) topRule.generateCandidateRefs(gl)
          if (topRule.body.nonEmpty) List(topRule) ++ topRule.refinements else topRule.refinements
        }
      case "top" =>
        topRules.filter(x => x.body.nonEmpty)
    }

  }

  /* The "action" variable here is either "add" or "replace" */
  def updateRules(newRules: List[Clause], action: String) = {
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




}

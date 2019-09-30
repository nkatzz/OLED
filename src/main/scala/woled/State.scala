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

  def getTheory() = initiationRules ++ terminationRules

  // Returns all non empty-headed rules currently in the theory and their refinements
  def getAllRules(gl: Globals) = {
    val all  = getTheory()
    all.flatMap { topRule =>
      if (topRule.refinements.isEmpty) topRule.generateCandidateRefs(gl)
      if (topRule.body.nonEmpty) List(topRule) ++ topRule.refinements else topRule.refinements
    }
  }

  def updateRules(newRules: List[Clause]) = {
    val (init, term) = newRules.partition(x => x.head.functor == "initiatedAt")
    initiationRules = initiationRules ++ init
    terminationRules = terminationRules ++ term
  }



}

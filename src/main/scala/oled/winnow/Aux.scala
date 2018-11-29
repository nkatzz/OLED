package oled.winnow

import app.runutils.Globals
import logic.Examples.Example
import logic.{Clause, Literal, Theory}
import utils.ClauseImplicits._
import oled.distributed.Structures.ClauseStats

/**
  * Created by nkatz at 1/11/2018
  */

object MessageTypes {

  class ProcessBatchMsg(val theory: Theory, val batch: Example, val targetClass: String = "")

  class FinishedBatchMsg(val theory: Theory, val ruleScoringTime: Double, val newRuleTestTime: Double,
                         val compressRulesTime: Double, val expandRulesTime: Double,
                         val newRuleGenerationTime: Double, val BatchProcessingTime: Double, val targetClass: String)

}

object AuxFuncs {


  /* generates candidate refinements for the Hoeffding test.
   The otherNodesNames is used only in the distributed setting. */
  def generateCandidateRefs(c: Clause): Unit = {
    val specializationDepth = Globals.glvalues("specializationDepth").toInt
    val candidateList = c.supportSet.clauses.flatMap(_.body).distinct.filter(!c.body.contains(_))
    val refinementsSets =
      (for (x <- 1 to specializationDepth) yield x).foldLeft(List[List[Clause]]()) { (accum, depth) =>
        val z = for (lits <- candidateList.toSet.subsets(depth).toList) yield Clause(c.head, c.body ++ lits)
        val z_ = Theory.compressTheory(z)
        accum :+ z_
      }
    val flattend = refinementsSets.flatten
    flattend.foreach{ refinement =>
      refinement.parentClause = c
      //------------------------------------
      refinement.mlnWeight = c.mlnWeight
      //------------------------------------
      val newMap = scala.collection.mutable.Map[String, ClauseStats]()
      if (Globals.glvalues("distributed").toBoolean) {
        // Just to be on the safe side in the distributed case
        if (c.countsPerNode.isEmpty) throw new RuntimeException(s"The countsPerNode map of clause ${c.uuid} is empty," +
          s" when it should have been populated at clause generation")
        c.countsPerNode.foreach { entry => newMap(entry._1) = new ClauseStats(0, 0, 0, 0)}
        refinement.countsPerNode = newMap
      }
    }
    c.refinements = flattend
  }



  /**
    *
    * @return The marked rules and the marked rule preds (e.g. rule(234234)) as a single string ready for ASP use.
    *         Also a map of ruleId --> rule
    */
  def marked(clauses: Vector[Clause], globals: Globals): (String, Map[String, Clause]) = {
    val allRefinements = clauses flatMap(_.refinements)

    //val allRules = (clauses ++ allRefinements) //++ clauses.map(x => x.supportSet.clauses.head)//.filter(x => x.score >= 0.9)

    //val allRules = clauses ++ clauses.map(x => x.supportSet.clauses.head)
    val allRules = clauses
    /*
    val markedTheory = clauses map (x => marked(x, globals))
    val markedRefinements = allRefinements map (x => marked(x, globals))
    */

    //val markedTheory = clauses map (x => markedQuickAndDirty(x, globals))
    //val markedRefinements = allRefinements map (x => markedQuickAndDirty(x, globals))

    val allRulesMarked = allRules map (x => markedQuickAndDirty(x, globals))

    println(allRules.map(_.w))

    val hashCodesClausesMap = (allRules map (x => x.##.toString -> x)).toMap
    val rulePredicates = hashCodesClausesMap.keySet.map(x => s"rule($x). ").mkString("\n")
    (allRulesMarked.map(_.tostring).mkString("\n")+rulePredicates, hashCodesClausesMap)
  }

  // The head of a weighted rule is of the form: marked(ruleId, "weight", actualHead)
  def marked(c: Clause, globals: Globals) = {
    val cc = Clause(head=Literal(functor = "marked", terms=List(c.##.toString, s""""${c.w}"""", c.head)), body=c.withTypePreds(globals).body)
    cc.w = c.w
    cc
  }

  // The head of a weighted rule is of the form: marked(ruleId, actualHead)
  def markedQuickAndDirty(c: Clause, globals: Globals) = {
    val cc = Clause(head=Literal(functor = "marked", terms=List(c.##.toString, c.head)), body=c.withTypePreds(globals).body)
    cc.w = c.w
    cc
  }

}



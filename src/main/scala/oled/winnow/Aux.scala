package oled.winnow

import app.runutils.Globals
import logic.Examples.Example
import logic.{Clause, Literal, Theory}
import utils.ClauseImplicits._

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

  /**
    *
    * @return The marked rules and the marked rule preds (e.g. rule(234234)) as a single string ready for ASP use.
    *         Also a map of ruleId --> rule
    */
  def marked(clauses: Vector[Clause], globals: Globals): (String, Map[String, Clause]) = {
    val allRefinements = clauses flatMap(_.refinements)
    val allRules = clauses ++ allRefinements
    //val allRules = clauses
    /*
    val markedTheory = clauses map (x => marked(x, globals))
    val markedRefinements = allRefinements map (x => marked(x, globals))
    */

    println(allRules.map(_.w))

    val markedTheory = clauses map (x => markedQuickAndDirty(x, globals))
    val markedRefinements = allRefinements map (x => markedQuickAndDirty(x, globals))

    val allRulesMarked = markedTheory ++ markedRefinements
    //val allRulesMarked = markedTheory

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



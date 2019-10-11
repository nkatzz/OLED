package woled

import akka.actor.{Actor, ActorRef, Props}
import app.runutils.IOHandling.Source
import app.runutils.{Globals, RunningOptions}
import logic.Examples.Example
import logic.{Clause, PosLiteral, Theory}
import oled.functions.SingleCoreOLEDFunctions
import oled.functions.SingleCoreOLEDFunctions.generateNewRules
import oled.weightlearn.parallel.IO.FinishedBatch
import oled.weightlearn.parallel.WeightedTheoryLearner
import org.slf4j.LoggerFactory
import utils.ASP

import scala.io.Source

class Learner[T <: app.runutils.IOHandling.Source](inps: RunningOptions, trainingDataOptions: T,
                           testingDataOptions: T, trainingDataFunction: T => Iterator[Example],
                           testingDataFunction: T => Iterator[Example],
                           targetClass: String) extends
  WeightedTheoryLearner(inps, trainingDataOptions, testingDataOptions, trainingDataFunction, testingDataFunction, targetClass) {

  import context.become

  private val logger = LoggerFactory.getLogger(self.path.name)

  private val state = inps.globals.state

  var batchCount = 0

  // Use a hand-crafted theory for debugging
  val source = Source.fromFile("/home/nkatz/dev/BKExamples/BK-various-taks/WeightLearning/Caviar/fragment/meeting/ASP/asp-rules-test")
  val list = source.getLines
  val rulesList = list.map(x => Clause.parse(x)).toList
  source.close
  inps.globals.state.updateRules(rulesList, "add")


  def inferenceState: Receive = { ??? }

  override def processingState: Receive = {

    case batch: Example =>

      println(s"\n *** BATCH $batchCount *** ")

      // Get the data in MLN format by doing numerical stuff thresholds etc. with clingo
      // and getting the atoms expected by the mode declarations

      val program = {
        val nar = batch.narrative.map(_+".").mkString("\n")
        val include = s"""#include "${inps.globals.BK_WHOLE_EC}"."""
        val show = inps.globals.bodyAtomSignatures.map(x => s"#show ${x.tostring}.").mkString("\n")
        Vector(nar, include, show)
      }

      // This transforms the actual data into an MLN-compatible form.
      val f = woled.Utils.dumpToFile(program)
      val t = ASP.solve(task=Globals.INFERENCE, aspInputFile=f)
      val answer = t.head.atoms
      val e = new Example(annot = batch.annotation, nar = answer, _time = batch.time)

      /*if (e.annotation.nonEmpty) {
        // I'll pass the current bottom theory in, to avoid creating redundant rules (avoid explicit compression).
        val t = inps.globals.state.getTheory()
        val bcs = if (t.nonEmpty) inps.globals.state.getTheory().map(x => x.supportSet.clauses.head) else Nil
        val newRules = WoledUtils.generateNewRules(Theory(bcs), e, inps.globals)
        inps.globals.state.updateRules(newRules)
      }*/

      /*val topInit = state.initiationRules
      val topTerm = state.terminationRules
      val growNewInit = Theory(topInit).growNewRuleTest(e, "initiatedAt", inps.globals)
      val growNewTerm = Theory(topTerm).growNewRuleTest(e, "terminatedAt", inps.globals)
      val newInit = if (growNewInit) oled.functions.SingleCoreOLEDFunctions.generateNewRules(Theory(topInit), e, "initiatedAt", inps.globals) else Nil
      val newTerm = if (growNewTerm) oled.functions.SingleCoreOLEDFunctions.generateNewRules(Theory(topTerm), e, "terminatedAt", inps.globals) else Nil
      state.updateRules(newInit ++ newTerm, "add")*/

      // We perform inference with the top rules only. The main reason is that there is a problem with conversion to CNF
      // when many clauses are involved. It hangs, often eats up all the memory etc.
      val rules = state.getAllRules(inps.globals, "top") // use only top rules until the compileCNF issue is resolved.

      //val _rules = inps.globals.state.getAllRules(inps.globals)
      //val rules = Theory.compressTheory(_rules) // this is not ok, (FIX THE THEORY TREE FOR CONCISE REPRESENTATION OF RULES). If I compress this, some refinements won't be scored

      // MAP inference and getting true groundings with Clingo (see below should be performed in parallel)
      val inferredState = WoledUtils.getInferredState(Theory.compressTheory(rules), e, "MAP", inps)

      val test = Scoring.score(e, inferredState, state.getAllRules(inps.globals, "all").toVector, inps)


      // Parallelizing this is trivial (to speed things up in case of many rules/large batches).
      // Simply split the rules to multiple workers, the grounding/counting tasks executed are completely rule-independent.
      val (predictionsPerRuleMap, ruleIdsMap, exmplsCount) = WoledUtils.getTrueRulesGroundings(e, state.getAllRules(inps.globals, "all").toVector, inps)

      val (batchTPs, batchFPs, batchFNs) = WoledUtils.getRulesMistakes(inferredState.keySet, predictionsPerRuleMap, ruleIdsMap, e, exmplsCount, inps)

      state.perBatchError = state.perBatchError :+ (batchFPs.size+batchFNs.size)

      if (batchFPs.nonEmpty || batchFNs.nonEmpty) {
        val topInit = state.initiationRules
        val topTerm = state.terminationRules
        val growNewInit = Theory(topInit).growNewRuleTest(e, "initiatedAt", inps.globals)
        val growNewTerm = Theory(topTerm).growNewRuleTest(e, "terminatedAt", inps.globals)
        val newInit = if (growNewInit) oled.functions.SingleCoreOLEDFunctions.generateNewRules(Theory(topInit), e, "initiatedAt", inps.globals) else Nil
        val newTerm = if (growNewTerm) oled.functions.SingleCoreOLEDFunctions.generateNewRules(Theory(topTerm), e, "terminatedAt", inps.globals) else Nil
        state.updateRules(newInit ++ newTerm, "add")

        // Turn this into a class
        /*val program = {
          val actuallyTrue = e.annotation.map(x => s"actuallyTrue($x).").mkString("")+"\n"
          val fps = batchFPs.map(x => s"falsePositive($x).").mkString("")+"\n"
          val fns = batchFNs.map(x => s"falseNegative($x).").mkString("")+"\n"
          val narrative = e.narrativeASP.mkString("")+"\n"
          val bk = s"""#include "${inps.globals.BK_WHOLE_EC}"."""
          Vector("%Actually true:\n", actuallyTrue, "%False Positives:\n", fps, "%False Negatives:\n", fns, "%Observations:\n", narrative, bk)
        }
        val input = woled.Utils.dumpToFile(program)
        println(input.getAbsolutePath)*/




      }

      /*if (state.initiationRules.nonEmpty) {
        Theory(state.initiationRules).scoreRules(e, inps.globals)
      }
      if (state.terminationRules.nonEmpty) {
        Theory(state.terminationRules).scoreRules(e, inps.globals)
      }*/

      println(rules.map(x => s"(${x.tps}, ${x.fps}, ${x.fns}, ${x.seenExmplsNum})").mkString(" "))

      // We only need the top rules for expansion here.
      val init = inps.globals.state.initiationRules
      val term = inps.globals.state.terminationRules

      val expandedTheory = SingleCoreOLEDFunctions.expandRules(Theory(init ++ term), inps, logger)

      inps.globals.state.updateRules(expandedTheory._1.clauses, "replace")

      println(Theory(state.getAllRules(inps.globals, "top")).showWithStats)

      batchCount += 1
      become(normalState)
      self ! new FinishedBatch

  }



}

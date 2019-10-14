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
  inps.globals.state.updateRules(rulesList, "add", inps)


  def inferenceState: Receive = { ??? }

  override def processingState: Receive = {

    case batch: Example =>

      logger.info(s"\n\n\n *** BATCH $batchCount *** ")

      if (batchCount == 277) {
        val stop = "stop"
      }

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

      // We perform inference with the top rules only. The main reason is that there is a problem with conversion to CNF
      // when many clauses are involved. It hangs, often eats up all the memory etc.
      val rules = state.getAllRules(inps.globals, "top") // use only top rules until the compileCNF issue is resolved.

      // MAP inference and getting true groundings with Clingo (see below should be performed in parallel)
      val inferredState = WoledUtils.getInferredState(Theory.compressTheory(rules), e, "MAP", inps)

      // Parallelizing this is trivial (to speed things up in case of many rules/large batches).
      // Simply split the rules to multiple workers, the grounding/counting tasks executed are completely rule-independent.
      val (batchTPs, batchFPs, batchFNs, totalGroundings) = Scoring.score(e, inferredState, state.getAllRules(inps.globals, "all").toVector, inps, logger)

      // If this is parallelized the global error needs to be updated in a coordinated fashion.
      // The best is to parallelize rule scoring and have another actor calculate batchTPs/FPs/FNs & batch groundings.
      state.perBatchError = state.perBatchError :+ (batchFPs+batchFNs)
      state.totalGroundings += totalGroundings

      /*state.updateGroundingsCounts(totalGroundings)

      if (batchFPs != 0 || batchFNs != 0) {
        val topInit = state.initiationRules
        val topTerm = state.terminationRules
        val growNewInit = Theory(topInit).growNewRuleTest(e, "initiatedAt", inps.globals)
        val growNewTerm = Theory(topTerm).growNewRuleTest(e, "terminatedAt", inps.globals)
        val newInit = if (growNewInit) oled.functions.SingleCoreOLEDFunctions.generateNewRules(Theory(topInit), e, "initiatedAt", inps.globals) else Nil
        val newTerm = if (growNewTerm) oled.functions.SingleCoreOLEDFunctions.generateNewRules(Theory(topTerm), e, "terminatedAt", inps.globals) else Nil
        state.updateRules(newInit ++ newTerm, "add", inps)
      }

      // We only need the top rules for expansion here.
      val init = inps.globals.state.initiationRules
      val term = inps.globals.state.terminationRules
      val expandedTheory = SingleCoreOLEDFunctions.expandRules(Theory(init ++ term), inps, logger)
      inps.globals.state.updateRules(expandedTheory._1.clauses, "replace", inps)*/

      //println(Theory(state.getAllRules(inps.globals, "top")).showWithStats)

      batchCount += 1
      become(normalState)
      self ! new FinishedBatch

  }



}

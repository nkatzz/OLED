package woled

import akka.actor.{Actor, ActorRef, Props}
import app.runutils.IOHandling.Source
import app.runutils.{Globals, RunningOptions}
import logic.Examples.Example
import logic.{Clause, Literal, Theory}
import oled.functions.SingleCoreOLEDFunctions
import oled.weightlearn.parallel.IO.FinishedBatch
import oled.weightlearn.parallel.WeightedTheoryLearner
import org.slf4j.LoggerFactory
import utils.ASP

import scala.io.Source
import scala.util.matching.Regex

class Learner[T <: app.runutils.IOHandling.Source](inps: RunningOptions, trainingDataOptions: T,
                           testingDataOptions: T, trainingDataFunction: T => Iterator[Example],
                           testingDataFunction: T => Iterator[Example],
                           targetClass: String) extends
  WeightedTheoryLearner(inps, trainingDataOptions, testingDataOptions, trainingDataFunction, testingDataFunction, targetClass) {

  import context.become

  private val logger = LoggerFactory.getLogger(self.path.name)

  private val state = inps.globals.state

  private var inertiaAtoms = Vector.empty[Literal]

  var batchCount = 0

  var withHandCrafted = false

  // Use a hand-crafted theory for debugging
  def matches(p: Regex, str: String) = p.pattern.matcher(str).matches
  val source = Source.fromFile("/home/nkatz/dev/BKExamples/BK-various-taks/WeightLearning/Caviar/fragment/meeting/ASP/asp-rules-test")
  val list = source.getLines.filter(line => !matches( """""".r, line) && !line.startsWith("%"))
  val rulesList = list.map(x => Clause.parse(x)).toList
  source.close
  inps.globals.state.updateRules(rulesList, "add", inps)
  withHandCrafted = true

  def inferenceState: Receive = { ??? }

  override def processingState: Receive = {

    case batch: Example =>

      logger.info(s"\n\n\n *** BATCH $batchCount *** ")

      if (batchCount == 56) {
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
      var rules = state.getAllRules(inps.globals, "top") // use only top rules until the compileCNF issue is resolved.

      if (this.inertiaAtoms.nonEmpty) {
        val stop = "stop"
      }

      // MAP inference and getting true groundings with Clingo (see below should be performed in parallel)
      this.inertiaAtoms = Vector.empty[Literal]
      var inferredState = WoledUtils.getInferredState(Theory.compressTheory(rules), e, this.inertiaAtoms, "MAP", inps)

      // Parallelizing this is trivial (to speed things up in case of many rules/large batches).
      // Simply split the rules to multiple workers, the grounding/counting tasks executed are completely rule-independent.
      val (tpCounts, fpCounts, fnCounts, totalGroundings, inertiaAtoms) = Scoring.scoreAndUpdateWeights(e, inferredState, state.getAllRules(inps.globals, "all").toVector, inps, logger)

      this.inertiaAtoms = inertiaAtoms

      if (this.inertiaAtoms.nonEmpty) {
        val stop = "stop"
      }

      //val (fps, fns, tpCounts, fpCounts, fnCounts) = Scoring.getMistakes(inferredState, batch)
      state.perBatchError = state.perBatchError :+ (fpCounts + fnCounts)

      logger.info(s"\n${state.perBatchError}")
      logger.info(s"\nFPs: $fpCounts, FNs: $fnCounts")



      if (!withHandCrafted) {

        state.totalGroundings += totalGroundings
        state.updateGroundingsCounts(totalGroundings)

        // Generate new rules with abduction & everything. This should be removed...
        if (fpCounts != 0 || fnCounts != 0) {
          val topInit = state.initiationRules
          val topTerm = state.terminationRules
          val growNewInit = Theory(topInit).growNewRuleTest(e, "initiatedAt", inps.globals)
          val growNewTerm = Theory(topTerm).growNewRuleTest(e, "terminatedAt", inps.globals)
          val newInit = if (growNewInit) oled.functions.SingleCoreOLEDFunctions.generateNewRules(Theory(topInit), e, "initiatedAt", inps.globals) else Nil
          val newTerm = if (growNewTerm) oled.functions.SingleCoreOLEDFunctions.generateNewRules(Theory(topTerm), e, "terminatedAt", inps.globals) else Nil
          state.updateRules(newInit ++ newTerm, "add", inps)
        }

        // Generate a few more rules randomly from mistakes. Initiation rules from the FNs and termination from the FPs.
        /*if (fpCounts != 0 || fnCounts != 0) {
          val (a, b) = Scoring.generateNewRules(fps, fns, batch, inps, logger)
          state.updateRules(a.toList ++ b.toList, "add", inps)
        }*/

        /* Weights updates. */

        // We need to fetch the theory again to factor in the new rules.
        //rules = state.getAllRules(inps.globals, "top")

        // MAP inference again to factor in the new rules.
        //inferredState = WoledUtils.getInferredState(Theory.compressTheory(rules), e, "MAP", inps)

        // Parallelizing this is trivial (to speed things up in case of many rules/large batches).
        // Simply split the rules to multiple workers, the grounding/counting tasks executed are completely rule-independent.
        //val (_, _, _, totalGroundings) = Scoring.scoreAndUpdateWeights(e, inferredState, state.getAllRules(inps.globals, "all").toVector, inps, logger)



        /* Rules' expansion. */

        // We only need the top rules for expansion here.
        val init = inps.globals.state.initiationRules
        val term = inps.globals.state.terminationRules
        val expandedTheory = SingleCoreOLEDFunctions.expandRules(Theory(init ++ term), inps, logger)
        inps.globals.state.updateRules(expandedTheory._1.clauses, "replace", inps)

        //println(Theory(state.getAllRules(inps.globals, "top")).showWithStats)
      }



      batchCount += 1
      become(normalState)
      self ! new FinishedBatch

  }



}

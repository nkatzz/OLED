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
  /*def matches(p: Regex, str: String) = p.pattern.matcher(str).matches
  val source = Source.fromFile("/home/nkatz/dev/BKExamples/BK-various-taks/WeightLearning/Caviar/fragment/meeting/ASP/asp-rules-test")
  val list = source.getLines.filter(line => !matches( """""".r, line) && !line.startsWith("%"))
  val rulesList = list.map(x => Clause.parse(x)).toList
  source.close
  inps.globals.state.updateRules(rulesList, "add", inps)
  withHandCrafted = true*/

  def inferenceState: Receive = { ??? }

  override def processingState: Receive = {

    case batch: Example =>

      logger.info(s"\n\n\n *** BATCH $batchCount *** ")

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

      var rules = List.empty[Clause]
      var inferredState = Map.empty[String, Boolean]
      var tpCounts = 0
      var fpCounts = 0
      var fnCounts = 0
      var totalGroundings = 0
      var inertiaAtoms =  Vector.empty[Literal]

      // MAP inference and getting true groundings with Clingo (see below should be performed in parallel)
      //this.inertiaAtoms = Vector.empty[Literal]              // Use this to difuse inertia

      /*=============== WOLED ================*/
      if (inps.weightLean) { // set --weight-learning=true to run WOLED, =false to run OLED

        // We perform inference with the top rules only. The main reason is that there is a problem with conversion to CNF
        // when many clauses are involved. It hangs, often eats up all the memory etc.
        rules = state.getAllRules(inps.globals, "top")
        //rules = state.getBestRules(inps.globals)

        println("MAP inference...")
        inferredState = WoledUtils.getInferredState(Theory.compressTheory(rules), e, this.inertiaAtoms, "MAP", inps)

        // Parallelizing this is trivial (to speed things up in case of many rules/large batches).
        // Simply split the rules to multiple workers, the grounding/counting tasks executed are completely rule-independent.
        println("Scoring...")
        val (_tpCounts, _fpCounts, _fnCounts, _totalGroundings, _inertiaAtoms) = Scoring.scoreAndUpdateWeights(e, inferredState, state.getAllRules(inps.globals, "all").toVector, inps, logger)
        tpCounts = _tpCounts
        fpCounts = _fpCounts
        fnCounts = _fnCounts
        totalGroundings = _totalGroundings
        inertiaAtoms =_inertiaAtoms

      /*=============== OLED ================*/
      } else {
        rules = state.getBestRules(inps.globals, "score").filter(x => x.score >= 0.7)
        val (_tpCounts, _fpCounts, _fnCounts, _, _, _) = oled.functions.SingleCoreOLEDFunctions.eval(Theory(rules), e, inps)
        tpCounts = _tpCounts
        fpCounts = _fpCounts
        fnCounts = _fnCounts
        val initTheory = Theory(state.initiationRules)
        val termTheory = Theory(state.terminationRules)
        if (initTheory.clauses.nonEmpty) initTheory.scoreRules(e, inps.globals)
        if (termTheory.clauses.nonEmpty) termTheory.scoreRules(e, inps.globals)
      }

      this.inertiaAtoms = inertiaAtoms
      this.inertiaAtoms = Vector.empty[Literal]              // Use this to difuse inertia

      state.perBatchError = state.perBatchError :+ (fpCounts + fnCounts)

      logger.info(s"\n${state.perBatchError}")
      logger.info(s"\nFPs: $fpCounts, FNs: $fnCounts")

      if (!withHandCrafted) {

        state.totalGroundings += totalGroundings
        state.updateGroundingsCounts(totalGroundings)

        // Generate new rules with abduction & everything. This should be removed...
        println("Generating new rules...")
        var newInit = List.empty[Clause]
        var newTerm = List.empty[Clause]
        if (fpCounts != 0 || fnCounts != 0) {
          val topInit = state.initiationRules
          val topTerm = state.terminationRules
          val growNewInit = Theory(topInit).growNewRuleTest(e, "initiatedAt", inps.globals)
          val growNewTerm = Theory(topTerm).growNewRuleTest(e, "terminatedAt", inps.globals)
          newInit = if (growNewInit) oled.functions.SingleCoreOLEDFunctions.generateNewRules(Theory(topInit), e, "initiatedAt", inps.globals) else Nil
          newTerm = if (growNewTerm) oled.functions.SingleCoreOLEDFunctions.generateNewRules(Theory(topTerm), e, "terminatedAt", inps.globals) else Nil
          state.updateRules(newInit ++ newTerm, "add", inps)
        }

        // Generate a few more rules randomly from mistakes. Initiation rules from the FNs and termination from the FPs.
        /*if (fpCounts != 0 || fnCounts != 0) {
          val (a, b) = Scoring.generateNewRules(fps, fns, batch, inps, logger)
          state.updateRules(a.toList ++ b.toList, "add", inps)
        }*/

        val newRules = newInit ++ newTerm
        // score the new rules and update their weights
        Scoring.scoreAndUpdateWeights(e, inferredState, newRules.toVector, inps, logger)

        /* Rules' expansion. */
        // We only need the top rules for expansion here.
        val init = inps.globals.state.initiationRules
        val term = inps.globals.state.terminationRules
        val expandedTheory = SingleCoreOLEDFunctions.expandRules(Theory(init ++ term), inps, logger)
        inps.globals.state.updateRules(expandedTheory._1.clauses, "replace", inps)

        inps.globals.state.pruneRules(inps.pruneThreshold)

        // Do a MAP inference and scoring step again, to update weights for the newly generated rules.
        // This is necessary for large batch sizes. This should be fixed so as to perform this step only
        // for new BC-generated rules, by grounding those (only) and computing differences from current MAP state.
        // UPDATE: This degrades performance. It needs to be done right.
        /*rules = state.getAllRules(inps.globals, "top")
        inferredState = WoledUtils.getInferredState(Theory.compressTheory(rules), e, this.inertiaAtoms, "MAP", inps)
        Scoring.scoreAndUpdateWeights(e, inferredState, state.getAllRules(inps.globals, "all").toVector, inps, logger)*/

        //println(Theory(state.getAllRules(inps.globals, "top")).showWithStats)
      }



      batchCount += 1
      become(normalState)
      self ! new FinishedBatch

  }



}

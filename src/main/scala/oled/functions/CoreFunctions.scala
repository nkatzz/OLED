/*
 * Copyright (C) 2016  Nikos Katzouris
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

package oled.functions

import java.text.DecimalFormat

import app.runutils.{Globals, RunningOptions}
import com.mongodb.casbah.MongoClient
import logic.Examples.Example
import logic._
import utils._
import utils.Implicits._

/**
  * Created by nkatz on 20/6/2017.
  */



trait CoreFunctions {

  /**
    * This trait contains functionality used by all versions of OLED.
    *
    * */




  def filterTriedRules(T: Theory, newRules: Iterable[Clause], logger: org.slf4j.Logger) = {
    val out = newRules.filter { newRule =>
      val bottomClauses = T.clauses.map(x => x.supportSet.clauses.head)
      val bottom = newRule.supportSet.clauses.head
      //!bottomClauses.exists(x => x.thetaSubsumes(bottom) && bottom.thetaSubsumes(x))
      !bottomClauses.exists(x => x.thetaSubsumes(bottom)) // this suffices for it to be excluded.
    }
    if (out.size != newRules.size) logger.info("Dropped new clause (repeated bottom clause)")
    out
  }

  /* Determines if a fluent holds initially in a mini-batch.
  * This is used in the functionality that supports learning
  * strongly initiated fluents. If we know that a fluent is strongly
  * initiated then we don't learn new rules from mini-batches where the fluent holds initially.*/
  def holdsInitially(e: Example) = {
    if (e.annotation.isEmpty) {
      false
    } else {
      val timeAll = e.narrative.map(x => Literal.parse(x).terms.last.tostring.toInt).sorted
      val labelsTime = e.annotation.map(x => Literal.parse(x).terms.last.tostring.toInt).sorted
      timeAll.head == labelsTime.head
    }
  }

  def startNewRules_withInertia(e: Example) = {
    if (e.annotation.isEmpty) {
      false
    } else {
      // Try to abduce initiation & termination atoms with Inertia.
      // If some abducibles are returned, construct new rules from them
    }
  }

  /* Online pruning. */
  def pruneRules(topTheory: Theory, inps: RunningOptions, logger: org.slf4j.Logger) = {
    // This doesn't work
    /*
    val pruned = topTheory.clauses.foldLeft(List[Clause]()){ (keep, clause) =>
      val epsilon = Utils.hoeffding(inps.delta, clause.seenExmplsNum)
      val meanPruningScore = clause.meanScorePruning(inps.pruneThreshold)
      //if (this.pruningThreshold - meanScore > epsilon && clause.seenExmplsNum > minSeenExmpls) {
      if (meanPruningScore > epsilon && clause.seenExmplsNum > inps.minSeenExmpls*10) {
        logger.info(s"\nPruned clause:\n${clause.tostring}\nMean score" +
          s" so far: ${clause.meanScorePruning(inps.pruneThreshold)} | tps: ${clause.tps} fps: ${clause.fps}, fns: ${clause.fns}")
        keep
      } else {
        keep :+ clause
      }
    }
    Theory(pruned)
    */
    val (keep, prune) = topTheory.clauses.foldLeft(List[Clause](), List[Clause]()) { (accum, clause) =>
      if (clause.seenExmplsNum > inps.pruneAfter && clause.score < inps.pruneThreshold) {
        (accum._1, accum._2 :+ clause)
      } else {
        (accum._1 :+ clause, accum._2)
      }
    }
    prune.foreach(x => logger.info(s"\nPruned clause\n${x.showWithStats}"))
    Theory(keep)
  }

  def generateNewBottomClauses(topTheory: Theory, e: Example, initorterm: String, globals: Globals) = {

    val terminatedOnly = if(initorterm == "terminatedAt") true else false
    val specialBKfile = if(initorterm=="initiatedAt") globals.BK_INITIATED_ONLY else globals.BK_TERMINATED_ONLY
    val (_, varKernel) =
      LogicUtils.generateKernel(e.toMapASP, learningTerminatedOnly = terminatedOnly, bkFile = specialBKfile, globals=globals)
    val bottomTheory = topTheory.clauses flatMap(x => x.supportSet.clauses)
    val goodKernelRules = varKernel.filter(newBottomRule => !bottomTheory.exists(supportRule => newBottomRule.thetaSubsumes(supportRule)))
    goodKernelRules
  }


  def generateNewBottomClausesNoEC(topTheory: Theory, e: Example, globals: Globals) = {
    val specialBKfile = globals.BK_WHOLE
    val (_, varKernel) = LogicUtils.generateKernel(e.toMapASP, bkFile = specialBKfile, globals=globals)
    val bottomTheory = topTheory.clauses flatMap(x => x.supportSet.clauses)
    val goodKernelRules = varKernel.filter(newBottomRule => !bottomTheory.exists(supportRule => newBottomRule.thetaSubsumes(supportRule)))
    goodKernelRules
  }

  def getnerateNewBottomClauses_withInertia(topTheory: Theory, e: Example, targetClass: String, globals: Globals) = {
    val specialBKfile = globals.ABDUCE_WITH_INERTIA
    val (_, varKernel) = LogicUtils.generateKernel(e.toMapASP, bkFile = specialBKfile, globals=globals)
    val filteredKernel = varKernel.filter(p => p.head.functor == targetClass)
    val bottomTheory = topTheory.clauses flatMap(x => x.supportSet.clauses)
    val goodKernelRules = filteredKernel.filter(newBottomRule => !bottomTheory.exists(supportRule => newBottomRule.thetaSubsumes(supportRule)))
    goodKernelRules map { x =>
      val c = Clause(head=x.head, body = List())
      c.addToSupport(x)
      c
    }
  }








  def showInfo(c: Clause, c1: Clause, c2: Clause, hoeffding: Double, observedDiff: Double, n: Int, inps: RunningOptions) = {

    def format(x: Double) = {
      val defaultNumFormat = new DecimalFormat("0.############")
      defaultNumFormat.format(x)
    }

    val showRefs = inps.showRefs
    val weightLearn = inps.weightLean

    if (showRefs) {
      if(!weightLearn) {
        s"\n===========================================================\n" +
          s"\nClause (score: ${c.score} | tps: ${c.tps} fps: ${c.fps} fns: ${c.fns})\n\n${c.tostring}\n\nwas refined to" +
          s" (new score: ${c1.score} | tps: ${c1.tps} fps: ${c1.fps} fns: ${c1.fns})\n\n${c1.tostring}\n\nε: $hoeffding, ΔG: $observedDiff, examples used: $n" +
          //s"\nall refs: \n\n ${c.refinements.sortBy(z => -z.score).map(x => x.tostring+" "+" | score "+x.score+" | similarity "+similarity(x)).mkString("\n")}" +
          s"\nall refs: \n\n ${c.refinements.sortBy(z => (-z.score,z.body.length+1)).map(x => x.tostring+" | score "+x.score+" (tps|fps|fns): "+(x.tps,x.fps,x.fns)).mkString("\n")}" +
          s"\n===========================================================\n"
      } else {
        s"\n===========================================================\n" +
          s"\nClause (score: ${c.score} | tps: ${c.tps} fps: ${c.fps} fns: ${c.fns} | MLN-weight: ${format(c.mlnWeight)})\n\n${c.tostring}\n\nwas refined to" +
          s" (new score: ${c1.score} | tps: ${c1.tps} fps: ${c1.fps} fns: ${c1.fns} | MLN-weight: ${format(c1.mlnWeight)})\n\n${c1.tostring}\n\nε: $hoeffding, ΔG: $observedDiff, examples used: $n" +
          //s"\nall refs: \n\n ${c.refinements.sortBy(z => -z.score).map(x => x.tostring+" "+" | score "+x.score+" | similarity "+similarity(x)).mkString("\n")}" +
          s"\nall refs: \n\n ${c.refinements.sortBy(z => (-z.score,z.body.length+1)).map(x => x.tostring+" | score "+x.score+" (tps|fps|fns): "+(x.tps,x.fps,x.fns) + "| MLN-weight: "+format(x.mlnWeight)).mkString("\n")}" +
          s"\n===========================================================\n"
      }

    } else {
      s"\n===========================================================\n" +
        s"\nClause (score: ${c.score} | tps: ${c.tps} fps: ${c.fps} fns: ${c.fns} | MLN-weight: ${format(c.mlnWeight)})\n\n${c.tostring}\n\nwas refined to" +
        s" (new score: ${c1.score} | tps: ${c1.tps} fps: ${c1.fps} fns: ${c1.fns} | MLN-weight: ${format(c1.mlnWeight)})\n\n${c1.tostring}\n\nε: $hoeffding, ΔG: $observedDiff, examples used: $n" +
        //s"\nall refs: \n\n ${c.refinements.sortBy(z => -z.score).map(x => x.tostring+" "+" | score "+x.score+" | similarity "+similarity(x)).mkString("\n")}" +
        //s"\nall refs: \n\n ${c.refinements.sortBy(z => (-z.score,z.body.length+1)).map(x => x.tostring+" | score "+x.score+" (tps|fps|fns): "+(x.tps,x.fps,x.fns)).mkString("\n")}" +
        s"\n===========================================================\n"
    }
  }



  def crossVal(t: Theory,
               data: Iterator[Example],
               handCraftedTheoryFile: String = "",
               globals: Globals,
               inps: RunningOptions) = {

    while (data.hasNext) {
      val e = data.next()
      //println(e.time)
      evaluateTheory(t, e, handCraftedTheoryFile, globals)
    }
    val stats = t.stats
    (stats._1, stats._2, stats._3, stats._4, stats._5, stats._6)
  }


  /* Evaluate a theory on a single batch */
  def eval(t: Theory, exmpl: Example, inps: RunningOptions, inputTheoryFile: String = "") = {
    evaluateTheory(t, exmpl, handCraftedTheoryFile = inputTheoryFile, inps.globals)
    val stats = t.stats
    t.clearStats()
    (stats._1, stats._2, stats._3, stats._4, stats._5, stats._6)
  }


  def evaluateTheory(theory: Theory, e: Example, handCraftedTheoryFile: String = "", globals: Globals): Unit = {

    if (e.annotation.nonEmpty) {
      val stop = "stop"
    }

    val varbedExmplPatterns = globals.EXAMPLE_PATTERNS_AS_STRINGS
    val coverageConstr = s"${globals.TPS_RULES}\n${globals.FPS_RULES}\n${globals.FNS_RULES}"
    val t =
      if(theory != Theory()) {
        theory.clauses.map(x => x.withTypePreds(globals).tostring).mkString("\n")
      } else {
        globals.INCLUDE_BK(handCraftedTheoryFile)
      }
    val show = globals.SHOW_TPS_ARITY_1 + globals.SHOW_FPS_ARITY_1 + globals.SHOW_FNS_ARITY_1
    val ex = e.tostring
    val program = ex + globals.INCLUDE_BK(globals.BK_CROSSVAL) + t + coverageConstr + show
    val f = Utils.getTempFile("isConsistent",".lp")
    Utils.writeLine(program, f, "overwrite")
    val answerSet = ASP.solve(task = Globals.INFERENCE, aspInputFile = f)
    if (answerSet.nonEmpty) {
      val atoms = answerSet.head.atoms
      atoms.foreach { a=>
        val lit = Literal.parse(a)
        val inner = lit.terms.head
        lit.functor match {
          case "tps" => theory.tps += inner.tostring
          case "fps" => theory.fps += inner.tostring
          case "fns" => theory.fns += inner.tostring
        }
      }
    }
  }





  /*
  *
  * Helper method to compute the running mean for various statistics.
  * Currently not used anywhere. Needs fixing, the vars should be manually updated
  * with new values. In general, that's stupid, I don't know why I'm not deleting this.
  * The vars where updated like this every time an example was processed:
  *
  *     //----------------------------------------------------------------
  *      // Getting some runtime stats
  *      //val lastProcTime = res._2
  *      //val newMeanProcTime = getRunningMean("time",lastProcTime)
  *      //val newMeanGrndSize = getRunningMean("groundPrg",GlobalValues.grnd.toDouble)
  *      //logger.debug(s"\nExmpl ${this.exmplCounter} Mean processing time so far: $newMeanProcTime sec\nMean grnd prg size: $newMeanGrndSize")
  *      // Updating global vars for the next computation of running means
  *      //this.timeRunningMean = newMeanProcTime
  *      //this.groundRunningMean = newMeanGrndSize
  *      //this.exmplCounter += 1
  *      //----------------------------------------------------------------
  * */
  private def getRunningMean(what: String, newValue: Double) = {

    // These are vars used to compute runtime statistics
    // (running averages over the stream), like mean
    // processing time per example and mean size of the
    // ground program per example
    var timeRunningMean = 0.0
    var groundRunningMean = 0.0
    var exmplCounter = 0

    // The running average can be computed by
    // ((prevAvg*n) + newValue)/(n+1)
    // where n is the number of seen data points
    def runningMean(prevAvg: Double, newVal: Double, n: Int) = ((prevAvg*n) + newValue)/(n+1)
    what match {
      case "time" => runningMean(timeRunningMean, newValue, exmplCounter)
      case "groundPrg" => runningMean(groundRunningMean, newValue, exmplCounter)
    }
  }


}



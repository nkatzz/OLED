package oled.functions

import app.runutils.{Globals, RunningOptions}
import com.mongodb.casbah.MongoCollection
import jep.Jep
import logic.Examples.Example
import logic.{Clause, Literal, LogicUtils, Theory}
import utils.DataUtils.{DataAsExamples, DataAsIntervals, DataFunction, TrainingSet}
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


  def filterTriedRules(T: Theory, newRules: List[Clause], logger: org.slf4j.Logger) = {
    val out = newRules.filter { newRule =>
      val bottomClauses = T.clauses.map(x => x.supportSet.clauses.head)
      val bottom = newRule.supportSet.clauses.head
      //!bottomClauses.exists(x => x.thetaSubsumes(bottom) && bottom.thetaSubsumes(x))
      !bottomClauses.exists(x => x.thetaSubsumes(bottom)) // this suffices for it to be excluded.
    }
    if (out.length != newRules.length) logger.info("Dropped new clause (repeated bottom clause)")
    out
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

  def generateNewBottomClauses(topTheory: Theory, e: Example, jep: Jep, initorterm: String, globals: Globals) = {

    val terminatedOnly = if(initorterm == "terminatedAt") true else false
    val specialBKfile = if(initorterm=="initiatedAt") globals.BK_INITIATED_ONLY else globals.BK_TERMINATED_ONLY
    val (_, varKernel) =
      LogicUtils.generateKernel(e.toMapASP, jep=jep,
        learningTerminatedOnly = terminatedOnly, bkFile = specialBKfile, globals=globals)
    val bottomTheory = topTheory.clauses flatMap(x => x.supportSet.clauses)
    val goodKernelRules = varKernel.filter(newBottomRule => !bottomTheory.exists(supportRule => newBottomRule.thetaSubsumes(supportRule)))
    goodKernelRules
  }


  def showInfo(c: Clause, c1: Clause, c2: Clause, hoeffding: Double, observedDiff: Double, n: Int, showRefs: Boolean) = {
    if (showRefs) {
      s"\n===========================================================\n" +
        s"\nClause (score: ${c.score} | tps: ${c.tps} fps: ${c.fps} fns: ${c.fns})\n\n${c.tostring}\n\nwas refined to" +
        s" (new score: ${c1.score} | tps: ${c1.tps} fps: ${c1.fps} fns: ${c1.fns})\n\n${c1.tostring}\n\nε: $hoeffding, ΔG: $observedDiff, examples used: $n" +
        //s"\nall refs: \n\n ${c.refinements.sortBy(z => -z.score).map(x => x.tostring+" "+" | score "+x.score+" | similarity "+similarity(x)).mkString("\n")}" +
        s"\nall refs: \n\n ${c.refinements.sortBy(z => (-z.score,z.body.length+1)).map(x => x.tostring+" | score "+x.score+" (tps|fps|fns): "+(x.tps,x.fps,x.fns)).mkString("\n")}" +
        s"\n===========================================================\n"
    } else {
      s"\n===========================================================\n" +
        s"\nClause (score: ${c.score} | tps: ${c.tps} fps: ${c.fps} fns: ${c.fns})\n\n${c.tostring}\n\nwas refined to" +
        s" (new score: ${c1.score} | tps: ${c1.tps} fps: ${c1.fps} fns: ${c1.fns})\n\n${c1.tostring}\n\nε: $hoeffding, ΔG: $observedDiff, examples used: $n" +
        //s"\nall refs: \n\n ${c.refinements.sortBy(z => -z.score).map(x => x.tostring+" "+" | score "+x.score+" | similarity "+similarity(x)).mkString("\n")}" +
        //s"\nall refs: \n\n ${c.refinements.sortBy(z => (-z.score,z.body.length+1)).map(x => x.tostring+" | score "+x.score+" (tps|fps|fns): "+(x.tps,x.fps,x.fns)).mkString("\n")}" +
        s"\n===========================================================\n"
    }
  }



  def crossVal(t: Theory,
               jep: Jep,
               data: Iterator[Example],
               handCraftedTheoryFile: String = "",
               globals: Globals,
               inps: RunningOptions) = {

    while (data.hasNext) {
      val e = data.next()
      println(e.time)
      evaluateTheory(t, e, jep, handCraftedTheoryFile, globals)
    }
    val stats = t.stats
    (stats._1, stats._2, stats._3, stats._4, stats._5, stats._6)
  }


  def evaluateTheory(theory: Theory, e: Example, jep: Jep, handCraftedTheoryFile: String = "", globals: Globals): Unit = {

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
    val f = Utils.getTempFile("isConsistent",".lp",deleteOnExit = true)
    Utils.writeLine(program, f, "overwrite")
    val answerSet = ASP.solve(task = Globals.INFERENCE, aspInputFile = f, jep=jep)
    if (answerSet.nonEmpty) {
      val atoms = answerSet.head.atoms
      atoms.foreach { a=>
        val lit = Literal.toLiteral(a)
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



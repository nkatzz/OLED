package oled_distributed

import app.{Globals, InputParameters}
import jep.Jep
import logic.{Clause, LogicUtils, Theory}
import oled_distributed.Structures.ClauseStats
import utils.{Exmpl, Utils}
import utils.Implicits._
import org.slf4j.Logger
import utils.DataUtils.DataAsIntervals

/**
  * Created by nkatz on 5/16/17.
  */

/*
*
* This basically contains the functionality of the TheoryLearner from the serial setting of OLED.
*
* This code is used for debugging purposes (trying to figure out if the sub-linear speed-up
* is due to blocking)
*
* */

object SequentialRunner {




  def run(learningInitWithInertia: Boolean,
          jep: Jep, initorterm: String, globals: Globals, onlinePruning: Boolean,
          delta: Double, breakTiesThreshold: Double, minSeenExmpls: Int,
          logger: Logger, repeatFor: Int,
          getDataFunction: (String, String, Int, DataAsIntervals) => Iterator[Exmpl],
          intervals: DataAsIntervals = DataAsIntervals(),dbName: String,
          inputParameters: InputParameters): (Theory,Double) = {

    def getTrainData() = getDataFunction(dbName, inputParameters.targetHLE, inputParameters.chunkSize, intervals)

    def runOnce(inTheory: Theory): Theory = {

      val data = getTrainData()

      data.foldLeft(inTheory){ (topTheory, newExample) =>
        //println(newExample.time, topTheory.clauses.length, s"${topTheory.clauses.map(x => (x.tps, x.fps, x.fns)).mkString(" ")}")
        val res = utils.Utils.time {
          processExample(topTheory, newExample, learningInitWithInertia,
            jep, initorterm, globals, onlinePruning, delta, breakTiesThreshold, minSeenExmpls, logger)
        }
        res._1
      }
    }

    //logger.info(s"Starting learning for $targetClass")

    val _finalTheory = utils.Utils.time{ (1 to repeatFor).foldLeft(Theory())( (t,_) =>  runOnce(t)) }
    val (finalTheory,time) = (_finalTheory._1,_finalTheory._2)
    //logger.info(s"\nTraining time for $targetClass: $time")
    val withPostPruning = false
    val output = withPostPruning match {
      case true =>
        /*
        logger.info(s"Starting post-pruning for $targetClass")
        logger.info(s"Rescoring $targetClass theory")
        reScore(DB,finalTheory,chunkSize,this.jep,trainingSetSize,targetClass, withInertia)
        logger.info(s"\nLearned hypothesis (before pruning):\n${finalTheory.showWithStats}")
        val pruned = finalTheory.clauses.filter(x => x.score > pruningThreshold)
        logger.debug(s"\nPruned hypothesis:\n${pruned.showWithStats}")
        //pruned.foreach { p => p.clearStatistics } // No need. I want to display the stats and also they cause no problem since the theory is evaluated as a whole
        Theory(pruned)
        */
        finalTheory
      case _ => finalTheory
    }
    logger.debug(s"\nTheory found:\n${output.tostring}")
    //this.jep.close()
    (output,time)
  }




  def processExample(topTheory: Theory, e: Exmpl, learningInitWithInertia: Boolean,
                     jep: Jep, initorterm: String, globals: Globals, onlinePruning: Boolean,
                     delta: Double, breakTiesThreshold: Double, minSeenExmpls: Int, logger: Logger): Theory = {
    var newTopTheory = topTheory
    val bottomClauses = Theory() // just a quick and dirty solution
    if (bottomClauses.clauses.isEmpty){ // in the opposite case we've collected the BCs in a first pass over the data.
      val startNew = newTopTheory.growNewRuleTest(e, jep, initorterm, globals)
      if (startNew) {
        val newRules = generateNewRules(topTheory, e, learningInitWithInertia, initorterm, jep, globals)
        newTopTheory = topTheory.clauses ++ newRules
      }
    }
    if (newTopTheory.clauses.nonEmpty) {
      //val t0 = System.nanoTime()
      newTopTheory.scoreRules(e.exmplWithInertia, jep, globals)
      //val t1 = System.nanoTime()
      //println(s"scoreRules time: ${(t1-t0)/1000000000.0}")

      try {
        val expanded = expandRules(newTopTheory, delta, breakTiesThreshold, minSeenExmpls, logger)
        if (onlinePruning) {
          // skip this for now
          //pruneRules(expanded)
          expanded
        } else {
          expanded
        }
      } catch {
        case z: IndexOutOfBoundsException =>
          println(s"top theory:\n ${topTheory.tostring}")
          println(e.id)
          Theory()
      }
    } else {
      if (bottomClauses.clauses.isEmpty) {
        newTopTheory
      } else {
        // generate a top theory from the already constructed bottom clauses
        val top = bottomClauses.clauses map { x =>
          val c = Clause(head=x.head, body = List())
          c.addToSupport(x)
          c
        }
        Theory(top)
      }
    }
  }

  def expandRules(topTheory: Theory, delta: Double, breakTiesThreshold: Double, minSeenExmpls: Int, logger: Logger): Theory = {
    val out = topTheory.clauses flatMap { parentRule =>
      val (couldExpand,epsilon,observedDiff,best,secondBest) = rightWay(parentRule, delta, breakTiesThreshold, minSeenExmpls)
      couldExpand match {
        case true =>
          ///*
          // This is the extra test that I added at Feedzai
          val extraTest =
            if(secondBest != parentRule) (best.score > parentRule.score) && (best.score - parentRule.score > epsilon)
            else best.score > parentRule.score
          extraTest match { //&& (1.0/best.body.size+1 > 1.0/parentRule.body.size+1) match {
            case true =>
              val refinedRule = best
              logger.info(showInfo(parentRule, best, secondBest, epsilon, observedDiff, parentRule.seenExmplsNum))
              refinedRule.seenExmplsNum = 0 // zero the counter
              refinedRule.supportSet = parentRule.supportSet // only one clause here
              List(refinedRule)
            case _ => List(parentRule)
          }
        case _ => List(parentRule)
      }
    }
    Theory(out)
  }

  def rightWay(parentRule: Clause, delta: Double, breakTiesThreshold: Double, minSeenExmpls: Int) = {
    val (observedDiff,best,secondBest) = parentRule.meanDiff
    val epsilon = utils.Utils.hoeffding(delta, parentRule.seenExmplsNum)
    val passesTest = if (epsilon < observedDiff) true else false
    //val tie = if (epsilon <= breakTiesThreshold && parentRule.seenExmplsNum >= minSeenExmpls) true else false
    val tie = if (observedDiff < epsilon  && epsilon < breakTiesThreshold && parentRule.seenExmplsNum >= minSeenExmpls) true else false

    val couldExpand = passesTest || tie
    (couldExpand,epsilon,observedDiff,best,secondBest)
  }





  def showInfo(c: Clause, c1: Clause, c2: Clause, hoeffding: Double, observedDiff: Double, n: Int) = {

    /*
    val similarity = (x: Clause) => {
      if (this.targetClass=="initiated"){
        //println(Theory(GlobalValues.CurrentTheoryInitiated).tostring)
        Utils.similarity(x,GlobalValues.CurrentTheoryInitiated)
      } else {
        //println(Theory(GlobalValues.CurrentTheoryTerminated).tostring)
        Utils.similarity(x,GlobalValues.CurrentTheoryTerminated)
      }
    }
    */
    s"\n===========================================================\n" +
      s"\nClause (score: ${c.score} | tps: ${c.tps} fps: ${c.fps} fns: ${c.fns})\n\n${c.tostring}\n\nwas refined to" +
      s" (new score: ${c1.score} | tps: ${c1.tps} fps: ${c1.fps} fns: ${c1.fns})\n\n${c1.tostring}\n\nε: $hoeffding, ΔG: $observedDiff, examples used: $n" +
      //s"\nall refs: \n\n ${c.refinements.sortBy(z => -z.score).map(x => x.tostring+" "+" | score "+x.score+" | similarity "+similarity(x)).mkString("\n")}" +
      s"\nall refs: \n\n ${c.refinements.sortBy(z => (-z.score,z.body.length+1)).map(x => x.tostring+" | score "+x.score+" (tps|fps|fns): "+(x.tps,x.fps,x.fns)).mkString("\n")}" +
      s"\n===========================================================\n"

    /*
    "\n========================================" +
      s"\nHoeffding bound : $hoeffding" +
      s"\nobserved mean difference: $observedDiff" +
      s"\nNumber of examples used: $n\n" +
      "------------------------------------------------\n" +
      "Current rule (" + c.score + "): \n" +
      c.tostring + "\n" +
      "------------------------------------------------\n" +
      "Best refinement (" + c1.score + "): \n" +
      c1.tostring + "\n" +
      "------------------------------------------------\n" +
      "second best refinement (" + c2.score + "): \n" +
      c2.tostring + "\n" +
      "============================================"
    */
  }

  def generateNewRules(topTheory: Theory, e: Exmpl, learningInitWithInertia: Boolean = false, initorterm: String, jep: Jep, globals: Globals) = {
    val terminatedOnly = if(initorterm=="terminatedAt") true else false
    val specialBKfile = if(initorterm=="initiatedAt") globals.BK_INITIATED_ONLY else globals.BK_TERMINATED_ONLY
    val (_, varKernel) =
      LogicUtils.generateKernel(e.exmplWithInertia.toMapASP, jep = jep, learningTerminatedOnly = terminatedOnly,
        oledLearningInitWithInertia = learningInitWithInertia, bkFile = specialBKfile, globals=globals)
    val bottomTheory = topTheory.clauses flatMap(x => x.supportSet.clauses)
    val goodKernelRules = varKernel.filter(newBottomRule => !bottomTheory.exists(supportRule => newBottomRule.thetaSubsumes(supportRule)))
    ///*
    goodKernelRules map { x =>
      val c = Clause(head=x.head, body = List())
      //println(x.tostring)
      c.addToSupport(x)
      val otherNodeNames = List("", "") //  just a quick solution to avoid crashes generated by debugging exception at the distributed setting
      otherNodeNames.foreach{ node =>
        c.countsPerNode(node) = new ClauseStats(0, 0, 0, 0)
      }
      c.generateCandidateRefs
      c
    }
  }


}

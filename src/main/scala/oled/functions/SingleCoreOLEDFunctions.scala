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

import app.runutils.{Globals, RunningOptions}
import com.mongodb.casbah.MongoClient
import com.typesafe.scalalogging.LazyLogging
import logic.Examples.Example
import logic.{AnswerSet, Clause, Theory}
import utils.ASP.getCoverageDirectives
import utils.DataUtils.{DataAsExamples, DataAsIntervals, DataFunction, TrainingSet}
import utils.Implicits._
import utils.{ASP, CaviarUtils, Database, Utils}

import scala.util.Random

/**
  * Created by nkatz on 6/21/17.
  */

/**
*
* This object contains functionality used by the single-core version of OLED only.
*
* */

object SingleCoreOLEDFunctions extends CoreFunctions {





  def processExample(topTheory: Theory, e: Example, targetClass: String,
                     inps: RunningOptions, logger: org.slf4j.Logger) = {

    var newTopTheory = topTheory

    var scoringTime = 0.0
    var newRuleTestTime = 0.0
    var compressRulesTime = 0.0
    var expandRulesTime = 0.0
    var newRuleGenerationTime = 0.0

    val initorterm: String =
      if(targetClass=="initiated") "initiatedAt"
      else if (targetClass=="terminated") "terminatedAt"
      else inps.globals.MODEHS.head.varbed.tostring

    val withInertia = Globals.glvalues("with-inertia").toBoolean

    val startNew =
      if (withInertia) {

        /*-------------------------------------------------------------------*/
        // This works, but it takes too long. The reason is that it tries
        // to abduce at almost every example. See the comment above the isSat
        // method in SingleCoreOLEDFunctions for more details. See also the related
        // comment in Globals.scala.

        //if (e.annotation.isEmpty) false else ! isSat(e, inps.globals, this.jep)

        /*-------------------------------------------------------------------*/

        // growNewRuleTest here works with inertia in both the initiation and the
        // termination cases.
        if (e.annotation.isEmpty) false else newTopTheory.growNewRuleTest(e, initorterm, inps.globals)

      } else {
        //if (inps.tryMoreRules && targetClass == "terminated") true // Always try to find extra termination rules, they are more rare.
        if (inps.tryMoreRules) {
          //val r = scala.util.Random
          //val coinFlip = r.nextFloat
          //if (coinFlip >= 0.5) true else false
          true // Sometimes, depending on the order of the examples, it's necessary to try more even for initiation.
        }
        else {
          val r = Utils.time{ newTopTheory.growNewRuleTest(e, initorterm, inps.globals) }
          if (inps.showStats) logger.info(s"grow new rule test time: ${r._2}")
          newRuleTestTime += r._2
          r._1
        }
      }

    if (startNew) {
      val newRules_ = Utils.time {
        if (withInertia) {
          getnerateNewBottomClauses_withInertia(topTheory, e, initorterm, inps.globals)
        } else {
          if (inps.tryMoreRules) {
            // Don't use the current theory here to force the system to generate new rules
            generateNewRules(Theory(), e, initorterm, inps.globals)
          } else {
            generateNewRules(topTheory, e, initorterm, inps.globals)
          }
        }
      }

      val newRules__ = newRules_._1

      if (inps.showStats) logger.info(s"New rules generation time: ${newRules_._2}")
      newRuleGenerationTime += newRules_._2
      // Just to be on the safe side...
      val newRules = newRules__.filter(x => x.head.functor == initorterm)

      if (newRules.nonEmpty) logger.info(s"Generated ${newRules.length} new rules.")

      val o1 = System.nanoTime()
      if (inps.compressNewRules) {
        newTopTheory = topTheory.clauses ++ filterTriedRules(topTheory, newRules, logger)
      } else {
        newTopTheory = topTheory.clauses ++ newRules
      }
      val o2 = System.nanoTime()
      if (inps.showStats) logger.info(s"compressing rules time: ${(o2-o1)/1000000000.0}")
      compressRulesTime += (o2-o1)/1000000000.0

    }
    if (newTopTheory.clauses.nonEmpty) {
      val t = Utils.time { newTopTheory.scoreRules(e, inps.globals) }
      if (inps.showStats) logger.info(s"Scoring rules time: ${t._2}")
      scoringTime += t._2

      val expanded = Utils.time {  expandRules(newTopTheory, inps, logger) }
      if (inps.showStats) logger.info(s"Expanding rules time: ${expanded._2}")
      expandRulesTime += expanded._2

      if (inps.onlinePruning) {
        //val pruned = pruneRules(expanded._1, inps, logger)
        val pruned = pruneRulesNaive(expanded._1, inps, logger)
        (pruned, scoringTime, newRuleTestTime, compressRulesTime, expandRulesTime, newRuleGenerationTime)
      } else {
        (expanded._1, scoringTime, newRuleTestTime, compressRulesTime, expandRulesTime, newRuleGenerationTime)
      }
    } else {
      (newTopTheory, scoringTime, newRuleTestTime, compressRulesTime, expandRulesTime, newRuleGenerationTime)
    }
  }



  def rightWay(parentRule: Clause, inps: RunningOptions) = {
    val (observedDiff, best, secondBest) = parentRule.meanDiff

    val epsilon = Utils.hoeffding(inps.delta, parentRule.seenExmplsNum)

    //logger.info(s"\n(observedDiff, epsilon, bestScore, secondBestScore): ($observedDiff, $epsilon, ${best.score}, ${secondBest.score})")

    val passesTest = if (epsilon < observedDiff) true else false
    //val tie = if (epsilon <= breakTiesThreshold && parentRule.seenExmplsNum >= minSeenExmpls) true else false
    val tie = if (observedDiff < epsilon  && epsilon < inps.breakTiesThreshold && parentRule.seenExmplsNum >= inps.minSeenExmpls) true else false

    //println(s"best score: ${best.score} 2nd-best: ${secondBest.score} $observedDiff < $epsilon && $epsilon < ${inps.breakTiesThreshold} ${parentRule.seenExmplsNum} >= ${inps.minSeenExmpls} $tie")

    val couldExpand =
      if (inps.minTpsRequired != 0) {
        // The best.mlnWeight >= parentRule.mlnWeight condition doesn't work of course...
        (passesTest || tie) && (best.getTotalTPs >= parentRule.getTotalTPs * inps.minTpsRequired/100.0) //&& best.mlnWeight >= parentRule.mlnWeight
      } else {
        // The best.mlnWeight >= parentRule.mlnWeight condition doesn't work of course...
        passesTest || tie //&& best.mlnWeight >= parentRule.mlnWeight
      }

    (couldExpand, epsilon, observedDiff, best, secondBest)
  }

  def expandRules(topTheory: Theory, inps: RunningOptions, logger: org.slf4j.Logger): Theory = {
    //val t0 = System.nanoTime()
    val out = topTheory.clauses flatMap { parentRule =>
      val (couldExpand,epsilon,observedDiff,best,secondBest) = rightWay(parentRule, inps)

      //println(best.score,best.tps, best.fps, best.fns, "  ", secondBest.score, secondBest.tps, secondBest.fps, secondBest.fns)

      couldExpand match {
        case true =>
          // This is the extra test that I added at Feedzai
          val extraTest =
            if(secondBest != parentRule) (best.score > parentRule.score) && (best.score - parentRule.score > epsilon)
            else best.score > parentRule.score
          extraTest match { //&& (1.0/best.body.size+1 > 1.0/parentRule.body.size+1) match {
            case true =>
              val refinedRule = best
              logger.info(showInfo(parentRule, best, secondBest, epsilon, observedDiff, parentRule.seenExmplsNum, inps))
              refinedRule.seenExmplsNum = 0 // zero the counter
              refinedRule.supportSet = parentRule.supportSet // only one clause here
              List(refinedRule)
            case _ => List(parentRule)
          }
        case _ => List(parentRule)
      }
    }
    //val t1 = System.nanoTime()
    //println(s"expandRules time: ${(t1-t0)/1000000000.0}")
    Theory(out)
  }


  def processExampleNoEC(topTheory: Theory, e: Example, inps: RunningOptions, logger: org.slf4j.Logger) = {

    var scoringTime = 0.0
    var newRuleTestTime = 0.0
    var compressRulesTime = 0.0
    var expandRulesTime = 0.0
    var newRuleGenerationTime = 0.0

    var newTopTheory = topTheory
    val startNew = if (e.annotation.isEmpty) false else newTopTheory.growNewRuleTestNoEC(e, inps.globals)
    if (startNew) {
      val newRules_ = Utils.time{
        if (inps.tryMoreRules) {
          // Don't use the current theory here to force the system to generate new rules
          generateNewRulesNoEC(Theory(), e, inps.globals)
        } else {
          generateNewRulesNoEC(topTheory, e, inps.globals)
        }
      }
      val newRules__ = newRules_._1
      if (inps.showStats) logger.info(s"New rules generation time: ${newRules_._2}")
      newRuleGenerationTime += newRules_._2

      val newRules = newRules__

      if (newRules.nonEmpty) logger.info(s"Generated ${newRules.length} new rules.")

      val o1 = System.nanoTime()
      if (inps.compressNewRules) {
        newTopTheory = topTheory.clauses ++ filterTriedRules(topTheory, newRules, logger)
      } else {
        newTopTheory = topTheory.clauses ++ newRules
      }
      val o2 = System.nanoTime()
      if (inps.showStats) logger.info(s"compressing rules time: ${(o2-o1)/1000000000.0}")
      compressRulesTime += (o2-o1)/1000000000.0
    }
    if (newTopTheory.clauses.nonEmpty) {
      val t = Utils.time { newTopTheory.scoreRulesNoEC(e, inps.globals) }
      if (inps.showStats) logger.info(s"Scoring rules time: ${t._2}")
      scoringTime += t._2

      val expanded = Utils.time {  expandRules(newTopTheory, inps, logger) }
      if (inps.showStats) logger.info(s"Expanding rules time: ${expanded._2}")
      expandRulesTime += expanded._2

      if (inps.onlinePruning) {
        val pruned = pruneRules(expanded._1, inps, logger)
        (pruned, scoringTime, newRuleTestTime, compressRulesTime, expandRulesTime, newRuleGenerationTime)
      } else {
        (expanded._1, scoringTime, newRuleTestTime, compressRulesTime, expandRulesTime, newRuleGenerationTime)
      }
    } else {
      (newTopTheory, scoringTime, newRuleTestTime, compressRulesTime, expandRulesTime, newRuleGenerationTime)
    }
  }







  def getTrainingData(params: RunningOptions, data: TrainingSet, targetClass: String): Iterator[Example] = {

    val mc = MongoClient()
    val collection = mc(params.train)("examples")

    def getData = utils.CaviarUtils.getDataAsChunks(collection, params.chunkSize, targetClass)

    data match {
      case x: DataAsIntervals =>
        if (data.isEmpty) {
          getData
        } else {
          /* Optionally shuffle the training data */
          if (params.shuffleData) {
            val shuffled = List(data.asInstanceOf[DataAsIntervals].trainingSet.head) ++ Random.shuffle(data.asInstanceOf[DataAsIntervals].trainingSet.tail)
            CaviarUtils.getDataFromIntervals(collection, params.targetHLE, shuffled, params.chunkSize)
          } else {
            // No shuffling:
            CaviarUtils.getDataFromIntervals(collection, params.targetHLE, data.asInstanceOf[DataAsIntervals].trainingSet, params.chunkSize)
          }
        }
      case x: DataAsExamples => data.asInstanceOf[DataAsExamples].trainingSet.toIterator
      case x: DataFunction =>
        data.asInstanceOf[DataFunction].function(params.train, params.targetHLE, params.chunkSize, DataAsIntervals())
      case _ => throw new RuntimeException(s"${data.getClass}: Don't know what to do with this data container!")
    }
  }


  def reScore(params: RunningOptions, data: Iterator[Example], theory: Theory, targetClass: String, logger: org.slf4j.Logger) = {
    theory.clauses foreach (p => p.clearStatistics) // zero all counters before re-scoring
    for (x <- data) {
      if (Globals.glvalues("with-ec").toBoolean) {
        theory.scoreRules(x, params.globals, postPruningMode = true)
      } else {
        theory.scoreRulesNoEC(x, params.globals, postPruningMode = true)
      }

    }
    logger.debug( theory.clauses map { p => s"score: ${p.score}, tps: ${p.tps}, fps: ${p.fps}, fns: ${p.fns}\n${p.tostring}" } mkString "\n" )
  }

  def generateNewRules(topTheory: Theory, e: Example, initorterm: String, globals: Globals) = {
    val bcs = generateNewBottomClauses(topTheory, e, initorterm, globals)
    bcs map { x =>
      val c = Clause(head=x.head, body = List())
      c.addToSupport(x)
      c
    }
  }

  def generateNewRulesNoEC(topTheory: Theory, e: Example, globals: Globals) = {
    val bcs = generateNewBottomClausesNoEC(topTheory, e, globals)
    bcs map { x =>
      val c = Clause(head=x.head, body = List())
      c.addToSupport(x)
      c
    }
  }

  def reScoreAndPrune(inps: RunningOptions, data: Iterator[Example], finalTheory: Theory, targetClass: String, logger: org.slf4j.Logger) = {
    logger.info(s"Starting post-pruning for $targetClass")
    logger.info(s"Rescoring $targetClass theory")
    if (finalTheory != Theory()) reScore(inps, data, finalTheory, targetClass, logger)
    logger.info(s"\nLearned hypothesis (before pruning):\n${finalTheory.showWithStats}")
    val pruned = finalTheory.clauses.filter(x => x.score > inps.pruneThreshold && x.seenExmplsNum > inps.minEvalOn)
    logger.debug(s"\nPruned hypothesis:\n${pruned.showWithStats}")
    Theory(pruned)
  }

  /* Used by the monolithic OLED when learning with inertia.*/
  def check_SAT_withInertia(theory: Theory, example: Example, globals: Globals): Boolean = {
    val e = (example.annotationASP ++ example.narrativeASP).mkString("\n")
    val exConstr = getCoverageDirectives(withCWA = Globals.glvalues("cwa"), globals = globals).mkString("\n")
    val t = theory.map(x => x.withTypePreds(globals).tostring).mkString("\n")
    val f = Utils.getTempFile("sat",".lp")
    Utils.writeToFile(f, "append")(
      p => List(e,exConstr,t,s"\n#include "+"\""+globals.ABDUCE_WITH_INERTIA+"\".\n") foreach p.println
    )
    val inFile = f.getCanonicalPath
    val out = ASP.solve(Globals.CHECKSAT, Map(), new java.io.File(inFile), example.toMapASP)
    if (out != Nil && out.head == AnswerSet.UNSAT){
      false
    } else {
      true
    }
  }

  /* Used by the monolithic OLED when learning with inertia.
   * After processing each example, form a joint current theory by
   * putting together the best specialization of each existing rule so far.
   * If you just use each top rule, chances are that over-general rules will
   * screw things up.*/
  def updateGlobalTheoryStore(theory: Theory, target: String, gl: Globals) = {

    def getBestClause(c: Clause) = {
      val allSorted = (List(c) ++ c.refinements).sortBy { x => (- x.score, x.body.length+1) }
      val best = allSorted.take(1)
      best.head
    }

    def getBestClauses(T: Theory) = {
      T.clauses.map(x => getBestClause(x))
    }

    if (target == "initiatedAt") {
      Globals.CURRENT_THEORY_INITIATED = getBestClauses(theory).toVector
    } else if (target == "terminatedAt") {
      Globals.CURRENT_THEORY_TERMINATED = getBestClauses(theory).toVector
    } else {
      throw new RuntimeException(s"Unknown target predicate: $target")
    }
  }

  /* Used by the monolithic OLED when learning with inertia.*/
  // The problem with deciding when to learn a new clause with
  // isSat here is that almost always, the decision will be yes!
  // That's because, even if we form the current theory in isSat
  // by selecting the best specialization from each existing clause.
  // chances are that there will be imperfect rules, which are not
  // specialized quickly enough, so we end-up with an unsatisfiable program.
  // We'd need something more in the style of ILED for this. Reacting fast
  // to every mistake, specializing immediately, so in the absence of noise, we quickly
  // learn the correct definitions. (Note that in any case, if there is noise
  // in the strongly-initiated setting, there is not chance to learn anything.
  // See also the related comment in Globals.scala
  def isSat(example: Example, globals: Globals) = {
    val jointTheory = Theory((Globals.CURRENT_THEORY_INITIATED ++ Globals.CURRENT_THEORY_TERMINATED).toList)
    check_SAT_withInertia(jointTheory, example, globals)
  }

}

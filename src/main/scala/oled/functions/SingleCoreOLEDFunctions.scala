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

  def getTrainingData(params: RunningOptions, data: TrainingSet, targetClass: String): Iterator[Example] = {

    val mc = MongoClient()
    val collection = mc(params.db)("examples")

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
        data.asInstanceOf[DataFunction].function(params.db, params.targetHLE, params.chunkSize, DataAsIntervals())
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

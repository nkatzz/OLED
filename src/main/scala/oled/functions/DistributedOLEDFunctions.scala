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
import logic.Examples.Example
import logic.{Clause, Literal, Theory}
import oled.distributed.Structures.ClauseStats

/**
  * Created by nkatz on 6/21/17.
  */

/**
  *
  * This object contains functionality used by the distributed version of OLED only.
  *
  * */

object DistributedOLEDFunctions extends CoreFunctions {

  def generateNewRules(topTheory: Theory, e: Example, initorterm: String, globals: Globals, otherNodeNames: List[String]) = {
    val bcs_ = generateNewBottomClauses(topTheory, e, initorterm, globals)
    val bcs = bcs_.filter(p => p.head.functor.contains(initorterm))
    bcs map { x =>
      val c = Clause(head=x.head, body = List())
      c.addToSupport(x)
      otherNodeNames.foreach{ node =>
        c.countsPerNode(node) = new ClauseStats(0, 0, 0, 0)
      }
      // In the distributed setting, refinements must be generated right after the construction of a clause,
      // in order to copy them in the clause copies that will be sent to other nodes (ensure same uuid's etc.)
      c.generateCandidateRefs(globals)
      c
    }
  }

  def showInfo(c: Clause, c1: Clause, c2: Clause, hoeffding: Double, observedDiff: Double, n: Int, showRefs: Boolean, onNode: String) = {

    val score = (clause: Clause) => clause.distScore

    if (showRefs) {
      s"\n===========================================================\n" +
        s"\nClause (score: ${score(c)} | ${c.showCountsPerNode(onNode)}\n\n${c.tostring}\n\nwas refined to" +
        s" (new score: ${score(c1)} | ${c1.showCountsPerNode(onNode)}\n\n${c1.tostring}\n\nε: $hoeffding, ΔG: $observedDiff, examples used: $n" +
        s"\nall refs (total tp/fp/fn counts):\n\n${c.refinements.sortBy(z => (-score(z), z.body.length+1)).map(x => x.tostring+" | " +
          "score "+score(x)+x.showCountsPerNode(onNode)).mkString("\n")}" +
        s"\n===========================================================\n"
    } else {
      s"\n===========================================================\n" +
        s"\nClause (score: ${score(c)} | ${c.showCountsPerNode(onNode)}\n\n${c.tostring}\n\nwas refined to" +
        s" (new score: ${score(c1)} | ${c1.showCountsPerNode(onNode)}\n\n${c1.tostring}\n\nε: $hoeffding, ΔG: $observedDiff, examples used: $n" +
        //s"\nall refs (total tp/fp/fn counts):\n\n${c.refinements.sortBy(z => (-score(z), z.body.length+1)).map(x => x.tostring+" | " +
        //  "score "+score(x)+x.showCountsPerNode(onNode)).mkString("\n")}" +
        s"\n===========================================================\n"
    }
  }





  /*
  * The rest is for clause expansion in the distributed setting. This should be refactored,
  * its almost the same with functions used in the monolithic setting.
  * */

  private def score(clause: Clause) = clause.distScore

  def rightWay(parentRule: Clause, delta: Double, breakTiesThreshold: Double, minSeenExmpls: Int, minTpsRequired: Int = 0) = {
    val (observedDiff,best,secondBest) = parentRule.distributedMeanDiff
    val epsilon = utils.Utils.hoeffding(delta, parentRule.getTotalSeenExmpls)
    val passesTest = if (epsilon < observedDiff) true else false
    val tie = if (observedDiff < epsilon  && epsilon < breakTiesThreshold && parentRule.getTotalSeenExmpls >= minSeenExmpls) true else false
    val couldExpand = if (minTpsRequired != 0) (passesTest || tie) && best.getTotalTPs > minTpsRequired else passesTest || tie
    (couldExpand,epsilon,observedDiff,best,secondBest)
  }

  def expandRule(parentRule: Clause, delta: Double, breakTiesThreshold: Double,
                 minSeenExmpls: Int, nodeName: String, params: RunningOptions, logger: org.slf4j.Logger) = {

    val minTpsRequired = params.minTpsRequired
    val (couldExpand,epsilon,observedDiff,best,secondBest) = rightWay(parentRule, delta, breakTiesThreshold, minSeenExmpls, minTpsRequired)
    if (couldExpand) {
      // This is the extra test that I added at Feedzai
      val extraTest =
        if(secondBest != parentRule) (score(best) > score(parentRule)) && (score(best) - score(parentRule) > epsilon)
        else score(best) > score(parentRule)
      if (extraTest) {
        val refinedRule = best
        logger.info(DistributedOLEDFunctions.showInfo(parentRule, best, secondBest, epsilon, observedDiff, parentRule.seenExmplsNum, params.showRefs, nodeName))
        refinedRule.seenExmplsNum = 0 // zero the counter
        refinedRule.totalSeenExmpls = 0 // zero the counter
        refinedRule.supportSet = parentRule.supportSet // only one clause here
        // In the distributed setting, refinements must be generated right after the construction of a clause,
        // in order to copy them in the clause copies that will be sent to other nodes (ensure same uuid's etc.)
        refinedRule.generateCandidateRefs(params.globals)
        (true, refinedRule)
      } else {
        logger.info(s"Hoeffding test failed (clause ${parentRule.uuid}) not expanded")
        (false, parentRule)
      }
    } else {
      logger.info(s"Hoeffding test failed (clause ${parentRule.uuid}) not expanded")
      (false, parentRule)
    }
  }

  def shouldExpand(parentRule: Clause, delta: Double, breakTiesThreshold: Double, minSeenExmpls: Int) = {
    val (couldExpand,epsilon,observedDiff,best,secondBest) = rightWay(parentRule, delta, breakTiesThreshold, minSeenExmpls)
    if (couldExpand) {
      val extraTest =
        if(secondBest != parentRule) (score(best) > score(parentRule)) && (score(best) - score(parentRule) > epsilon)
        else score(best) > score(parentRule)
      if (extraTest) {
        true
      } else {
        false
      }
    } else {
      false
    }
  }












}

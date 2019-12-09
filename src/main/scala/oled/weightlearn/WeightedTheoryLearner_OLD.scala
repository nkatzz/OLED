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

package oled.weightlearn

import app.runutils.IOHandling.Source
import app.runutils.RunningOptions
import logic.Examples.Example
import logic.{Clause, Literal, Theory}
import oled.functions.SingleCoreOLEDFunctions.reScoreAndPrune
import oled.single_core.TheoryLearner
import org.slf4j.LoggerFactory
import utils.Utils
import oled.functions.SingleCoreOLEDFunctions._
import utils.Implicits._
import oled.functions.WeightLearningFunctions._


class WeightedTheoryLearner_OLD[T <: Source](inps: RunningOptions, trainingDataOptions: T, testingDataOptions: T,
                                         trainingDataFunction: T => Iterator[Example],
                                         testingDataFunction: T => Iterator[Example],
                                         targetClass: String) extends
  TheoryLearner(inps, trainingDataOptions, testingDataOptions, trainingDataFunction, testingDataFunction, targetClass) {

  private val logger = LoggerFactory.getLogger(self.path.name)

  override def run: (Theory, Double) = {

    def runOnce(inTheory: Theory): Theory = {
      val trainingData = trainingDataFunction(trainingDataOptions)
      if (trainingData.isEmpty) {
        logger.error(s"DB ${inps.train} is empty.")
        System.exit(-1)
      }

      var step = 1 // for cda

      trainingData.foldLeft(inTheory){ (topTheory, newExample) =>

        println(newExample.time)

        if (newExample.time == "5047") {
          val stop = "stop"
        }

        val res = Utils.time {
          processExample_1(topTheory, newExample, step)
          //processExample(topTheory, newExample, step)
        }

        step += 1

        println(s"Process time: ${res._2}")

        res._1

      }
    }

    logger.info(s"Starting learning for $targetClass")

    val allClauses = List( Clause.parse("initiatedAt(meeting(X0,X1),X2) :- happensAt(active(X0),X2),happensAt(active(X1),X2),close(X0,X1,25,X2)") )
    //val allClauses = List( Clause.parse("terminatedAt(meeting(X0,X1),X2) :- happensAt(disappear(X1),X2)") )
    //val allClauses = List( Clause.parse("terminatedAt(meeting(X0,X1),X2) :- happensAt(active(X0),X2),happensAt(active(X1),X2),close(X0,X1,24,X2)") )
    val inTheory = Theory(allClauses)
    val _finalTheory = Utils.time{ (1 to inps.repeatFor).foldLeft(inTheory)( (t,_) =>  runOnce(t)) }

    //val _finalTheory = Utils.time{ (1 to inps.repeatFor).foldLeft(Theory())( (t,_) =>  runOnce(t)) }






    val (finalTheory,time) = (_finalTheory._1,_finalTheory._2)
    logger.info(s"\nTraining time for $targetClass: $time")

    /* We'll have to see if all that stuff with post-pruning will be ported in the MLN-based version*/
    val output = inps.withPostPruning match {
      case true =>
        val data = trainingDataFunction(trainingDataOptions)
        reScoreAndPrune(inps, data, finalTheory, initorterm, logger)
      case _ =>
        // no re-scoring
        val pruned = finalTheory.clauses.filter(x => x.score > inps.pruneThreshold && x.seenExmplsNum > inps.minEvalOn )
        logger.info(s"\nLearnt hypothesis (non-pruned):\n${finalTheory.showWithStats}")
        Theory(pruned)
    }

    logger.debug(s"\n$targetClass theory found:\n${output.tostring}")
    (output,time)
  }





  def processExample(topTheory: Theory, e: Example, step: Int): Theory = {

    var newTopTheory = topTheory

    val t0 = System.nanoTime()

    // WE NEED TO HANDLE THE startNew VARIABLE AS WELL...
    ///*
    val startNew =
    if (this.inps.tryMoreRules && this.targetClass == "terminated") true
    else newTopTheory.growNewRuleTest(e, initorterm, inps.globals)
    //*/

    //val startNew = true

    if (startNew) {
      val newRules_ = if (this.inps.tryMoreRules) {
        // Don't use the current theory here to force the system to generate new rules
        generateNewRules(Theory(), e, this.initorterm, inps.globals)
      } else {
        generateNewRules(topTheory, e, this.initorterm, inps.globals)
      }
      // Just to be on the safe side...
      val newRules = newRules_.toVector.filter(x => x.head.functor == this.initorterm)

      if (newRules.nonEmpty) logger.info(s"Generated ${newRules.length} new rules.")

      if (this.inps.compressNewRules) {
        newTopTheory = topTheory.clauses ++ filterTriedRules(topTheory, newRules, logger)
      } else {
        newTopTheory = topTheory.clauses ++ newRules
      }
    }

    val t1 = System.nanoTime()
    println(s"New rule stuff (ASP) time: ${(t1-t0)/1000000000.0}")

    if (newTopTheory.clauses.nonEmpty) {

      val generate_refs_timed = Utils.time {
        newTopTheory.clauses.toVector foreach (rule => if (rule.refinements.isEmpty) rule.generateCandidateRefs(inps.globals))
      }
      println(s"Generate refinements time: ${generate_refs_timed._2}")

      // There's an issue with inference with empty-bodied rules (see the big comment at
      // ASP2MLN.parseRules for more details, so for now we do not give these rules to the MLN side).

      val allClauses = newTopTheory.clauses.flatMap { x =>
        if (x.body.nonEmpty) List(x) ++ x.refinements
        else x.refinements
      }

      val t4 = System.nanoTime()

      val enumClauses = (1 to allClauses.length).toList

      val uuidsToRuleIdsMap = (allClauses.map(_.uuid) zip enumClauses) toMap

      val mlnClauses = (enumClauses zip allClauses toMap) map { case (k, v) => v.tostring_MLN(k) }

      val t5 = System.nanoTime()
      println(s"Preparation for MLN time: ${(t5-t4)/1000000000.0}")

      // The types, annotation template etc need to be automated.
      val build_mln_timed = Utils.time{

        val targetTemplate =
          if(this.initorterm == "initiatedAt") "InitiatedAt(fluent,time,ruleId)"
          else "TerminatedAt(fluent,time,ruleId)"
        buildMLN(List("time", "person", "dist", "event", "event1", "fluent", "ruleId"), targetTemplate, mlnClauses, e, enumClauses, inps)
      }
      val mlnInfo = build_mln_timed._1
      val (mln, annotationDB, exmplCount) = (mlnInfo.mln, mlnInfo.annotationDB, mlnInfo.exmplCount)
      //println(s"Building MLN time: ${build_mln_timed._2} sec")

      val learn_weights_timed = Utils.time { learnWeights(mln, annotationDB, step, postLearning=true) }
      val mrf = learn_weights_timed._1
      println(s"AdaGrad time: ${learn_weights_timed._2} sec")

      //-----------------------------------------------
      // Debug:
      /*
      val debug = {


        val m = getMRFAsStringSet(mrf).map(x => x.toLowerCase()).toArray

        val k = m map { x =>
          if (x.contains("!")) {
            val y = x.split("!")(1)
            val y1 = Literal.parseWPB2("not "+y)
            Literal.toMLNFlat(y1)
          } else {
            val y1 = Literal.parseWPB2(x)
            Literal.toMLNFlat(y1)
          }
        }

        k.sortBy(x => (x.terms(2).name.split("_")(1).toInt,
          x.terms(1).name.toInt, x.terms.head.name.split("_")(1), x.terms.head.name.split("_")(1)) ).map(z => z.tostring_mln).mkString("\n")

      }
      */
      //-----------------------------------------------

      val perform_inference_timed = Utils.time { performInference(mrf) }
      val inferredAtoms = perform_inference_timed._1
      println(s"Inference time: ${perform_inference_timed._2} sec")

      val get_stats_timed = Utils.time{ getStatistics(mlnInfo, inferredAtoms) }
      val statsperClause = get_stats_timed._1
      println(s"Get statistics per clause time: ${get_stats_timed._2} sec")

      // Tide this up.
      def updateClauseStats(clause: Clause, uuidsToRuleIdsMap: Map[String, Int],
                            statsperClause: Map[Int, (Double, Double, Int, Int, Int)]) = {
        val id = uuidsToRuleIdsMap(clause.uuid)
        val stats = statsperClause(id)
        clause.weight = stats._1
        clause.subGradient = stats._2
        clause.tps = clause.tps + stats._3
        clause.fps = clause.fps + stats._4
        clause.fns = clause.fns + stats._5
      }

      val t = Utils.time {
        newTopTheory.clauses foreach { clause =>
          if (clause.body.nonEmpty) updateClauseStats(clause, uuidsToRuleIdsMap, statsperClause)
          clause.refinements.toVector.foreach(updateClauseStats(_, uuidsToRuleIdsMap, statsperClause))
          clause.seenExmplsNum += exmplCount
          clause.refinements.toVector.foreach(y => y.seenExmplsNum += exmplCount)
          clause.supportSet.clauses.toVector.foreach(y => y.seenExmplsNum += exmplCount)
        }
      }
      println(s"Updating all clauses state time: ${t._2} sec")

      //newTopTheory.scoreRules(e, this.jep, inps.globals) // Already taken care of...
      val expanded = expandRules(newTopTheory, inps, logger)
      if (inps.onlinePruning) {
        pruneRules(expanded._1, inps, logger)
      } else {
        expanded._1
      }
    } else {
      newTopTheory
    }

  }







  /* For dubugging, run with a single hard-coded rule*/


  ///*

  def processExample_1(topTheory: Theory, e: Example, step: Int): Theory = {

    val allClauses = topTheory.clauses

    println(s"MLN WEIGHT: ${allClauses.head.weight}")

    val enumClauses = (1 to allClauses.length).toList

    val uuidsToRuleIdsMap = (allClauses.map(_.uuid) zip enumClauses) toMap

    val mlnClauses = (enumClauses zip allClauses toMap) map { case (k, v) => v.tostring_MLN(k) }

    // The types, annotation template etc need to be automated.
    val build_mln_timed = Utils.time{
      buildMLN(List("time", "person", "dist", "event", "event1", "fluent", "ruleId"),
        "InitiatedAt(fluent,time,ruleId)", mlnClauses, e, enumClauses, inps)
    }
    val mlnInfo = build_mln_timed._1
    val (mln, annotationDB, exmplCount) = (mlnInfo.mln, mlnInfo.annotationDB, mlnInfo.exmplCount)
    //println(s"Building MLN time: ${build_mln_timed._2} sec")

    val learn_weights_timed = Utils.time { learnWeights(mln, annotationDB, step) }
    val mrf = learn_weights_timed._1
    println(s"AdaGrad time: ${learn_weights_timed._2} sec")

    val perform_inference_timed = Utils.time { performInference(mrf) }
    val inferredAtoms = perform_inference_timed._1
    println(s"Inference time: ${perform_inference_timed._2} sec")


    val get_stats_timed = Utils.time{ getStatistics(mlnInfo, inferredAtoms) }
    val statsperClause = get_stats_timed._1
    println(s"Get statistics per clause time: ${get_stats_timed._2} sec")

    // Tide this up.
    def updateClauseStats(clause: Clause, uuidsToRuleIdsMap: Map[String, Int],
                          statsperClause: Map[Int, (Double, Double, Int, Int, Int)]) = {

      val id = uuidsToRuleIdsMap(clause.uuid)
      val stats = statsperClause(id)
      clause.weight = stats._1
      clause.subGradient = stats._2
      clause.tps = clause.tps + stats._3
      clause.fps = clause.fps + stats._4
      clause.fns = clause.fns + stats._5
    }

    val t = Utils.time {
      topTheory.clauses foreach { clause =>
        if (clause.body.nonEmpty) updateClauseStats(clause, uuidsToRuleIdsMap, statsperClause)
        //clause.refinements.toVector.foreach(updateClauseStats(_, uuidsToRuleIdsMap, statsperClause))
        //clause.seenExmplsNum += exmplCount
        //clause.refinements.toVector.foreach(y => y.seenExmplsNum += exmplCount)
        //clause.supportSet.clauses.toVector.foreach(y => y.seenExmplsNum += exmplCount)
      }
    }

    topTheory
  }

  //*/





}
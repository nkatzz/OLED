package oled.single_core

import akka.actor.Actor
import app.runutils.IOHandling.{MongoSource, Source}
import app.runutils.RunningOptions
import jep.Jep
import logic.Examples.Example
import logic.{Clause, LogicUtils, Theory}
import org.slf4j.LoggerFactory
import utils.DataUtils.TrainingSet
import utils.Implicits._
import utils.{Database, Utils}
import oled.functions.SingleCoreOLEDFunctions._

/**
  * Created by nkatz on 27/2/2016.
  *
  */



class TheoryLearner[T <: Source](inps: RunningOptions,
                                 trainingDataOptions: T,
                                 testingDataOptions: T,
                                 trainingDataFunction: T => Iterator[Example],
                                 testingDataFunction: T => Iterator[Example],
                                 targetClass: String) extends Actor {

  private val logger = LoggerFactory.getLogger(self.path.name)

  def receive = {
    case "go" => sender ! run
  }

  val jep = new Jep()

  val initorterm: String = if(targetClass=="initiated") "initiatedAt" else "terminatedAt"

  def run: (Theory,Double) = {

    def runOnce(inTheory: Theory): Theory = {
      val trainingData = trainingDataFunction(trainingDataOptions)
      if (trainingData.isEmpty) {
        logger.error(s"DB ${inps.db} is empty.")
        System.exit(-1)
      }
      trainingData.foldLeft(inTheory){ (topTheory, newExample) =>
        //println(newExample.time, topTheory.clauses.length, s"${topTheory.clauses.map(x => (x.tps, x.fps, x.fns, x.score)).mkString(" ")}")
        println(newExample.time)

        val res = Utils.time {
          processExample(topTheory, newExample)
        }
        println(res._2)
        res._1
      }
    }

    logger.info(s"Starting learning for $targetClass")
    val _finalTheory = Utils.time{ (1 to inps.repeatFor).foldLeft(Theory())( (t,_) =>  runOnce(t)) }
    val (finalTheory,time) = (_finalTheory._1,_finalTheory._2)
    logger.info(s"\nTraining time for $targetClass: $time")
    val output = inps.withPostPruning match {
      case true =>
        val data = trainingDataFunction(trainingDataOptions)
        reScoreAndPrune(inps, data, finalTheory, initorterm, jep, logger)
      case _ =>
        // no re-scoring
        val pruned = finalTheory.clauses.filter(x => x.score > inps.pruneThreshold && x.seenExmplsNum > inps.minEvalOn )
        logger.info(s"\nLearnt hypothesis (non-pruned):\n${finalTheory.showWithStats}")
        Theory(pruned)
    }
    logger.debug(s"\n$targetClass theory found:\n${output.tostring}")
    this.jep.close()
    (output,time)
  }


  def processExample(topTheory: Theory, e: Example): Theory = {
    var newTopTheory = topTheory
    val startNew = if (this.inps.tryMoreRules) true else newTopTheory.growNewRuleTest(e, this.jep, initorterm, inps.globals)
    if (startNew) {
      val newRules_ = if (this.inps.tryMoreRules) {
        // Don't use the current theory here to force the system to generate new rules
        generateNewRules(Theory(), e, this.jep, this.initorterm, inps.globals)
      } else {
        generateNewRules(topTheory, e, this.jep, this.initorterm, inps.globals)
      }
      // Just to be on the safe side...
      val newRules = newRules_.filter(x => x.head.functor == this.initorterm)

      if (this.inps.compressNewRules) {
        newTopTheory = topTheory.clauses ++ filterTriedRules(topTheory, newRules, logger)
      } else {
        newTopTheory = topTheory.clauses ++ newRules
      }
    }
    if (newTopTheory.clauses.nonEmpty) {
      //val t0 = System.nanoTime()
      newTopTheory.scoreRules(e, this.jep, inps.globals)
      //val t1 = System.nanoTime()
      //println(s"scoreRules time: ${(t1-t0)/1000000000.0}")
      val expanded = expandRules(newTopTheory)
      if (inps.onlinePruning) {
        pruneRules(expanded, inps, logger)
      } else {
        expanded
      }
    } else {
      newTopTheory
    }
  }


  def rightWay(parentRule: Clause) = {
    val (observedDiff,best,secondBest) = parentRule.meanDiff
    val epsilon = Utils.hoeffding(inps.delta, parentRule.seenExmplsNum)
    val passesTest = if (epsilon < observedDiff) true else false
    //val tie = if (epsilon <= breakTiesThreshold && parentRule.seenExmplsNum >= minSeenExmpls) true else false
    val tie = if (observedDiff < epsilon  && epsilon < inps.breakTiesThreshold && parentRule.seenExmplsNum >= inps.minSeenExmpls) true else false

    val couldExpand = passesTest || tie
    (couldExpand,epsilon,observedDiff,best,secondBest)
  }

  def expandRules(topTheory: Theory): Theory = {
    //val t0 = System.nanoTime()
    val out = topTheory.clauses flatMap { parentRule =>
      val (couldExpand,epsilon,observedDiff,best,secondBest) = rightWay(parentRule)
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
              logger.info(showInfo(parentRule, best, secondBest, epsilon, observedDiff, parentRule.seenExmplsNum, this.inps.showRefs))
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












}

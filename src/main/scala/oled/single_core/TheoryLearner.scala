package oled.single_core

import akka.actor.Actor
import app.runutils.IOHandling.{MongoSource, Source}
import app.runutils.{Globals, RunningOptions}
import com.mongodb.casbah.MongoClient
import logic.Examples.Example
import logic.{Clause, LogicUtils, Theory}
import org.slf4j.LoggerFactory
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
  private var totalBatchProcessingTime = 0.0
  private var totalRuleScoringTime = 0.0
  private var totalNewRuleTestTime = 0.0
  private var totalCompressRulesTime = 0.0
  private var totalExpandRulesTime = 0.0
  private var totalNewRuleGenerationTime = 0.0


  def receive = {
    case "go" => sender ! run
  }

  val initorterm: String = if(targetClass=="initiated") "initiatedAt" else "terminatedAt"

  private val withInertia = Globals.glvalues("with-inertia").toBoolean

  def run: (Theory,Double) = {

    def runOnce(inTheory: Theory): Theory = {
      val trainingData = trainingDataFunction(trainingDataOptions)
      if (trainingData.isEmpty) {
        logger.error(s"DB ${inps.db} is empty.")
        System.exit(-1)
      }
      trainingData.foldLeft(inTheory){ (topTheory, newExample) =>

        if (inps.showStats) println(newExample.time)

        val res = Utils.time {
          val t = processExample(topTheory, newExample)

          // This is used only when learning with inertia. But I think
          // its ok to keep it so that I can print out stats for the current
          // joint theory (for debugging).
          //if (withInertia) updateGlobalTheoryStore(t, initorterm, inps.globals)
          updateGlobalTheoryStore(t, initorterm, inps.globals)

          t
        }
        if (inps.showStats) logger.info(s"Total batch process time: ${res._2}")
        this.totalBatchProcessingTime += res._2
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
        reScoreAndPrune(inps, data, finalTheory, initorterm, logger)
      case _ =>
        // no re-scoring
        val pruned = finalTheory.clauses.filter(x => x.score > inps.pruneThreshold && x.seenExmplsNum > inps.minEvalOn )
        logger.info(s"\nLearnt hypothesis (non-pruned):\n${finalTheory.showWithStats}")
        Theory(pruned)
    }
    logger.debug(s"\n$targetClass theory found:\n${output.tostring}")
    logger.info(s"Total batch processing time: $totalBatchProcessingTime")
    logger.info(s"Total rule scoring time: $totalRuleScoringTime")
    logger.info(s"Total rule expansion time: $totalExpandRulesTime")
    logger.info(s"Total rule compression time: $totalCompressRulesTime")
    logger.info(s"Total testing for new rule generation time: $totalNewRuleTestTime")
    logger.info(s"Total new rule generation  time: $totalNewRuleGenerationTime")
    (output,time)
  }


  def processExample(topTheory: Theory, e: Example): Theory = {

    var newTopTheory = topTheory

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
        if (this.inps.tryMoreRules && this.targetClass == "terminated") true
        else {
          val r = Utils.time{ newTopTheory.growNewRuleTest(e, initorterm, inps.globals) }
          if (inps.showStats) logger.info(s"grow new rule test time: ${r._2}")
          this.totalNewRuleTestTime += r._2
          r._1
        }
      }

    if (startNew) {
      val newRules_ = Utils.time {
        if (withInertia) {
          getnerateNewBottomClauses_withInertia(topTheory, e, this.initorterm, inps.globals)
        } else {
          if (this.inps.tryMoreRules) {
            // Don't use the current theory here to force the system to generate new rules
            generateNewRules(Theory(), e, this.initorterm, inps.globals)
          } else {
            generateNewRules(topTheory, e, this.initorterm, inps.globals)
          }
        }
      }

      val newRules__ = newRules_._1

      if (inps.showStats) logger.info(s"New rules genration time: ${newRules_._2}")
      this.totalNewRuleGenerationTime += newRules_._2
      // Just to be on the safe side...
      val newRules = newRules__.filter(x => x.head.functor == this.initorterm)

      if (newRules.nonEmpty) logger.info(s"Generated ${newRules.length} new rules.")

      val o1 = System.nanoTime()
      if (this.inps.compressNewRules) {
        newTopTheory = topTheory.clauses ++ filterTriedRules(topTheory, newRules, logger)
      } else {
        newTopTheory = topTheory.clauses ++ newRules
      }
      val o2 = System.nanoTime()
      if (inps.showStats) logger.info(s"compressing rules time: ${(o2-o1)/1000000000.0}")
      this.totalCompressRulesTime += (o2-o1)/1000000000.0

    }
    if (newTopTheory.clauses.nonEmpty) {
      val t = Utils.time { newTopTheory.scoreRules(e, inps.globals) }
      if (inps.showStats) logger.info(s"Scoring rules time: ${t._2}")
      this.totalRuleScoringTime += t._2

      val expanded = Utils.time {  expandRules(newTopTheory) }
      if (inps.showStats) logger.info(s"Expanding rules time: ${expanded._2}")
      this.totalExpandRulesTime += expanded._2

      if (inps.onlinePruning) {
        pruneRules(expanded._1, inps, logger)
      } else {
        expanded._1
      }
    } else {
      newTopTheory
    }
  }


  def rightWay(parentRule: Clause) = {
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
              logger.info(showInfo(parentRule, best, secondBest, epsilon, observedDiff,
                parentRule.seenExmplsNum, this.inps))
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

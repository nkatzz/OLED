package oled.weightlearn

import app.runutils.IOHandling.Source
import app.runutils.RunningOptions
import logic.Examples.Example
import logic.Theory
import oled.functions.SingleCoreOLEDFunctions.reScoreAndPrune
import oled.single_core.TheoryLearner
import org.slf4j.LoggerFactory
import utils.Utils
import oled.functions.SingleCoreOLEDFunctions._
import utils.Implicits._


class WeightedTheoryLearner[T <: Source](inps: RunningOptions, trainingDataOptions: T, testingDataOptions: T,
                                         trainingDataFunction: T => Iterator[Example],
                                         testingDataFunction: T => Iterator[Example],
                                         targetClass: String) extends
  TheoryLearner(inps, trainingDataOptions, testingDataOptions, trainingDataFunction, testingDataFunction, targetClass) {

  private val logger = LoggerFactory.getLogger(self.path.name)

  override def run: (Theory,Double) = {

    def runOnce(inTheory: Theory): Theory = {
      val trainingData = trainingDataFunction(trainingDataOptions)
      if (trainingData.isEmpty) {
        logger.error(s"DB ${inps.db} is empty.")
        System.exit(-1)
      }
      trainingData.foldLeft(inTheory){ (topTheory, newExample) =>
        //println(newExample.time)
        val res = Utils.time {
          processExample(topTheory, newExample)
        }
        res._1
      }
    }

    logger.info(s"Starting learning for $targetClass")
    val _finalTheory = Utils.time{ (1 to inps.repeatFor).foldLeft(Theory())( (t,_) =>  runOnce(t)) }
    val (finalTheory,time) = (_finalTheory._1,_finalTheory._2)
    logger.info(s"\nTraining time for $targetClass: $time")

    /* We'll have to see if all that stuff with post-pruning will be ported in the MLN-based version*/
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




  override def processExample(topTheory: Theory, e: Example): Theory = {

    var newTopTheory = topTheory

    val startNew =
      if (this.inps.tryMoreRules && this.targetClass == "terminated") true
      else newTopTheory.growNewRuleTest(e, this.jep, initorterm, inps.globals)

    if (startNew) {
      val newRules_ = if (this.inps.tryMoreRules) {
        // Don't use the current theory here to force the system to generate new rules
        generateNewRules(Theory(), e, this.jep, this.initorterm, inps.globals)
      } else {
        generateNewRules(topTheory, e, this.jep, this.initorterm, inps.globals)
      }
      // Just to be on the safe side...
      val newRules = newRules_.filter(x => x.head.functor == this.initorterm)

      if (newRules.nonEmpty) logger.info(s"Generated ${newRules.length} new rules.")

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







}

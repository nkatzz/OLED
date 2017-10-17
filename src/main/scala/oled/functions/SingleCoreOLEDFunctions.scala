package oled.functions

import app.runutils.{Globals, RunningOptions}
import com.mongodb.casbah.MongoClient
import jep.Jep
import logic.Examples.Example
import logic.{Clause, Theory}
import utils.DataUtils.{DataAsExamples, DataAsIntervals, DataFunction, TrainingSet}
import utils.Implicits._
import utils.{CaviarUtils, Database}

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


  def reScore(params: RunningOptions, data: Iterator[Example], theory: Theory, targetClass: String, jep: Jep, logger: org.slf4j.Logger) = {
    theory.clauses foreach (p => p.clearStatistics) // zero all counters before re-scoring
    for (x <- data) {
      theory.scoreRules(x, jep, params.globals, postPruningMode = true)
    }
    logger.debug( theory.clauses map { p => s"score: ${p.score}, tps: ${p.tps}, fps: ${p.fps}, fns: ${p.fns}\n${p.tostring}" } mkString "\n" )
  }

  def generateNewRules(topTheory: Theory, e: Example, jep: Jep, initorterm: String, globals: Globals) = {
    val bcs = generateNewBottomClauses(topTheory, e, jep, initorterm, globals)
    bcs map { x =>
      val c = Clause(head=x.head, body = List())
      c.addToSupport(x)
      c
    }
  }

  def reScoreAndPrune(inps: RunningOptions, data: Iterator[Example], finalTheory: Theory, targetClass: String, jep: Jep, logger: org.slf4j.Logger) = {
    logger.info(s"Starting post-pruning for $targetClass")
    logger.info(s"Rescoring $targetClass theory")
    if (finalTheory != Theory()) reScore(inps, data, finalTheory, targetClass, jep, logger)
    logger.info(s"\nLearned hypothesis (before pruning):\n${finalTheory.showWithStats}")
    val pruned = finalTheory.clauses.filter(x => x.score > inps.pruneThreshold && x.seenExmplsNum > inps.minEvalOn)
    logger.debug(s"\nPruned hypothesis:\n${pruned.showWithStats}")
    Theory(pruned)
  }

}

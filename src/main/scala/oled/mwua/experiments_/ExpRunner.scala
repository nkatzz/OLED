package oled.mwua.experiments_

import akka.actor.{ActorSystem, Props}
import app.runutils.{CMDArgs, Globals}
import com.typesafe.scalalogging.LazyLogging
import experiments.caviar.FullDatasetHoldOut.MongoDataOptions
import experiments.caviar.{FullDatasetHoldOut, MeetingTrainTestSets}
import logic.Examples.Example
import oled.mwua.Learner

/**
  * Created by nkatz at 2/11/2018
  */


/* NOT USED IN ANYTHING YET!! */

object ExpRunner extends LazyLogging {


  def main(args: Array[String]) = {

    val thresholds = List(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9)

    val dataSets = Vector(MeetingTrainTestSets.meeting1, MeetingTrainTestSets.meeting2,
      MeetingTrainTestSets.meeting3,MeetingTrainTestSets.meeting4,
      MeetingTrainTestSets.meeting5,MeetingTrainTestSets.meeting6,
      MeetingTrainTestSets.meeting7,MeetingTrainTestSets.meeting8,
      MeetingTrainTestSets.meeting9,MeetingTrainTestSets.meeting10)

    val argsok = CMDArgs.argsOk(args)

    if (!argsok._1) {
      logger.error(argsok._2) ; System.exit(-1)
    } else {

      val runningOptions = CMDArgs.getOLEDInputArgs(args)

      thresholds.foreach { threshold =>

        Globals.hedgePredictionThreshold = threshold

        var counter = 0
        var results = List.empty[(Int, Int, Int)]

        dataSets.foreach { dataset =>
          counter += 1
          println(s"Processing dataset/threshold $counter/$threshold")
          val trainingDataOptions =
            new MongoDataOptions(dbNames = dataset._1,
              chunkSize = runningOptions.chunkSize, targetConcept = runningOptions.targetHLE, sortDbByField = "time", what = "training")

          val testingDataOptions =
            new MongoDataOptions(dbNames = dataset._2,
              chunkSize = runningOptions.chunkSize, targetConcept = runningOptions.targetHLE, sortDbByField = "time", what = "testing")

          val trainingDataFunction: MongoDataOptions => Iterator[Example] = FullDatasetHoldOut.getMongoData

          val testingDataFunction: MongoDataOptions => Iterator[Example] = FullDatasetHoldOut.getMongoData


          // use a hand-crafted theory for sequential prediction (updates the weights but not the structure of the rules).
          //--evalth=/home/nkatz/Desktop/theory
          //val startMsg = "predict"

          val learner = new ExpLearner(runningOptions, trainingDataOptions, testingDataOptions, trainingDataFunction, testingDataFunction)
          val result = learner.run()
          results = results :+ result
        }

        val (totalTPs, totalFPs, totalFNs) = results.foldLeft(0,0,0) { (x, y) => (x._1 + y._1, x._2 + y._2, x._3 + y._3) }

        val microPrecision = totalTPs.toDouble/(totalTPs.toDouble + totalFPs.toDouble)
        val microRecall = totalTPs.toDouble/(totalTPs.toDouble + totalFNs.toDouble)
        val microFscore = (2*microPrecision*microRecall)/(microPrecision+microRecall)

        println(s"Results from threshold $threshold\ntps: $totalTPs\nfps: $totalFPs\nfns: $totalFNs\nPrecision: " +
          s"$microPrecision\nRecall: $microRecall\nMicro F1-score on test set: $microFscore")


      }


    }
  }
}

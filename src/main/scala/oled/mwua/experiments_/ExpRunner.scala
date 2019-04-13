package oled.mwua.experiments_

import java.io.{BufferedWriter, File, FileWriter}

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

    val learningRates = List(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9)
    val feedBackBias = List(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0)
    val inertia = List(true, false)

    /*
    val dataSets = Vector(MeetingTrainTestSets.meeting1, MeetingTrainTestSets.meeting2,
      MeetingTrainTestSets.meeting3,MeetingTrainTestSets.meeting4,
      MeetingTrainTestSets.meeting5,MeetingTrainTestSets.meeting6,
      MeetingTrainTestSets.meeting7,MeetingTrainTestSets.meeting8,
      MeetingTrainTestSets.meeting9,MeetingTrainTestSets.meeting10)
    */

    val train2 =
      Vector("caviar-video-21-meeting-moving", "caviar-video-7", "caviar-video-28-meeting", "caviar-video-25", "caviar-video-30",
        "caviar-video-11", "caviar-video-6", "caviar-video-14-meeting-moving", "caviar-video-26", "caviar-video-27",
        "caviar-video-20-meeting-moving", "caviar-video-13-meeting", "caviar-video-19-meeting-moving",
        "caviar-video-12-moving", "caviar-video-1-meeting-moving", "caviar-video-9", "caviar-video-16", "caviar-video-23-moving",
        "caviar-video-29", "caviar-video-5", "caviar-video-22-meeting-moving", "caviar-video-18", "caviar-video-4",
        "caviar-video-24-meeting-moving", "caviar-video-8", "caviar-video-10", "caviar-video-2-meeting-moving",
        "caviar-video-15", "caviar-video-3", "caviar-video-17")

    val argsok = CMDArgs.argsOk(args)

    if (!argsok._1) {
      logger.error(argsok._2) ; System.exit(-1)
    } else {

      val runningOptions = CMDArgs.getOLEDInputArgs(args)

      learningRates.foreach { rate =>
        Globals.sleepingExpertsLearningRate = rate
        feedBackBias.foreach { bias =>
          Globals.sleepingExpertsFeedBackBias = bias
          inertia.foreach { withInertia =>
            Globals.hedgeInertia = withInertia

            val trainingDataOptions =
              new MongoDataOptions(dbNames = train2, chunkSize = runningOptions.chunkSize,
                targetConcept = runningOptions.targetHLE, sortDbByField = "time", what = "training")

            val testingDataOptions = trainingDataOptions

            val trainingDataFunction: MongoDataOptions => Iterator[Example] = FullDatasetHoldOut.getMongoData
            val testingDataFunction: MongoDataOptions => Iterator[Example] = FullDatasetHoldOut.getMongoData

            val learner = new ExpLearner(runningOptions, trainingDataOptions, testingDataOptions, trainingDataFunction, testingDataFunction)
            val result = learner.run()

            //(Int, Int, Int, Double, Vector[Double])
            val (tps, fps, fns, fscore, errorVec) = (result._1,result._2,result._3,result._4,result._5)

            val msg = s"${runningOptions.targetHLE}, rate: $rate, feedback bias: $bias, " +
              s"inertia: $withInertia\ntps: $tps, fps: $fps, fns: $fns, total error: ${fps+fns}, fscore: $fscore\nerror vector:\n$errorVec\n\n"

            println(msg)

            val fw = new FileWriter(s"/home/nkatz/Desktop/TPLP-2019-results/${runningOptions.targetHLE}", true)
            try {
              fw.write(msg)
            }
            finally fw.close()


          }
        }
      }

    }
  }
}

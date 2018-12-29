package experiments.winnow

import akka.actor.{ActorSystem, Props}
import app.runutils.CMDArgs
import app.runutils.IOHandling.MongoSource
import com.mongodb.casbah.{MongoClient, MongoCollection}
import com.typesafe.scalalogging.LazyLogging
import experiments.caviar.FullDatasetHoldOut.MongoDataOptions
import experiments.caviar.{FullDatasetHoldOut, MeetingTrainTestSets}
import logic.Examples.Example
import oled.mwua.Learner
import utils.DataUtils.Interval

/**
  * Created by nkatz at 2/11/2018
  */

object ExpRunner extends LazyLogging {


  def main(args: Array[String]) = {

    val dataSets = Vector(MeetingTrainTestSets.meeting1, MeetingTrainTestSets.meeting2,
      MeetingTrainTestSets.meeting3,MeetingTrainTestSets.meeting4,
      MeetingTrainTestSets.meeting5,MeetingTrainTestSets.meeting6,
      MeetingTrainTestSets.meeting7,MeetingTrainTestSets.meeting8,
      MeetingTrainTestSets.meeting9,MeetingTrainTestSets.meeting10)

    // we give the dataset number as a single parameter at the end of the cmd args list, a simple number 0..9
    val datasetNum = args.last.toInt

    val _args = args.take(args.length-1)

    val argsok = CMDArgs.argsOk(_args)

    if (!argsok._1) {
      logger.error(argsok._2) ; System.exit(-1)
    } else {

      val runningOptions = CMDArgs.getOLEDInputArgs(_args)

      val dataset = dataSets(datasetNum)

      val trainingDataOptions =
        new MongoDataOptions(dbNames = dataset._1,
          chunkSize = runningOptions.chunkSize, targetConcept = runningOptions.targetHLE, sortDbByField = "time", what = "training")

      val testingDataOptions =
        new MongoDataOptions(dbNames = dataset._2,
          chunkSize = runningOptions.chunkSize, targetConcept = runningOptions.targetHLE, sortDbByField = "time", what = "testing")

      val trainingDataFunction: MongoDataOptions => Iterator[Example] = FullDatasetHoldOut.getMongoData

      val testingDataFunction: MongoDataOptions => Iterator[Example] = FullDatasetHoldOut.getMongoData

      val system = ActorSystem("HoeffdingLearningSystem")

      val startMsg = if (runningOptions.evalth != "None") "eval" else "start"

      // use a hand-crafted theory for sequential prediction (updates the weights but not the structure of the rules).
      //--evalth=/home/nkatz/Desktop/theory
      //val startMsg = "predict"

      system.actorOf(Props(
        new Learner(runningOptions,
          trainingDataOptions,
          testingDataOptions,
          trainingDataFunction,
          testingDataFunction,
          writeExprmtResultsTo = s"${System.getProperty("user.dir")}/results-${runningOptions.targetHLE}")), name = "Learner") !  startMsg

    }
  }
}

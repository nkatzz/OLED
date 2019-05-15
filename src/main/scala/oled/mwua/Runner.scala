package oled.mwua

import akka.actor.{ActorSystem, Props}
import app.runners.MLNDataHandler
import app.runners.MLNDataHandler.MLNDataOptions
import app.runutils.CMDArgs
import app.runutils.IOHandling.MongoSource
import com.mongodb.casbah.commons.MongoDBObject
import com.mongodb.casbah.{MongoClient, MongoCollection}
import com.typesafe.scalalogging.LazyLogging
import experiments.caviar.FullDatasetHoldOut.MongoDataOptions
import experiments.caviar.{FullDatasetHoldOut, MeetingTrainTestSets}
import experiments.datautils.caviar_data.CopyCAVIAR.{mc, newDB, newDBName}
import logic.Examples.Example
import utils.DataUtils.Interval
import com.mongodb.casbah.Imports._

/**
  * Created by nkatz at 2/11/2018
  */

//--inpath=/home/nkatz/dev/BKExamples/BK-various-taks/DevTest/caviar-bk/meeting --train=caviar --delta=0.001 --prune=0.8 --repfor=5 --chunksize=50 --try-more-rules=false --prequential=true --target=meeting --preprune=0.9 --onlineprune=false --spdepth=1

object Runner extends LazyLogging {


  def main(args: Array[String]) = {

    val argsok = CMDArgs.argsOk(args)

    if (!argsok._1) {
      logger.error(argsok._2) ; System.exit(-1)
    } else {

      val runningOptions = CMDArgs.getOLEDInputArgs(args)

      // This is the running setting with DefaultMongoDataOptions and the getMongoData function found in this class
      /*
      val trainingDataOptions = new DefaultMongoDataOptions(
        dbName = runningOptions.train,
        collectionName = runningOptions.mongoCollection,
        chunkSize = runningOptions.chunkSize,
        limit = runningOptions.dataLimit,
        targetConcept = runningOptions.targetHLE,
        sortDbByField = "None"
      )

      val testingDataOptions = trainingDataOptions
      val trainingDataFunction: DefaultMongoDataOptions => Iterator[Example] = getMongoData
      val testingDataFunction: DefaultMongoDataOptions => Iterator[Example] = getMongoData
      val system = ActorSystem("HoeffdingLearningSystem")

      val startMsg = if (runningOptions.evalth != "None") "eval" else "start"

      system.actorOf(Props(new Learner(runningOptions, trainingDataOptions, testingDataOptions, trainingDataFunction,
        testingDataFunction)), name = "Learner") !  startMsg
      */


      // This is the running setting in the object FullDatasetHoldOut

      /*
      val train1 =
      Vector("caviar-video-1-meeting-moving", "caviar-video-3", "caviar-video-2-meeting-moving", "caviar-video-5",
        "caviar-video-6", "caviar-video-13-meeting", "caviar-video-7", "caviar-video-8",
        "caviar-video-14-meeting-moving", "caviar-video-9", "caviar-video-10",
        "caviar-video-19-meeting-moving", "caviar-video-11", "caviar-video-12-moving",
        "caviar-video-20-meeting-moving", "caviar-video-15", "caviar-video-16",
        "caviar-video-21-meeting-moving", "caviar-video-17", "caviar-video-18",
        "caviar-video-22-meeting-moving", "caviar-video-4", "caviar-video-23-moving", "caviar-video-25",
        "caviar-video-24-meeting-moving", "caviar-video-26", "caviar-video-27",
        "caviar-video-28-meeting", "caviar-video-29", "caviar-video-30")
      */

      ///*
      val train1 =
        Vector("caviar-video-21-meeting-moving", "caviar-video-7", "caviar-video-28-meeting", "caviar-video-25", "caviar-video-30",
          "caviar-video-11", "caviar-video-6", "caviar-video-26", "caviar-video-27", "caviar-video-20-meeting-moving", "caviar-video-13-meeting",
          "caviar-video-12-moving", "caviar-video-1-meeting-moving", "caviar-video-9", "caviar-video-16", "caviar-video-23-moving",
          "caviar-video-29", "caviar-video-5", "caviar-video-18", "caviar-video-4", "caviar-video-19-meeting-moving",
          "caviar-video-24-meeting-moving", "caviar-video-8", "caviar-video-10", "caviar-video-2-meeting-moving",
          "caviar-video-22-meeting-moving", "caviar-video-15", "caviar-video-3",
          "caviar-video-17", "caviar-video-14-meeting-moving")
      //*/

      // just a re-ordering of train1, so that videos with positive examples are not left at the end of the sequence
      val train2 =
        Vector("caviar-video-21-meeting-moving", "caviar-video-7", "caviar-video-28-meeting", "caviar-video-25", "caviar-video-30",
          "caviar-video-11", "caviar-video-6", "caviar-video-14-meeting-moving", "caviar-video-26", "caviar-video-27",
          "caviar-video-20-meeting-moving", "caviar-video-13-meeting", "caviar-video-19-meeting-moving",
          "caviar-video-12-moving", "caviar-video-1-meeting-moving", "caviar-video-9", "caviar-video-16", "caviar-video-23-moving",
          "caviar-video-29", "caviar-video-5", "caviar-video-22-meeting-moving", "caviar-video-18", "caviar-video-4",
          "caviar-video-24-meeting-moving", "caviar-video-8", "caviar-video-10", "caviar-video-2-meeting-moving",
           "caviar-video-15", "caviar-video-3", "caviar-video-17")

      val openSSH = Vector("openssh")

      //val trainShuffled = scala.util.Random.shuffle(train1)
      //logger.info(s"\nData order:\n$trainShuffled")

      /* This is for running with the entire CAVIAR (no test set)*/
      ///*
      val trainingDataOptions =
        new MongoDataOptions(dbNames = train2, //trainShuffled ,//dataset._1,
          chunkSize = runningOptions.chunkSize, targetConcept = runningOptions.targetHLE, sortDbByField = "time", what = "training")

      val testingDataOptions = trainingDataOptions
      //*/

      /* This is for running on the training set and then performing prequential evaluation on the test set. */
      /*
      val dataset = MeetingTrainTestSets.meeting1
      //val dataset = MeetingTrainTestSets.meeting1

      //val trainShuffled = scala.util.Random.shuffle(dataset._1)
      //logger.info(s"\nData order:\n$trainShuffled")

      val trainingDataOptions =
        new MongoDataOptions(dbNames = dataset._1,//trainShuffled, //
          chunkSize = runningOptions.chunkSize, targetConcept = runningOptions.targetHLE, sortDbByField = "time", what = "training")

      val testingDataOptions =
        new MongoDataOptions(dbNames = dataset._2,
          chunkSize = runningOptions.chunkSize, targetConcept = runningOptions.targetHLE, sortDbByField = "time", what = "testing")
      */
      val trainingDataFunction: MongoDataOptions => Iterator[Example] = FullDatasetHoldOut.getMongoData
      val testingDataFunction: MongoDataOptions => Iterator[Example] = FullDatasetHoldOut.getMongoData

      /*
      val trainingDataOptions = new MLNDataOptions("/home/nkatz/dev/CAVIAR_MLN/CAVIAR_MLN/move/fold_1", runningOptions.chunkSize)
      val testingDataOptions = new MLNDataOptions("/home/nkatz/dev/CAVIAR_MLN/CAVIAR_MLN/move/fold_1", runningOptions.chunkSize)
      val trainingDataFunction: MLNDataOptions => Iterator[Example] = MLNDataHandler.getTrainingData
      val testingDataFunction: MLNDataOptions => Iterator[Example] = MLNDataHandler.getTestingData
      */

      val system = ActorSystem("HoeffdingLearningSystem")

      // use also start to evaluate a hand-crafted theory, the whole thing is hard-coded in Learner_NEW
      val startMsg = "start"//if (runningOptions.evalth != "None") "eval" else "start"

      // use a hand-crafted theory for sequential prediction (updates the weights but not the structure of the rules).
      //--evalth=/home/nkatz/Desktop/theory
      //val startMsg = "predict"

      /*
      // This is used to generate the "streaming" version of CAVIAR, where each entry in the database
      // consists of the observations at time t plus the labels at time t+1.
      val data = trainingDataFunction(trainingDataOptions)
      val dataPairs = data.sliding(2)
      val mc = MongoClient()

      val newDBName = s"caviar-streaming-${runningOptions.targetHLE}"
      val newDB = mc(newDBName)("examples")
      mc.dropDatabase(newDBName)

      var count = 0

      dataPairs foreach { pair =>
        val (first, second) = (pair.head, pair.tail.head)
        // The predictionTime atom is used by Aux.computeRuleGroundings to generate query atoms at the appropriate time point
        //val observations = first.narrative :+ List(s"time(${first.time.toInt+40})", s"predictionTime(${first.time.toInt+40})")
        val observations = first.narrative :+ s"time(${first.time.toInt+40})"
        val labels = second.annotation
        //val entry = MongoDBObject("time" -> first.time) ++ ("annotation" -> labels ) ++ ("narrative" -> observations)
        val entry = MongoDBObject("time" -> count) ++ ("annotation" -> labels ) ++ ("narrative" -> observations)
        count += 1
        if (labels.nonEmpty) println(entry)
        newDB.insert(entry)
      }
      */

      ///*
      system.actorOf(Props(new Learner_NEW(runningOptions, trainingDataOptions, testingDataOptions, trainingDataFunction,
        testingDataFunction)), name = "Learner") ! startMsg
      //*/

      // Use this to evaluate a hand-crafted theory
      /*
      system.actorOf(Props(new Learner_OLD_DEBUG(runningOptions, trainingDataOptions, testingDataOptions, trainingDataFunction,
        testingDataFunction)), name = "Learner") ! startMsg
      */

      // Use this for evaluating OLED
      /*
      system.actorOf(Props(new Learner(runningOptions, trainingDataOptions, testingDataOptions, trainingDataFunction,
        testingDataFunction)), name = "Learner") ! startMsg
      */
    }
  }

  private class DefaultMongoDataOptions(val dbName: String, val collectionName: String = "examples", val chunkSize: Int = 1,
                                        val limit: Double = Double.PositiveInfinity.toInt, val targetConcept: String = "None",
                                        val sortDbByField: String = "time", val sort: String = "ascending",
                                        val intervals: List[Interval] = Nil, val examplesIds: List[String] = Nil) extends MongoSource

  def getMongoData(opts: DefaultMongoDataOptions): Iterator[Example] = {

    val mc = MongoClient()
    val collection: MongoCollection = mc(opts.dbName)(opts.collectionName)

    val data = opts.allData(collection, opts.sort, opts.sortDbByField) map { x =>
      val e = Example(x)
      opts.targetConcept match {
        case "None" => new Example(annot = e.annotation, nar = e.narrative, _time = e.time)
        case _ => new Example(annot = e.annotation filter (_.contains(opts.targetConcept)), nar = e.narrative, _time = e.time)
      }
    }

    opts.chunkSize > 1 match {
      case false => data
      case _ =>
        data.grouped(opts.chunkSize).map { x =>
          //data.sliding(opts.chunkSize).map { x =>
          x.foldLeft(Example()) { (z, y) =>
            new Example(annot = z.annotation ++ y.annotation, nar = z.narrative ++ y.narrative, _time = x.head.time)
          }
        }
    }
  }


}

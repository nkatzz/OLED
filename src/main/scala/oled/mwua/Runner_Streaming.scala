package oled.mwua

import akka.actor.{ActorSystem, Props}
import app.runutils.CMDArgs
import app.runutils.IOHandling.MongoSource
import com.mongodb.casbah.{MongoClient, MongoCollection}
import com.typesafe.scalalogging.LazyLogging
import experiments.caviar.FullDatasetHoldOut
import experiments.caviar.FullDatasetHoldOut.MongoDataOptions
import logic.Examples.Example
import oled.mwua.Runner.logger

object Runner_Streaming extends LazyLogging {

  def main(args: Array[String]) = {
    val argsok = CMDArgs.argsOk(args)
    if (!argsok._1) {
      logger.error(argsok._2) ; System.exit(-1)
    } else {

      val runningOptions = CMDArgs.getOLEDInputArgs(args)

      val trainingDataOptions = new StreamingMongoDataOptions(dbName = runningOptions.train, targetConcept = runningOptions.targetHLE)

      val testingDataOptions = trainingDataOptions

      val trainingDataFunction: StreamingMongoDataOptions => Iterator[Example] = getMongoData
      val testingDataFunction: StreamingMongoDataOptions => Iterator[Example] = getMongoData

      val system = ActorSystem("HoeffdingLearningSystem")

      // use also start to evaluate a hand-crafted theory, the whole thing is hard-coded in Learner_NEW
      val startMsg = "start-streaming" //"start"//if (runningOptions.evalth != "None") "eval" else "start"

      system.actorOf(Props(new Learner_NEW(runningOptions, trainingDataOptions, testingDataOptions, trainingDataFunction,
        testingDataFunction)), name = "Learner") ! startMsg

    }
  }

  class StreamingMongoDataOptions(val dbName: String, val chunkSize: Int = 1,
                                  val limit: Double = Double.PositiveInfinity.toInt,
                                  val targetConcept: String = "None", val sortDbByField: String = "time",
                                  val sort: String = "ascending") extends MongoSource

  def getMongoData(opts: StreamingMongoDataOptions): Iterator[Example] = {
    val mc = MongoClient()
    val collection: MongoCollection = mc(opts.dbName)("examples")

    val dataIterator = opts.allData(collection, opts.sort, opts.sortDbByField) map { x =>
      val e = Example(x)
      opts.targetConcept match {
        case "None" => new Example(annot = e.annotation, nar = e.narrative, _time = e.time)
        case _ => new Example(annot = e.annotation filter (_.contains(opts.targetConcept)), nar = e.narrative, _time = e.time)
      }
    }
    dataIterator
  }

}

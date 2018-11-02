package app.runners

import akka.actor.{ActorSystem, Props}
import app.runutils.CMDArgs
import app.runutils.IOHandling.MongoSource
import com.mongodb.casbah.{MongoClient, MongoCollection}
import com.typesafe.scalalogging.LazyLogging
import logic.Examples.Example
import oled.single_core.Master
import utils.DataUtils.Interval


/**
  * Created by nkatz on 6/30/17.
  */

object OLEDDefaultRunner extends LazyLogging {

  def main(args: Array[String]) = {

    val argsok = CMDArgs.argsOk(args)

    if (!argsok._1) {

      logger.error(argsok._2)
      System.exit(-1)

    } else {

      val runningOptions = CMDArgs.getOLEDInputArgs(args)

      val trainingDataOptions = new DefaultMongoDataOptions(
        dbName = runningOptions.train,
        collectionName = runningOptions.mongoCollection,
        chunkSize = runningOptions.chunkSize,
        limit = runningOptions.dataLimit,
        targetConcept = runningOptions.targetHLE,
        sortDbByField = "None"
      )

      val testingDataOptions = new DefaultMongoDataOptions(
        dbName = runningOptions.test,
        collectionName = runningOptions.mongoCollection,
        chunkSize = runningOptions.chunkSize,
        limit = runningOptions.dataLimit,
        targetConcept = runningOptions.targetHLE,
        sortDbByField = "None"
      )


      val trainingDataFunction: DefaultMongoDataOptions => Iterator[Example] = getMongoData
      val testingDataFunction: DefaultMongoDataOptions => Iterator[Example] = getMongoData
      val system = ActorSystem("HoeffdingLearningSystem")
      val startMsg = if (runningOptions.evalth != "None") "eval" else "start"

      system.actorOf(Props(new Master(runningOptions, trainingDataOptions, testingDataOptions, trainingDataFunction,
        testingDataFunction)), name = "Master-Actor") !  startMsg

    }
  }

  private class DefaultMongoDataOptions(val dbName: String,
                                        val collectionName: String = "examples",
                                        val chunkSize: Int = 1,
                                        val limit: Double = Double.PositiveInfinity.toInt,
                                        val targetConcept: String = "None",
                                        val sortDbByField: String = "None",
                                        val sort: String = "ascending",
                                        val intervals: List[Interval] = Nil,
                                        val examplesIds: List[String] = Nil) extends MongoSource

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
package experiments.caviar

import akka.actor.{ActorSystem, Props}
import app.runutils.CMDArgs
import app.runutils.IOHandling.MongoSource
import com.mongodb.casbah.{MongoClient, MongoCollection}
import com.typesafe.scalalogging.LazyLogging
import logic.Examples.Example
import oled.mwua.Learner


object FullDatasetHoldOut extends LazyLogging {

  def main(args: Array[String]) = {

    val argsok = CMDArgs.argsOk(args)

    if (!argsok._1) {
      logger.error(argsok._2) ; System.exit(-1)
    } else {

      val runningOptions = CMDArgs.getOLEDInputArgs(args)

      val dataset = MeetingTrainTestSets.meeting7

      val trainingDataOptions =
        new MongoDataOptions(dbNames = dataset._1,
          chunkSize = runningOptions.chunkSize, targetConcept = runningOptions.targetHLE, sortDbByField = "time", what = "training")

      val testingDataOptions =
        new MongoDataOptions(dbNames = dataset._2,
          chunkSize = runningOptions.chunkSize, targetConcept = runningOptions.targetHLE, sortDbByField = "time", what = "testing")

      val trainingDataFunction: MongoDataOptions => Iterator[Example] = getMongoData

      val testingDataFunction: MongoDataOptions => Iterator[Example] = getMongoData

      val system = ActorSystem("HoeffdingLearningSystem")

      val startMsg = if (runningOptions.evalth != "None") "eval" else "start"

      system.actorOf(Props(new Learner(runningOptions, trainingDataOptions, testingDataOptions, trainingDataFunction,
        testingDataFunction)), name = "Learner") !  startMsg

    }
  }




  class MongoDataOptions(val dbNames: Vector[String], val chunkSize: Int = 1,
                                 val limit: Double = Double.PositiveInfinity.toInt,
                                 val targetConcept: String = "None", val sortDbByField: String = "time",
                                 val sort: String = "ascending", val what: String = "training") extends MongoSource


  /* "what" is either training or testing */
  def getMongoData(opts: MongoDataOptions): Iterator[Example] = {
    val mc = MongoClient()

    val exmplIters = opts.dbNames map { dbName =>

      val collection: MongoCollection = mc(dbName)("examples")

      val data = opts.allData(collection, opts.sort, opts.sortDbByField) map { x =>
        val e = Example(x)

        opts.targetConcept match {
          case "None" => new Example(annot = e.annotation, nar = e.narrative, _time = e.time)
          case _ => new Example(annot = e.annotation filter (_.contains(opts.targetConcept)), nar = e.narrative, _time = e.time)
        }
      }

      if (opts.what == "training") {
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
      } else { // no chunking for testing data
        ///*
        val firstDataPoint = data.next()
        val annotation = firstDataPoint.annotation
        val narrative = firstDataPoint.narrative
        val time = firstDataPoint.time
        val merged = data.foldLeft(annotation, narrative) { (accum, ex) =>
          (accum._1 ++ ex.annotation, accum._2 ++ ex.narrative)
        }
        val e = new Example(annot = merged._1, nar = merged._2, _time = time)
        Iterator(e)
        //*/

        // comment the above and uncomment this to have chunked data
        /*
        data.grouped(opts.chunkSize).map { x =>
          //data.sliding(opts.chunkSize).map { x =>
          x.foldLeft(Example()) { (z, y) =>
            new Example(annot = z.annotation ++ y.annotation, nar = z.narrative ++ y.narrative, _time = x.head.time)
          }
        }
        */
      }
    }
    exmplIters.foldLeft(Iterator[Example]())(_ ++ _)
  }



}

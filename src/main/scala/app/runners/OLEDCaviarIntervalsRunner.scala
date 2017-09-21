package app.runners

import app.runutils.IOHandling.MongoSource
import com.mongodb.casbah.{MongoClient, MongoCollection}
import data_handling.caviar_intervals.{MeetingTrainingData, MovingTrainingData}
import utils.CaviarUtils
import utils.DataUtils.{DataAsIntervals, Interval}

import scala.util.Random

/**
  * Created by nkatz on 7/3/17.
  */




object OLEDCaviarIntervalsRunner {



  private class DataOptions(val dbName: String,
                            val collectionName: String = "examples",
                            val chunkSize: Int = 1,
                            val limit: Double = Double.PositiveInfinity.toInt,
                            val targetConcept: String = "None",
                            val sortDbByField: String = "None",
                            val sort: String = "ascending",
                            val intervals: List[Interval] = Nil,
                            val trainSetid: Int,
                            val randomOrder: Boolean = true,
                            val shuffle: Boolean = false) extends MongoSource

  def getDataFromIntervals(opts: DataOptions) = {
    val mc = MongoClient()
    val collection: MongoCollection = mc(opts.dbName)(opts.collectionName)

    val intervals =
      if (opts.targetConcept == "meeting") {
        List(MeetingTrainingData.getMeetingTrainingData(opts.trainSetid, randomOrder = opts.randomOrder))
      } else {
        List(MovingTrainingData.getMovingTrainingData(opts.trainSetid, randomOrder = opts.randomOrder))
      }

    if (opts.shuffle) {
      //val shuffled = List(data.asInstanceOf[DataAsIntervals].trainingSet.head) ++ Random.shuffle(data.asInstanceOf[DataAsIntervals].trainingSet.tail)
      //CaviarUtils.getDataFromIntervals(collection, params.targetHLE, shuffled, params.chunkSize)
    } else {

    }


  }


}

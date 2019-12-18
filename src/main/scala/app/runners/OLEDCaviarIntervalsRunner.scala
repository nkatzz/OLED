/*
 * Copyright (C) 2016  Nikos Katzouris
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

package app.runners

import akka.actor.{ActorSystem, Props}
import app.runutils.CMDArgs
import app.runutils.IOHandling.MongoSource
import com.mongodb.casbah.{MongoClient, MongoCollection}
import experiments.datautils.caviar_intervals.{MeetingTrainingData, MovingTrainingData}
import logic.Examples.Example
import oled.single_core.Master
import utils.CaviarUtils
import utils.DataUtils.{DataAsIntervals, Interval}

import scala.util.Random

/**
  * Created by nkatz on 7/3/17.
  */

object OLEDCaviarIntervalsRunner {

  private class DataOptions(
      val dbName: String,
      val collectionName: String = "examples",
      val chunkSize: Int = 1,
      val limit: Double = Double.PositiveInfinity.toInt,
      val targetConcept: String = "None",
      val sortDbByField: String = "None",
      val sort: String = "ascending",
      val intervals: List[Interval] = Nil,
      val trainSetid: Int,
      val randomOrder: Boolean = false,
      val shuffle: Boolean = false) extends MongoSource

  def main(args: Array[String]) = {

    val trainSetId = args.map(x => x.split("=")).find(x => x(0) == "--trainset").
      getOrElse(throw new RuntimeException("--trainset missing."))(1).toInt

    //val trainSetId = 2 // change this to run different folds

    val argsok = CMDArgs.argsOk(args)
    if (!argsok._1) {
      println(argsok._2)
      System.exit(-1)
    } else {
      val runningOptions = CMDArgs.getOLEDInputArgs(args)
      val trainingDataOptions =
        new DataOptions(dbName         = runningOptions.train,
                        collectionName = runningOptions.mongoCollection,
                        chunkSize      = runningOptions.chunkSize,
                        limit          = runningOptions.dataLimit,
                        targetConcept  = runningOptions.targetHLE,
                        sortDbByField  = "time",
                        trainSetid     = trainSetId,
                        randomOrder    = runningOptions.randomOrder)
      val testingDataOptions = trainingDataOptions
      val trainingDataFunction: DataOptions => Iterator[Example] = getTrainingData
      val testingDataFunction: DataOptions => Iterator[Example] = getTestingData
      val system = ActorSystem("HoeffdingLearningSystem")
      val startMsg = if (runningOptions.evalth != "None") "eval" else "start"
      system.actorOf(Props(new Master(runningOptions, trainingDataOptions,
                                      testingDataOptions, trainingDataFunction, testingDataFunction)), name = "Master-Actor") ! startMsg

    }
  }

  def getTrainingData(opts: DataOptions) = {
    val data = getIntervals(opts)
    val trainingIntervals = data.trainingSet
    val mc = MongoClient()
    val collection: MongoCollection = mc(opts.dbName)(opts.collectionName)
    CaviarUtils.getDataFromIntervals(collection, opts.targetConcept, trainingIntervals, opts.chunkSize)
  }

  def getTestingData(opts: DataOptions) = {
    val data = getIntervals(opts)
    val testingIntervals = data.testingSet
    val mc = MongoClient()
    val collection: MongoCollection = mc(opts.dbName)(opts.collectionName)
    //CaviarUtils.getDataFromIntervals(collection, opts.targetConcept, testingIntervals, opts.chunkSize)
    CaviarUtils.getDataFromIntervals(collection, opts.targetConcept, testingIntervals, 200) // fix the chunk size in testing
  }

  def getIntervals(opts: DataOptions) = {
    if (opts.targetConcept == "meeting") {
      MeetingTrainingData.getMeetingTrainingData(opts.trainSetid, randomOrder = opts.randomOrder)
    } else {
      MovingTrainingData.getMovingTrainingData(opts.trainSetid, randomOrder = opts.randomOrder)
    }
  }
}

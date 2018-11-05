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

package app.runners.maritime_experiments

import akka.actor.{ActorSystem, Props}
import app.runutils.CMDArgs
import logic.Examples.Example
import oled.single_core.Master

import scala.io.Source

/**
  * Created by nkatz on 7/9/17.
  */


object SingleCoreMaritimeRunner {

  def main(args: Array[String]) = {
    val argsok = CMDArgs.argsOk(args)
    if (!argsok._1) {
      println(argsok._2)
      System.exit(-1)
    } else {
      val runOpts = CMDArgs.getOLEDInputArgs(args)

      val trainingDataOptions = SingleCoreDataOptions.sailingDataOptionsTraining
      val testingDataOptions = trainingDataOptions

      // the key is area
      // This speedLimitsMap is common to all cores (there's only one general speed limits file)
      val speedLimitsMap = populateSpeedLimitsMap(trainingDataOptions.speedLimitsPath, scala.collection.mutable.Map[String, scala.collection.mutable.Set[String]]())

      val nodeData = new NodeData(trainingDataOptions.hlePath, trainingDataOptions.llePath, trainingDataOptions.closeToPortsPath,
        trainingDataOptions.targetConcept, speedLimitsMap)

      val trainingFunction: MaritimeDataOptions => Iterator[Example] = nodeData.getTrainingData
      val testingFunction: MaritimeDataOptions => Iterator[Example] = nodeData.getTestingData

      val system = ActorSystem("HoeffdingLearningSystem")
      val startMsg = if (runOpts.evalth != "None") "EvaluateHandCrafted" else "start"
      system.actorOf(Props(new Master(runOpts, trainingDataOptions, testingDataOptions, trainingFunction, testingFunction)), name = "Master-Actor") !  startMsg


    }
  }

  def populateSpeedLimitsMap(dataPath: String, speedLimitsMap: scala.collection.mutable.Map[String, scala.collection.mutable.Set[String]]) = {
    println("Getting speed limits map")
    val data = Source.fromFile(dataPath).getLines
    data foreach { x =>
      val s = x.split("\\|")
      val area = s(0)
      val limit = s(1)
      val atom = s"""speedLimit("$area","$limit")"""
      if (speedLimitsMap.contains(area)) speedLimitsMap(area) = speedLimitsMap(area) += atom
      else speedLimitsMap(area) = scala.collection.mutable.Set(atom)
    }
    speedLimitsMap
  }


}

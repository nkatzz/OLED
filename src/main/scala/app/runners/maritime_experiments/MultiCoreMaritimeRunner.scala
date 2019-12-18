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
import app.runutils.{CMDArgs, RunningOptions}
import logic.Examples.Example
import oled.distributed.Dispatcher

/**
  * Created by nkatz on 7/9/17.
  */

object MultiCoreMaritimeRunner {

  def main(args: Array[String]) = {

    val argsok = CMDArgs.argsOk(args)
    if (!argsok._1) {
      println(argsok._2)
      System.exit(-1)
    } else {

      val runOpts = CMDArgs.getOLEDInputArgs(args)

      val opts = getOptions("lowSpeed", 50, 1250, 8)

      val speedLimitsMap = app.runners.maritime_experiments.SingleCoreMaritimeRunner.populateSpeedLimitsMap(opts.head.speedLimitsPath, scala.collection.mutable.Map[String, scala.collection.mutable.Set[String]]())

      val p = prepare(runOpts, opts, speedLimitsMap)

      val testingFunction: MaritimeDataOptions => Iterator[Example] = p.head._1.getTestingData

      val testingOptions = opts.head

      val message = "go"

      // Start the actor system
      val system = ActorSystem("distributed-oled")

      system.actorOf(Props(new Dispatcher(opts zip p.map(x => x._2), runOpts, 2, testingOptions, testingFunction)), name = "TopLevelDispatcher") ! message

    }

  }

  def getOptions(hle: String, chunkSize: Int, limit: Int, coresNum: Int) = {
    val prefixes = coresNum match {
      case 2 => List("2-1", "2-2")
      case 4 => List("4-1", "4-2", "4-3", "4-4")
      case 8 => List("8-1", "8-2", "8-3", "8-4", "8-5", "8-6", "8-7", "8-8")
    }
    val joinPrefix = (p: String) => p.split("-").mkString("")
    val llePath = "/home/nkatz/dev/maritime/brest-data/datasets-my-split"
    val hlePath = "/home/nkatz/dev/maritime/brest-data/recognition-my-split"
    val speedLimitsPath = "/home/nkatz/dev/maritime/brest-data/areas_speed_limits.csv"
    val info = prefixes map (x => (s"$llePath/dataset${joinPrefix(x)}.txt", s"brest-$x", s"$hlePath/${joinPrefix(x)}/$hle.csv", s"$hlePath/${joinPrefix(x)}/close_to_ports.csv"))
    info map { x =>
      new MaritimeDataOptions(llePath          = x._1, db = x._2, hlePath = x._3, speedLimitsPath = speedLimitsPath, closeToPortsPath = x._4, chunkSize, limit.toDouble, hle)
    }

  }

  def prepare(runOpts: RunningOptions, opts: List[MaritimeDataOptions],
      speedLimitsMap: scala.collection.mutable.Map[String, scala.collection.mutable.Set[String]]) = {

    opts.map { opt =>
      val nodeData = new NodeData(opt.hlePath, opt.llePath, opt.closeToPortsPath, opt.targetConcept, speedLimitsMap)
      val trainingFunction: MaritimeDataOptions => Iterator[Example] = nodeData.getTrainingData
      (nodeData, trainingFunction)
    }

    //val testingFunction: MaritimeDataOptions => Iterator[Example] = nodeData.getTestingData

  }

}

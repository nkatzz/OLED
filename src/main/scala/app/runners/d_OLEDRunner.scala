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
import app.runners.OLEDMaritimeRunner._
import app.runutils.{CMDArgs, Globals}
import datautils.caviar_intervals.MeetingTrainingDistributed
import datautils.maritime_data.yet_another_attempt.MaritimeToMongo.{populatePortsMap, populateSpeedLimitsMap}
import logic.Examples.Example
import oled.distributed.{Dispatcher, Utils}
import utils.DataUtils.DataAsIntervals



/**
  * Created by nkatz on 2/13/17.
  */



object d_OLEDRunner {

  def main(args: Array[String]) = {

    val argsok = CMDArgs.argsOk(args)

    if (!argsok._1) {
      println(argsok._2)
      System.exit(-1)
    } else {

      Globals.glvalues("distributed") = "true"
      val params = CMDArgs.getOLEDInputArgs(args)

      val dataFunction: MaritimeDataOptions => Iterator[Example] = getData

      //val options = List( (core_2_1, dataFunction), (core_2_2, dataFunction) )
      //val testingOptions = testingData

      //populateHLEsMap(dataOpts1.hlePath, dataOpts1.targetConcept)
      //populatePortsMap(dataOpts1.closeToPortsPath)
      //populateProximityMap(dataOpts1.llePath)
      //populateSpeedLimitsMap(dataOpts1.speedLimitsPath)


      val message = "go"

      // Start the actor system
      val system = ActorSystem("distributed-oled")

      //system.actorOf(Props( new Dispatcher(options, params, 2, testingOptions, dataFunction) ), name = "TopLevelDispatcher") ! message

    }
  }















}

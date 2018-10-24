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

package datautils.maritime_data.yet_another_attempt

import com.mongodb.casbah.Imports.MongoDBObject
import com.mongodb.casbah.{MongoClient, MongoCollection}
import com.mongodb.casbah.Imports._
import scala.io.Source

/**
  * Created by nkatz on 7/4/17.
  */

object MaritimeToMongo {

  val path = "/home/nkatz/dev/maritime/nkatz_brest/1-core"
  private val datasetFile = path+"/dataset.txt" // The LLEs file
  private val speedLimitsFile = path+"/static_data/all_areas/areas_speed_limits.csv"
  private val closeToPortsFile = path+"/recognition/close_to_ports.csv" // this has a different schema than the other hles
  private val highSpeedFile = path+"/recognition/highSpeedIn-no-infs.csv"
  private val loiteringFile = path+"/recognition/loitering-no-infs.csv"
  private val lowSpeedFile = path+"/recognition/lowSpeed-no-infs.csv"
  private val sailingFile = path+"/recognition/sailing-no-infs.csv"
  private val stoppedFile = path+"/recognition/stopped-no-infs.csv"
  private val withinAreaFile = path+"/recognition/withinArea-no-infs.csv"
  private val rendezVousFile = path+"/recognition/rendezVouz-no-infs.csv"

  // The key is vessel
  var HLEMap = scala.collection.mutable.Map[String, scala.collection.mutable.Set[String]]()
  // The key is time
  var portsMap = scala.collection.mutable.Map[String, scala.collection.mutable.Set[String]]()
  // The key is vessel
  var proximityMap = scala.collection.mutable.Map[String, scala.collection.mutable.Set[String]]()
  // the key is area
  var speedLimitsMap = scala.collection.mutable.Map[String, scala.collection.mutable.Set[String]]()
  // The key is time
  var LLEMap = scala.collection.mutable.Map[Int, (scala.collection.mutable.Set[String], scala.collection.mutable.Set[String], scala.collection.mutable.Set[String])]()





  def main(args: Array[String]) = {

    val dbName = "maritime-brest"
    val mongoClient = MongoClient()
    mongoClient(dbName).dropDatabase()

    val newCollection = mongoClient(dbName)("examples")
    newCollection.dropCollection()
    newCollection.createIndex(MongoDBObject("time"-> 1))

    populateHLEMap(highSpeedFile, "highSpeedIn")
    populatePortsMap(closeToPortsFile)
    populateProximityMap(datasetFile)
    populateSpeedLimitsMap(speedLimitsFile)
    populateLLEsMap(datasetFile)
    joinDataInMongo(newCollection)
  }

  def joinDataInMongo(newCollection: MongoCollection) = {

    var counter = 0

    val times = LLEMap.keySet

    times foreach { time =>
      val record = LLEMap(time)
      val (lleAtoms, vessels, areas) = (record._1, record._2, record._3)

      val hleAtoms = vessels.flatMap { v =>
        if(HLEMap.contains(v)) HLEMap(v)
        else scala.collection.mutable.Set[String]()
      }
      val proximityAtoms = vessels.flatMap { v =>
        if (proximityMap.contains(v)) proximityMap(v)
        else scala.collection.mutable.Set[String]()
      }

      val closeToPortsAtoms = {
        if(portsMap.contains(time.toString)) portsMap(time.toString)
        else scala.collection.mutable.Set[String]()
      }

      val speedLimitAtoms = areas.flatMap { a =>
        if (speedLimitsMap.contains(a)) speedLimitsMap(a)
        else scala.collection.mutable.Set[String]()
      }.filter(p => p != "None")

      val narrative = lleAtoms ++ proximityAtoms ++ closeToPortsAtoms ++ speedLimitAtoms
      val entry = MongoDBObject("annotation" -> hleAtoms) ++ ("narrative" -> narrative) ++ ("time" -> time.toInt)
      newCollection.insert(entry)
      counter += 1
      println(counter)
    }
  }


  def LLEsToMongo(dataPath: String, collection: MongoCollection) = {

    println("Inserting LLEs to mongo")

    collection.createIndex(MongoDBObject("time" -> 1))

    val data = Source.fromFile(dataPath).getLines//.filter(x => !x.contains("HoldsFor") && !x.contains("coord"))
    while(data.hasNext) {
      val x = data.next()
      if (!x.contains("HoldsFor") && !x.contains("coord")) {
        var area = "None"
        var predicate = "None"
        val info = x.split("HappensAt")(1)
        val _info = info.split("\\]")
        val time = _info(1).trim.toInt
        val rest = _info(0).split("\\[")(1)
        val lle = rest.split(" ")(0)
        val vessel = rest.split(" ")(1)
        lle match {
          case "gap_start" =>
            //HappensAt [gap_start 271043753] 1451802715
            predicate = s"""happensAt(gap_start("$vessel"),"$time")"""
          case "velocity" =>
            //HappensAt [velocity 240675000 0 270.00005134150797] 1451802711
            //the 4th parameter in [velocity 240675000 0 270.00005134150797] is heading, which is not used anywhere
            val speed = rest.split(" ")(2)
            predicate = s"""happensAt(velocity("$vessel","$speed"),"$time")"""
          case "change_in_speed_start" =>
            //HappensAt [change_in_speed_start 237955000] 1451802743
            predicate = s"""happensAt(change_in_speed_start("$vessel"),"$time")"""
          case "stop_start" =>
            //HappensAt [stop_start 636013060] 1451802771
            predicate = s"""happensAt(stop_start("$vessel"),"$time")"""
          case "change_in_heading" =>
            //HappensAt [change_in_heading 240096000] 1451802787
            predicate = s"""happensAt(change_in_heading("$vessel"),"$time")"""
          case "isInArea" =>
            //HappensAt [isInArea 239471800 area300240700] 1451802848
            area = rest.split(" ")(2)
            predicate = s"""happensAt(isInArea("$vessel", "$area"),"$time")"""
          case "change_in_speed_end" =>
            //HappensAt [change_in_speed_end 237144200] 1451802872
            predicate = s"""happensAt(change_in_speed_end("$vessel"),"$time")"""
          case "slow_motion_start" =>
            //HappensAt [slow_motion_start 240802000] 1451802892
            predicate = s"""happensAt(slow_motion_start("$vessel"),"$time")"""
          case "stop_end" =>
            //HappensAt [stop_end 356460000] 1451802924
            predicate = s"""happensAt(stop_end("$vessel"),"$time")"""
          case "gap_end" =>
            //HappensAt [gap_end 271043772] 1451802920
            predicate = s"""happensAt(gap_end("$vessel"),"$time")"""
          case "leavesArea" =>
            //HappensAt [leavesArea 239371500 area300674000] 1451802925
            area = rest.split(" ")(2)
            predicate = s"""happensAt(leavesArea("$vessel","$area"),"$time")"""
          case "slow_motion_end" =>
            predicate = s"""happensAt(slow_motion_end("$vessel"),"$time")"""
          //HappensAt [slow_motion_end 271044099] 1451802937
        }
        val entry = MongoDBObject("time" -> time.toInt) ++ ("predicate" -> predicate) ++ ("vessels" -> List(vessel)) ++ ("areas" -> List(area))
        collection.insert(entry)
      }
    }
  }




  def populateHLEMap(dataPath: String, hle: String) = {
    println(s"Getting $hle map")
    handleHLEs(dataPath, hle)
  }

  /* CAN'T GET THIS INTO A MAP */
  ///*
  def populateLLEsMap(dataPath: String) = {
    println("Getting LLEs map")

    var counter = 0

    val data = Source.fromFile(dataPath).getLines
    while(data.hasNext) {
      val x = data.next()
      if (!x.contains("HoldsFor") && !x.contains("coord")) {
        var area = "None"
        var predicate = "None"
        val info = x.split("HappensAt")(1)
        val _info = info.split("\\]")
        val time = _info(1).trim.toInt
        val rest = _info(0).split("\\[")(1)
        val lle = rest.split(" ")(0)
        val vessel = rest.split(" ")(1)
        lle match {
          case "gap_start" =>
            //HappensAt [gap_start 271043753] 1451802715
            predicate = s"""happensAt(gap_start("$vessel"),"$time")"""
          case "velocity" =>
            //HappensAt [velocity 240675000 0 270.00005134150797] 1451802711
            //the 4th parameter in [velocity 240675000 0 270.00005134150797] is heading, which is not used anywhere
            val speed = rest.split(" ")(2)
            predicate = s"""happensAt(velocity("$vessel","$speed"),"$time")"""
          case "change_in_speed_start" =>
            //HappensAt [change_in_speed_start 237955000] 1451802743
            predicate = s"""happensAt(change_in_speed_start("$vessel"),"$time")"""
          case "stop_start" =>
            //HappensAt [stop_start 636013060] 1451802771
            predicate = s"""happensAt(stop_start("$vessel"),"$time")"""
          case "change_in_heading" =>
            //HappensAt [change_in_heading 240096000] 1451802787
            predicate = s"""happensAt(change_in_heading("$vessel"),"$time")"""
          case "isInArea" =>
            //HappensAt [isInArea 239471800 area300240700] 1451802848
            area = rest.split(" ")(2)
            predicate = s"""happensAt(isInArea("$vessel", "$area"),"$time")"""
          case "change_in_speed_end" =>
            //HappensAt [change_in_speed_end 237144200] 1451802872
            predicate = s"""happensAt(change_in_speed_end("$vessel"),"$time")"""
          case "slow_motion_start" =>
            //HappensAt [slow_motion_start 240802000] 1451802892
            predicate = s"""happensAt(slow_motion_start("$vessel"),"$time")"""
          case "stop_end" =>
            //HappensAt [stop_end 356460000] 1451802924
            predicate = s"""happensAt(stop_end("$vessel"),"$time")"""
          case "gap_end" =>
            //HappensAt [gap_end 271043772] 1451802920
            predicate = s"""happensAt(gap_end("$vessel"),"$time")"""
          case "leavesArea" =>
            //HappensAt [leavesArea 239371500 area300674000] 1451802925
            area = rest.split(" ")(2)
            predicate = s"""happensAt(leavesArea("$vessel","$area"),"$time")"""
          case "slow_motion_end" =>
            predicate = s"""happensAt(slow_motion_end("$vessel"),"$time")"""
          //HappensAt [slow_motion_end 271044099] 1451802937
        }
        if (LLEMap.contains(time)) {
          val currentValue = LLEMap(time)
          val updatedAtoms = scala.collection.mutable.Set[String]() ++= currentValue._1 += predicate
          val updatedVessels = scala.collection.mutable.Set[String]() ++= currentValue._2 += vessel
          val updatedAreas = scala.collection.mutable.Set[String]() ++= currentValue._3 += area
          LLEMap(time) = (updatedAtoms, updatedVessels, updatedAreas)
        } else {
          LLEMap(time) = (scala.collection.mutable.Set(predicate), scala.collection.mutable.Set(vessel), scala.collection.mutable.Set(area))
        }
      }
      counter += 1
      //println(s"Grouping LLEs by time. Data point: $counter")
    }
  }
  //*/

  def populateSpeedLimitsMap(dataPath: String) = {
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
  }

  def populateProximityMap(dataPath: String) = {
    println("Getting proximity map")
    val lines = Source.fromFile(dataPath).getLines.filter(x => x.contains("proximity"))
    lines foreach { x =>
      val s = x.split("=")(0).split(" ")
      val vessel1 = s(2)
      val vessel2 = s(3)
      val z = x.split("=")(1).trim().split(" ")(1).split("\\(")(1).split("\\)")(0).split("-")
      val startTime = z(0).toInt
      val endTime = z(1).toInt - 1
      val atom = s"""close("$vessel1","$vessel2",interval("$startTime","$endTime"))"""
      if (proximityMap.contains(vessel1)) proximityMap(vessel1) = proximityMap(vessel1) += atom
      else proximityMap(vessel1) = scala.collection.mutable.Set(atom)
      if (proximityMap.contains(vessel2)) proximityMap(vessel2) = proximityMap(vessel2) += atom
      else proximityMap(vessel2) = scala.collection.mutable.Set(atom)
    }
  }

  def populatePortsMap(dataPath: String) = {
    println("Getting close-to-ports map")
    val data = Source.fromFile(dataPath).getLines
    data foreach { x =>
      val s = x.split("\\|")
      val time = s(0)
      val vessel = s(1)
      val atom = s"""notCloseToPorts("$vessel","$time")"""
      if (portsMap.contains(time)) portsMap(time) = portsMap(time) += atom
      else portsMap(time) = scala.collection.mutable.Set(atom)
    }
  }

  def handleHLEs(path: String, hle: String) = {
    val data = Source.fromFile(path).getLines.filter(x => !x.contains("inf"))
    hle match {
      case "highSpeedIn" | "withinArea" =>
        data foreach { x =>
          //println()
          val s = x.split("\\|")
          val (startTime, endTime, vessel, area) = (s(4).toInt, s(5).toInt - 1, s(1), s(2))
          val atom = s"""holdsAt($hle("$vessel","$area"),interval("$startTime","$endTime"))"""
          if (HLEMap.contains(vessel)) HLEMap(vessel) = HLEMap(vessel) += atom
          else HLEMap(vessel) = scala.collection.mutable.Set(atom)
        }
      case "loitering" | "stopped" | "lowSpeed" | "sailing" =>
        data foreach { x =>
          val s = x.split("\\|")
          val (startTime, endTime, vessel) = (s(3).toInt, s(4).toInt - 1, s(1))
          val atom = s"""holdsAt($hle("$vessel"),interval("$startTime","$endTime"))"""
          if (HLEMap.contains(vessel)) HLEMap(vessel) = HLEMap(vessel) += atom
          else HLEMap(vessel) = scala.collection.mutable.Set(atom)
        }
      case "rendezVous" =>
        data foreach { x =>
          val s = x.split("\\|")
          val (startTime, endTime, vessel1, vessel2)  = (s(4).toInt, s(5).toInt - 1, s(1), s(0))
          val atom = s"""holdsAt($hle("$vessel1","$vessel2"),interval("$startTime","$endTime"))"""
          if (HLEMap.contains(vessel1)) HLEMap(vessel1) = HLEMap(vessel1) += atom
          else HLEMap(vessel1) = scala.collection.mutable.Set(atom)
          if (HLEMap.contains(vessel2)) HLEMap(vessel2) = HLEMap(vessel2) += atom
          else HLEMap(vessel2) = scala.collection.mutable.Set(atom)
        }
      }
    }




}

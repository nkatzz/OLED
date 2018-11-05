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

package datautils.maritime_data

import scala.io.Source
import java.io._

import com.mongodb.casbah.Imports.MongoCollection
import com.mongodb.casbah.commons.MongoDBObject
import datautils.maritime_data.Structures.{BDEntry, Entry, HLE, LLE}

import scala.collection.mutable.ListBuffer

/**
  * Created by nkatz on 6/22/17.
  */


/**
  *
  * This is a (half-finished) version that tries to pre-process the data with in-memory maps
  * THIS DOESN'T WORK (GOT AN OUT OF HEAP ERROR AFTER A LOOOONG TIME OF PROCESSING LLES)
  *
  * */


object ConvertTime {

  // This stores the data with time as the key
  var dbEntries = scala.collection.mutable.Map[Int, BDEntry]()

  val path = "/home/nkatz/dev/maritime/nkatz_brest_0.6/1-core"

  val datasetFile = path+"/dataset.txt"
  val speedLimitsFile = path+"/static_data/all_areas/areas_speed_limits.csv"

  val closeToPortsFile = new File(path+"/recognition/close_to_ports.csv") // this has a different schema than the other hles
  val highSpeedFile = new File(path+"/recognition/highSpeedIn.csv")
  val loiteringFile = new File(path+"/recognition/loitering.csv")
  val lowSpeedFile = new File(path+"/recognition/lowSpeed.csv")
  val sailingFile = new File(path+"/recognition/sailing.csv")
  val stoppedFile = new File(path+"/recognition/stopped.csv")
  val withinAreaFile = new File(path+"/recognition/withinArea.csv")

  //val hleFiles = Vector(closeToPortsFile, highSpeedFile, loiteringFile, lowSpeedFile, sailingFile, stoppedFile, withinAreaFile)

  val hleFiles = Vector(highSpeedFile)

  def main(args: Array[String]) = {

    val tt = utils.Utils.time {
      println("Creating times map")
      val times = getAllTimes()
      // The map of unix times -> regular numbers
      val timesMap = (getAllTimes() zip (0 to times.length)).toMap

      println("Processing LLEs")
      //val lles = Source.fromFile(datasetFile).getLines.filter(x => !x.contains("HoldsFor") && !x.contains("coord")).foreach(x => handleLLEs(x, timesMap))

      val lles = Source.fromFile(datasetFile).getLines.filter(x => !x.contains("HoldsFor") && !x.contains("coord")).map(x => handleLLEs(x, timesMap))

      println("Grouping lles")
      val llesGrouped = lles.toList.groupBy(x => x.time)


      llesGrouped.foreach(x => println(x))

      //println("Processing High Speed")
      //similarHLEstoPredicateForm(highSpeedFile.getCanonicalPath, timesMap)
    }
    println(tt._2)
  }

  def updateDBEntriesMap(entry: Entry) = {
    entry match {
      case x: LLE =>
        if (dbEntries.contains(x.time)) {
          val prev = dbEntries.getOrElse(x.time, throw new RuntimeException(s"Key ${x.time} not found in dbEntries map"))
          val newEntry = BDEntry(x.time, (List() ++ prev.lles) :+ x.atom, prev.hles, (List() ++ prev.vessels) :+ x.vessel, (List() ++ prev.areas) :+ x.area)
          dbEntries(x.time) = newEntry
        } else {
          dbEntries(x.time) = BDEntry(x.time, List(x.atom), Nil, List(x.vessel), List(x.area))
        }
      case x: HLE =>
        if (dbEntries.contains(x.time)) {
          val prev = dbEntries.getOrElse(x.time, throw new RuntimeException(s"Key ${x.time} not found in dbEntries map"))
          val newEntry = BDEntry(x.time, prev.lles, (List() ++ prev.hles) :+ x.atom, List() ++ prev.vessels ++ x.vessels, (List() ++ prev.areas) :+ x.area)
          dbEntries(x.time) = newEntry
          println(dbEntries(x.time))
        } else {
          // If this happens something is wrong
          throw new RuntimeException(s"Time key for atom ${x.atom} is missing from the entries map.")
        }
      case _ =>
    }
  }

  def getAllTimes() = {
    val lleTimes = Source.fromFile(datasetFile).getLines.map(x => getTimeLLEs(x)).filter(_ != "None").toVector.distinct
    val hleTimes = hleFiles.foldLeft(Vector[String]()){ (x, y) =>
      val data = Source.fromFile(y).getLines.flatMap(x => getTimeHLEs(x)).toVector.distinct
      x ++ data
    }
    (lleTimes ++ hleTimes).distinct.map(_.toInt).sorted
  }

  /* Get the time stamp from an LLE line. Proximity is declared with HoldsFor
 (and we don't need proximity), so we don't use it.*/
  def getTimeLLEs(dataLine: String) = {
    // "HoldsFor" is used for vessel proximity we won't use it now.
    if (!dataLine.contains("HoldsFor")) {
      val info = dataLine.split("HappensAt")(1)
      val _info = info.split("\\]")
      _info(1).trim
    } else {
      "None"
    }
  }

  def getTimeHLEs(dataLine: String) = {
    try {
      if (dataLine.contains("not_near")) { // this is for close_to_ports that has a different schema
        Vector(dataLine.split("\\|")(0))
      } else if (dataLine.contains("highSpeedIn")){
        val split = dataLine.split("\\|")
        Vector(split(4), split(5))
      } else {
        val split = dataLine.split("\\|")
        Vector(split(3), split(4))
      }
    } catch {
      case e: ArrayIndexOutOfBoundsException =>
        println(dataLine)
        Vector("0")
    }
  }



  /*
 * Handles: highSpeedIn withinArea that have the same schema
 *
 * */
  def areaHLEsToPredicateForm(path: String, timesMap: Map[Int, Int]) = {
    val reversedMap = timesMap.map{ case (k, v) => (v, k)  }
    val data = Source.fromFile(path).getLines
    data.foldLeft(List[HLE]()) { (accum, x) =>
      //println()
      val s = x.split("\\|")
      val startTime = timesMap(s(4).toInt)
      val endTime = timesMap(s(5).toInt)
      val hle = s(0)
      val vessel = s(1)
      val area = s(2)

      var count = startTime
      var accum_ = ListBuffer[Int]()
      while(count <= endTime) {
        if (reversedMap.contains(count)) {
          accum_ += count
        }
        count +=1
      }
      val intermediateTimes = accum_
      val intermediateHles = intermediateTimes.map(time => HLE(hle, s"""holdsAt($hle("$vessel","$area"),"$time")""", time, List(vessel), area))
      accum ++ intermediateHles.toList
    }
  }


  /*
  *
  * Handles: loitering, low-speed, sailing, stopped (they have the same schema)
  *
  *
  * */
  def similarHLEstoPredicateForm(path: String, timesMap: Map[Int, Int]) = {
    val reversedMap = timesMap.map{ case (k, v) => (v, k)  }
    val data = Source.fromFile(path).getLines
    data.foreach{ x =>
      val s = x.split("\\|")
      val startTime = timesMap(s(3).toInt)
      val endTime = timesMap(s(4).toInt)
      val hle = s(0)
      val vessel = s(1)

      var count = startTime
      var accum_ = ListBuffer[Int]()
      while(count <= endTime) {
        if (reversedMap.contains(count)) {
          accum_ += count
        }
        count +=1
      }
      val intermediateTimes = accum_
      val intermediateHles = intermediateTimes.map(time => HLE(hle, s"""holdsAt($hle("$vessel"),"$time")""", time, List(vessel), "None"))
      intermediateHles.foreach(x => updateDBEntriesMap(x))
    }
  }



  /* Convert a data point to predicate form. This does not work with proximity for now.
   * This returns the predicate itself, the vessel and the area (if an area is involved).
   *
   * */
  def handleLLEs(x: String, timesMap: Map[Int, Int]) = {
    var area = "None"
    var predicate = "None"
    val info = x.split("HappensAt")(1)
    val _info = info.split("\\]")
    val time = timesMap(_info(1).trim.toInt).toString
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
    val entry = LLE(lle, predicate, time.toInt, vessel, area)
    //updateDBEntriesMap(entry)
    entry
  }






}

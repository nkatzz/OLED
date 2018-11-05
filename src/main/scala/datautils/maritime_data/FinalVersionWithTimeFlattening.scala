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

import java.io.File

import scala.io.Source
import com.mongodb.casbah.MongoClient
import com.mongodb.casbah.Imports._
import com.typesafe.scalalogging.LazyLogging

import scala.collection.mutable.ListBuffer


/**
  * Created by nkatz on 6/23/17.
  */


/**
  * HERE WE TRY TO STORE EVERYTHING IN A MONGO DB AS IS. TRAINING EXAMPLES WILL
  * BE GENERATED AT LEARNING TIME WITH PROPER QUERIES THAN FETCH THINGS WITH THE
  * SAME TIME STAMP.
  *
  * PERHAPS THIS WILL TAKE A BIT MORE TIME THAN HAVING ALL PRE-PROCESSED AND INPLACE,
  * BUT I GET OUT OF HEAP ERRORS WHEN I TRY TO PRE-PROCESS THE DATA WITH IN-MEMORY MAPS.
  *
  * */

object FinalVersionWithTimeFlattening extends LazyLogging {

  val path = "/home/nkatz/dev/maritime/nkatz_brest/1-core"

  private val datasetFile = path+"/dataset.txt"
  private val speedLimitsFile = path+"/static_data/all_areas/areas_speed_limits.csv"

  /*
  private val closeToPortsFile = new File(path+"/recognition/close_to_ports.csv") // this has a different schema than the other hles
  private val highSpeedFile = new File(path+"/recognition/highSpeedIn-no-infs.csv")
  private val loiteringFile = new File(path+"/recognition/loitering-no-infs.csv")
  private val lowSpeedFile = new File(path+"/recognition/lowSpeed-no-infs.csv")
  private val sailingFile = new File(path+"/recognition/sailing-no-infs.csv")
  private val stoppedFile = new File(path+"/recognition/stopped-no-infs.csv")
  private val withinAreaFile = new File(path+"/recognition/withinArea-no-infs.csv")
  private val rendezVousFile = new File(path+"/recognition/rendezVouz-no-infs.csv")
  */
  private val closeToPortsFile = new File(path+"/recognition/close_to_ports.csv") // this has a different schema than the other hles
  private val highSpeedFile = new File(path+"/recognition/highSpeedIn.csv")
  private val loiteringFile = new File(path+"/recognition/loitering.csv")
  private val lowSpeedFile = new File(path+"/recognition/lowSpeed.csv")
  private val sailingFile = new File(path+"/recognition/sailing.csv")
  private val stoppedFile = new File(path+"/recognition/stopped.csv")
  private val withinAreaFile = new File(path+"/recognition/withinArea.csv")
  private val rendezVousFile = new File(path+"/recognition/rendezVouz.csv")



  /* withinArea IS MISSING ANNOTATION */

  val hleFiles = Vector(highSpeedFile, loiteringFile, lowSpeedFile, sailingFile, stoppedFile, rendezVousFile)

  //private val hleFiles = Vector(highSpeedFile)


  def main(args: Array[String]) = {

    val dbName = "maritime-brest"
    val mongoClient = MongoClient()

    //mongoClient.dropDatabase(dbName)

    println("Creating times map")
    val times = getAllTimes()
    // The map of unix times -> regular numbers
    //val timesMap = (getAllTimes() zip (0 to times.length)).toMap

    // TEST: Lest's keep it the same
    val timesMap = (times zip times).toMap


    /*This takes too much time, just preserve the collection until you debug the piece of shit*/
    //println("Processing LLEs")
    //LLEsToMongo(mongoClient(dbName)("lles"))



    println("Processing stopped")
    val stoppedCol = mongoClient(dbName)("stopped")
    stoppedCol.dropCollection()
    similarHLEsToMongo(stoppedFile.getCanonicalPath, timesMap, stoppedCol)


    println("Processing close to ports")
    val portsCol = mongoClient(dbName)("ports")
    portsCol.dropCollection()
    portsToMongo(portsCol)


    println("Processing proximity")
    val proxCol = mongoClient(dbName)("proximity")
    proxCol.dropCollection()
    proximityToMongo(timesMap, proxCol)


    println("Processing High Speed")
    val highSpeedCol = mongoClient(dbName)("high_speed")
    highSpeedCol.dropCollection()
    AreaHLEsToMongo(highSpeedFile.getCanonicalPath, timesMap, highSpeedCol)



    println("Processing sailing")
    val sailingCol = mongoClient(dbName)("sailing")
    sailingCol.dropCollection()
    similarHLEsToMongo(sailingFile.getCanonicalPath, timesMap, sailingCol)

    println("Processing low speed")
    val lowSpeedCol = mongoClient(dbName)("low-speed")
    lowSpeedCol.dropCollection()
    similarHLEsToMongo(lowSpeedFile.getCanonicalPath, timesMap, lowSpeedCol)

    println("Processing loitering")
    val loiteringCol = mongoClient(dbName)("loitering")
    loiteringCol.dropCollection()
    similarHLEsToMongo(loiteringFile.getCanonicalPath, timesMap, loiteringCol)

    println("Processing rendezvous")
    val rendezvousCol = mongoClient(dbName)("rendezvous")
    rendezvousCol.dropCollection()
    rendezVousToMongo(rendezVousFile.getCanonicalPath, timesMap, rendezvousCol)

    //println("Processing within area")
    //AreaHLEsToMongo(withinAreaFile.getCanonicalPath, timesMap, mongoClient(dbName)("within_area"))

    println("Processing speed limits")
    speedLimitsToMongo(speedLimitsFile, mongoClient(dbName)("speed_limits"))

    println("Done!")

  }


  private def portsToMongo(collection: MongoCollection) = {
    val data = Source.fromFile(closeToPortsFile).getLines
    data foreach { x =>
      val s = x.split("\\|")
      val time = s(0).toInt
      val vessel = s(1)
      val atom = s"""notCloseToPorts("$vessel","${time.toString}")"""
      val entry = MongoDBObject("time" -> time) ++ ("atom" -> atom) ++ ("vessel" -> vessel)
      collection.insert(entry)
    }
  }

  def getAllTimes() = {
    val lleTimes = Source.fromFile(datasetFile).getLines.map(x => getTimeLLEs(x)).filter(_ != "None").toVector.distinct
    /*
    val hleTimes = hleFiles.foldLeft(Vector[String]()){ (x, y) =>
      val data = Source.fromFile(y).getLines.filter(x => !x.contains("inf")).flatMap(x => getTimeHLEs(x)).toVector.distinct
      x ++ data
    }
    */
    val procimityTimes = getProximityTimes()
    //(lleTimes ++ hleTimes ++ procimityTimes).distinct.map(_.toInt).sorted
    (lleTimes ++ procimityTimes).distinct.map(_.toInt).sorted
  }

  /* Get the time stamp from an LLE line. Proximity is declared with HoldsFor
 (and we don't need proximity), so we don't use it.*/
  private def getTimeLLEs(dataLine: String) = {
    // "HoldsFor" is used for vessel proximity we won't use it now.
    if (!dataLine.contains("HoldsFor")) {
      val info = dataLine.split("HappensAt")(1)
      val _info = info.split("\\]")
      _info(1).trim
    } else {
      "None"
    }
  }



  private def getProximityTimes() = {
    val lines = Source.fromFile(datasetFile).getLines.filter(x => x.contains("proximity"))
    lines.foldLeft(Vector[String]()){ (accum, x) =>
      val s = x.split("=")(0).split(" ")
      val z = x.split("=")(1).trim().split(" ")(1).split("\\(")(1).split("\\)")(0).split("-")
      val startTime = z(0)
      val endTime = z(1)
      accum ++ List(startTime, endTime)
    }
  }

  private def getTimeHLEs(dataLine: String) = {
    try {
      if (dataLine.contains("not_near")) { // this is for close_to_ports that has a different schema
        Vector(dataLine.split("\\|")(0))
      } else if (dataLine.contains("highSpeedIn") || dataLine.contains("withinArea")){
        val split = dataLine.split("\\|")
        Vector(split(4), split(5))
      } else if (dataLine.contains("loitering") || dataLine.contains("sailing") || dataLine.contains("stopped") || dataLine.contains("lowSpeed")) {
        val split = dataLine.split("\\|")
        Vector(split(3), split(4))
      } else { //rendezvous
        val split = dataLine.split("\\|")
        Vector(split(4), split(5))
      }
    } catch {
      case e: ArrayIndexOutOfBoundsException =>
        println(dataLine)
        Vector("0")
    }
  }


  /* Convert a data point to predicate form. This does not work with proximity for now.
   * This returns the predicate itself, the vessel and the area (if an area is involved).
   *
   * */
  def LLEsToMongo(collection: MongoCollection) = {
    val data = Source.fromFile(datasetFile).getLines//.filter(x => !x.contains("HoldsFor") && !x.contains("coord"))
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
        val entry = MongoDBObject("time" -> time.toInt) ++ ("lle" -> predicate) ++ ("hle" -> "None") ++ ("vessels" -> List(vessel)) ++ ("areas" -> List(area))
        collection.insert(entry)
      }
    }
  }




  /*
   * Handles: highSpeedIn, withinArea that have the same schema
   *
   * */
  def AreaHLEsToMongo(path: String, map: Map[Int, Int], collection: MongoCollection) = {

    val data = Source.fromFile(path).getLines.filter(x => !x.contains("inf"))
    data.foreach { x =>
      val s = x.split("\\|")
      //val startTime = map(s(4).toInt)
      //val endTime = map(s(5).toInt) - 1
      val startTime = s(4).toInt
      val endTime = s(5).toInt - 1
      val hle = s(0)
      val vessel = s(1)
      val area = s(2)

      val intermediate = List(startTime) ++ (for (x <- startTime to endTime if map.contains(x)) yield x).toList :+ endTime
      val atom = s"""holdsAt($hle("$vessel","$area"),"ReplaceThisByActualTime")"""
      val entry = MongoDBObject("interval" -> intermediate.toList) ++ ("hle" -> atom) ++ ("vessels" -> List(vessel)) ++ ("areas" -> List(area))
      collection.insert(entry)
    }
  }




  def similarHLEsToMongo(path: String, timesMap: Map[Int, Int], collection: MongoCollection) = {

    val data = Source.fromFile(path).getLines.filter(x => !x.contains("inf"))
    data foreach { x =>
      val s = x.split("\\|")
      //val startTime = timesMap(s(3).toInt)
      //val endTime = timesMap(s(4).toInt) - 1
      val startTime = s(3).toInt
      val endTime = s(4).toInt - 1
      val hle = s(0)
      val vessel = s(1)
      val intermediate = List(startTime) ++ (for (x <- startTime to endTime if timesMap.contains(x)) yield x).toList :+ endTime
      val atom = s"""holdsAt($hle("$vessel"),"ReplaceThisByActualTime")"""
      val entry = MongoDBObject("interval" -> intermediate) ++ ("hle" -> atom) ++ ("vessels" -> List(vessel))
      try {
        collection.insert(entry)
      } catch {
        case _: org.bson.BsonSerializationException => println(startTime)
      }

    }
  }

  def rendezVousToMongo(path: String, timesMap: Map[Int, Int], collection: MongoCollection) = {
    val data = Source.fromFile(path).getLines.filter(x => !x.contains("inf"))
    data foreach { x =>
      val s = x.split("\\|")
      //val startTime = timesMap(s(4).toInt)
      //val endTime = timesMap(s(5).toInt) - 1
      val startTime = s(4).toInt
      val endTime = s(5).toInt - 1
      val hle = s(0)
      val vessel1 = s(1)
      val vessel2 = s(0)
      val intermediate = List(startTime) ++ (for (x <- startTime to endTime if timesMap.contains(x)) yield x).toList :+ endTime
      val atom = s"""holdsAt($hle("$vessel1","$vessel2"),"ReplaceThisByActualTime")"""
      val entry = MongoDBObject("interval" -> intermediate) ++ ("hle" -> atom) ++ ("vessels" -> List(vessel1,vessel2))
      collection.insert(entry)
    }
  }

  def proximityToMongo(timesMap: Map[Int, Int], collection: MongoCollection) = {
    val lines = Source.fromFile(datasetFile).getLines.filter(x => x.contains("proximity"))
    lines foreach { x =>
      val s = x.split("=")(0).split(" ")
      val vessel1 = s(2)
      val vessel2 = s(3)
      val z = x.split("=")(1).trim().split(" ")(1).split("\\(")(1).split("\\)")(0).split("-")
      //val startTime = timesMap(z(0).toInt)
      //val endTime = timesMap(z(1).toInt) - 1
      val startTime = z(0).toInt
      val endTime = z(1).toInt - 1
      val intermediate = List(startTime) ++ (for (x <- startTime to endTime if timesMap.contains(x)) yield x).toList :+ endTime
      val atom = s"""close("$vessel1","$vessel2"),"ReplaceThisByActualTime")"""
      val entry = MongoDBObject("interval" -> intermediate) ++ ("hle" -> atom) ++ ("vessels" -> List(vessel1,vessel2))
      collection.insert(entry)
    }
  }

  def speedLimitsToMongo(path: String, collection: MongoCollection) = {
    val data = Source.fromFile(path).getLines
    data foreach { x =>
      val s = x.split("\\|")
      val area = s(0)
      val limit = s(1)
      val atom = s"""speedLimit("$area","$limit")"""
      val entry = MongoDBObject("area" -> area) ++ ("limit" -> limit) ++ ("atom" -> atom)
      collection.insert(entry)
    }
  }







}

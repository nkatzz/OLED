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

import com.mongodb.casbah.Imports.MongoDBObject
import com.mongodb.casbah.Imports._
import com.mongodb.casbah.MongoClient
import scala.io.Source

/**
  * Created by nkatz on 7/9/17.
  */

/**
  *
  * IMPORTANT:
  *
  * TO CREATE THIS MAP YOY NEED LOTS OF MEMORY.
  *
  * GIVE SOMETHING LIKE -Xmx4G TO THE JVM.
  *
  *
  * THIS CLASS IS TO BE USED ONLY ONCE PER LLE FILE (SEE EXAMPLE BELOW) TO GET THE LLES INTO MONGO.
  *
  * */


object LLEsToMongo {

  // The key is time
  var LLEMap = scala.collection.mutable.Map[Int, (scala.collection.mutable.Set[String], scala.collection.mutable.Set[String], scala.collection.mutable.Set[String])]()


  def main(args: Array[String]) = {
    val dataOpts1 = new MaritimeDataOptions(
      llePath = "/home/nkatz/dev/maritime/brest-data/datasets-my-split/dataset88.txt",
      hlePath = "/home/nkatz/dev/maritime/brest-data/recognition/21/lowSpeed-no-infs.csv",
      speedLimitsPath = "/home/nkatz/dev/maritime/brest-data/areas_speed_limits.csv",
      closeToPortsPath = "/home/nkatz/dev/maritime/brest-data/recognition/88/close_to_ports.csv",
      chunkSize = 10,
      targetConcept = "lowSpeed",
      limit = 100000.0,
      trainingMode = false)

    populateLLEsMap(dataOpts1.llePath)

    llesToMongo("brest-8-8")
  }

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


  /* Call this method to insert the LLEs from a dataset to mongo. This is supposed to be called once. */
  def llesToMongo(dbName: String) = {
    val mongoClient = MongoClient()
    mongoClient(dbName).dropDatabase()
    val collection = mongoClient(dbName)("examples")
    collection.createIndex(MongoDBObject("time" -> 1))
    val times = LLEMap.keySet.toVector.sorted
    var counter = 0
    times.foreach { time =>
      val record = LLEMap(time)
      val (lleAtoms, vessels, areas) = (record._1, record._2, record._3)
      val entry = MongoDBObject("lles" -> lleAtoms) ++ ("vessels" -> vessels) ++ ("areas" -> areas) ++ ("time" -> time) ++ ("index" -> counter)
      collection.insert(entry)
      println(s"inserting $counter to $dbName")
      counter += 1
    }
  }

}

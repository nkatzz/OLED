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

package experiments.datautils.maritime_data.new_attempt

import akka.actor.{Actor, ActorSystem, PoisonPill, Props}
import com.mongodb.casbah.Imports._
import com.mongodb.casbah.{MongoClient, MongoCollection}

import scala.io.Source
import scala.collection.immutable.HashSet

/**
  * Created by nkatz on 7/4/17.
  */

object Runner {

  def main(args: Array[String]) = {
    println("Creating times hash set")
    val times = HashSet() ++ getAllTimes()
    val system = ActorSystem("MatitimePreprocessing")
    system.actorOf(Props(new Master(times)), name = "Master") ! "go"
  }

  private val path = "/home/nkatz/dev/maritime/nkatz_brest/1-core"
  private val datasetFile = path + "/dataset.txt" // The LLEs file

  def getAllTimes() = {
    val lleTimes = Source.fromFile(datasetFile).getLines.map(x => getTimeLLEs(x)).filter(_ != "None").toVector.distinct
    val procimityTimes = getProximityTimes()
    (lleTimes ++ procimityTimes).distinct.map(_.toInt).sorted
  }

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

}

class Master(times: HashSet[Int]) extends Actor {

  val path = "/home/nkatz/dev/maritime/nkatz_brest/1-core"
  private val datasetFile = path + "/dataset.txt" // The LLEs file
  private val speedLimitsFile = path + "/static_data/all_areas/areas_speed_limits.csv"
  private val closeToPortsFile = path + "/recognition/close_to_ports.csv" // this has a different schema than the other hles
  private val highSpeedFile = path + "/recognition/highSpeedIn-no-infs.csv"
  private val loiteringFile = path + "/recognition/loitering-no-infs.csv"
  private val lowSpeedFile = path + "/recognition/lowSpeed-no-infs.csv"
  private val sailingFile = path + "/recognition/sailing-no-infs.csv"
  private val stoppedFile = path + "/recognition/stopped-no-infs.csv"
  private val withinAreaFile = path + "/recognition/withinArea-no-infs.csv"
  private val rendezVousFile = path + "/recognition/rendezVouz-no-infs.csv"

  val dbName = "maritime-brest"
  val mongoClient = MongoClient()
  mongoClient.dropDatabase(dbName)

  var counter = 9 // THIS MUST BE ADAPTED IF THE JOBS CHANGE

  def receive = {
    case "go" =>
      println("Getting times")
      // withinArea & loitering are missing
      context.actorOf(Props(new LLEsToMongo(datasetFile, mongoClient(dbName)("lles"))), name = "lles-actor") ! "go"
      context.actorOf(Props(new PortsToMongo(closeToPortsFile, mongoClient(dbName)("ports"))), name = "ports-actor") ! "go"
      context.actorOf(Props(new ProximityToMongo(datasetFile, this.times, mongoClient(dbName)("proximity"))), name = "proximity-actor") ! "go"
      context.actorOf(Props(new SpeedLimitsToMongo(speedLimitsFile, mongoClient(dbName)("speed_limits"))), name = "speed-limit-actor") ! "go"
      context.actorOf(Props(new HighLevelEventToMongo(highSpeedFile, this.times, "highSpeedIn", mongoClient(dbName)("highSpeedIn"))), name = "high-speed-actor") ! "go"
      context.actorOf(Props(new HighLevelEventToMongo(rendezVousFile, this.times, "rendezVous", mongoClient(dbName)("rendezVous"))), name = "loitering-actor") ! "go"
      context.actorOf(Props(new HighLevelEventToMongo(lowSpeedFile, this.times, "lowSpeed", mongoClient(dbName)("lowSpeed"))), name = "low-speed-actor") ! "go"
      context.actorOf(Props(new HighLevelEventToMongo(sailingFile, this.times, "sailing", mongoClient(dbName)("sailing"))), name = "sailing-actor") ! "go"
      context.actorOf(Props(new HighLevelEventToMongo(stoppedFile, this.times, "stopped", mongoClient(dbName)("stopped"))), name = "stopped-actor") ! "go"

    case "done" =>
      this.counter -= 1
      println(s"$counter jobs remaining")
      if (this.counter == 0) {
        context.system.terminate()
      }
  }

}

class HighLevelEventToMongo(val dataPath: String, val times: HashSet[Int], val hle: String, val collection: MongoCollection) extends Actor {

  def receive = {
    case "go" =>
      println(s"Starting with ${this.hle}")
      HLEsToMongo(this.dataPath, this.times, this.collection)
    case "done" =>
      println(s"Finished with ${this.hle}")
      context.parent ! "done"
      self ! PoisonPill
  }

  def HLEsToMongo(path: String, times: HashSet[Int], collection: MongoCollection) = {

    //collection.createIndex(MongoDBObject("vessel"))

    val data = Source.fromFile(path).getLines.filter(x => !x.contains("inf"))

    this.hle match {
      case "highSpeedIn" | "withinArea" =>

        /*
        def insertAll(startTime: Int, endTime: Int, vessel: String, area: String) = {
          insert(startTime, vessel, area)
          val interval  = for (x <- startTime to endTime if this.times.contains(x)) yield x
          interval foreach { time => insert(time, vessel, area) }
        }

        def insert(time: Int, vessel: String, area: String) = {
          val atom = s"""holdsAt(${this.hle}("$vessel","$area"),"$time")"""
          val entry = MongoDBObject("time" -> time) ++ ("atom" -> atom)
          collection.insert(entry)
        }
        */

        data foreach { x =>
          val s = x.split("\\|")
          val startTime = s(4).toInt
          val endTime = s(5).toInt - 1
          val vessel = s(1)
          val area = s(2)
          //insertAll(startTime, endTime, vessel, area)
          val atom = s"""holdsAt(${this.hle}("$vessel","$area"),interval("$startTime","$endTime"))"""
          val entry = MongoDBObject("vessel" -> vessel) ++ ("atom" -> atom)
          collection.insert(entry)
        }
        context.self ! "done"

      case "loitering" | "stopped" | "lowSpeed" | "sailing" =>

        /*
        def insertAll(startTime: Int, endTime: Int, vessel: String) = {
          insert(startTime, vessel)
          val interval  = for (x <- startTime to endTime if times.contains(x)) yield x
          interval foreach { time => insert(time, vessel) }
        }

        def insert(time: Int, vessel: String) = {
          val atom = s"""holdsAt(${this.hle}("$vessel"),"$time")"""
          val entry = MongoDBObject("time" -> time) ++ ("atom" -> atom)
          collection.insert(entry)
        }
        */

        data foreach { x =>
          val s = x.split("\\|")
          val startTime = s(3).toInt
          val endTime = s(4).toInt - 1
          val vessel = s(1)
          //insertAll(startTime, endTime, vessel)
          val atom = s"""holdsAt(${this.hle}("$vessel"),interval("$startTime","$endTime"))"""
          val entry = MongoDBObject("vessel" -> vessel) ++ ("atom" -> atom)
          collection.insert(entry)
        }
        context.self ! "done"

      case "rendezVous" =>

        /*
        def insertAll(startTime: Int, endTime: Int, vessel1: String, vessel2: String) = {
          insert(startTime, vessel1, vessel2)
          val interval  = for (x <- startTime to endTime if times.contains(x)) yield x
          interval foreach { time => insert(time, vessel1, vessel2) }
        }

        def insert(time: Int, vessel1: String, vessel2: String) = {
          val atom = s"""holdsAt(${this.hle}("$vessel1","$vessel2"),"$time")"""
          val entry = MongoDBObject("time" -> time) ++ ("atom" -> atom)
          collection.insert(entry)
        }
        */

        data foreach { x =>
          val s = x.split("\\|")
          val startTime = s(4).toInt
          val endTime = s(5).toInt - 1
          val vessel1 = s(1)
          val vessel2 = s(0)
          //insertAll(startTime, endTime, vessel1, vessel2)
          val atom = s"""holdsAt(${this.hle}("$vessel1","$vessel2"),interval("$startTime","$endTime"))"""
          // Insert the atom twice, once for each vessel, so that we can use both vessels as keys to
          // fetch the proximity atom at learning time.
          val entry1 = MongoDBObject("vessel" -> vessel1) ++ ("atom" -> atom)
          val entry2 = MongoDBObject("vessel" -> vessel2) ++ ("atom" -> atom)
          collection.insert(entry1)
          collection.insert(entry2)
        }
        context.self ! "done"
    }
  }

}

class LLEsToMongo(val dataPath: String, val collection: MongoCollection) extends Actor {

  def receive = {
    case "go" =>
      println("Starting with LLEs")
      LLEsToMongo(collection)
    case "done" =>
      println("Finished with LLEs")
      context.parent ! "done"
      self ! PoisonPill
  }

  def LLEsToMongo(collection: MongoCollection) = {

    //collection.createIndex(MongoDBObject("time") -> 1)

    val data = Source.fromFile(dataPath).getLines //.filter(x => !x.contains("HoldsFor") && !x.contains("coord"))
    while (data.hasNext) {
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
    context.self ! "done"
  }
}

class SpeedLimitsToMongo(val path: String, val collection: MongoCollection) extends Actor {

  def receive = {
    case "go" =>
      println("Starting with speed limits")
      toMongo(this.path, this.collection)
    case "done" =>
      println("Finished with speed limits")
      context.parent ! "done"
      self ! PoisonPill
  }

  def toMongo(path: String, collection: MongoCollection) = {

    //collection.createIndex(MongoDBObject("area"))

    val data = Source.fromFile(path).getLines
    data foreach { x =>
      val s = x.split("\\|")
      val area = s(0)
      val limit = s(1)
      val atom = s"""speedLimit("$area","$limit")"""
      val entry = MongoDBObject("area" -> area) ++ ("atom" -> atom)
      collection.insert(entry)
    }
    context.self ! "done"
  }

}

class ProximityToMongo(val path: String, val times: HashSet[Int], val collection: MongoCollection) extends Actor {

  def receive = {
    case "go" =>
      println("Starting with proximity")
      toMongo(this.path, this.times, this.collection)
    case "done" =>
      println("Finished with proximity")
      context.parent ! "done"
      self ! PoisonPill
  }

  def toMongo(path: String, times: HashSet[Int], collection: MongoCollection) = {

    //collection.createIndex(MongoDBObject("vessel"))

    // "Unfolding" the annotation intervals generates tenths of GBs of data.
    // No obvious way to handle that and no need to do so (hopefully...)
    /*
    def insertAll(startTime: Int, endTime: Int, vessel1: String, vessel2: String) = {
      insert(startTime, vessel1, vessel2)
      val interval  = for (x <- startTime to endTime if times.contains(x)) yield x
      interval foreach { time => insert(time, vessel1, vessel2) }
    }

    def insert(time: Int, vessel1: String, vessel2: String) = {
      val atom = s"""close("$vessel1","$vessel2"),"$time")"""
      val entry = MongoDBObject("time" -> time) ++ ("atom" -> atom)
      collection.insert(entry)
    }
    */

    val lines = Source.fromFile(path).getLines.filter(x => x.contains("proximity"))
    lines foreach { x =>
      val s = x.split("=")(0).split(" ")
      val vessel1 = s(2)
      val vessel2 = s(3)
      val z = x.split("=")(1).trim().split(" ")(1).split("\\(")(1).split("\\)")(0).split("-")
      val startTime = z(0).toInt
      val endTime = z(1).toInt - 1
      val atom = s"""close("$vessel1","$vessel2",interval("$startTime","$endTime"))"""
      // Insert the atom twice, once for each vessel, so that we can use both vessels as keys to
      // fetch the proximity atom at learning time.
      val entry1 = MongoDBObject("vessel" -> vessel1) ++ ("atom" -> atom)
      val entry2 = MongoDBObject("vessel" -> vessel2) ++ ("atom" -> atom)
      collection.insert(entry1)
      collection.insert(entry2)
    }
    self ! "done"
  }

}

class PortsToMongo(val path: String, val collection: MongoCollection) extends Actor {

  def receive = {
    case "go" =>
      println("Starting with ports")
      toMongo(this.path, this.collection)
    case "done" =>
      println("Finished with ports")
      context.parent ! "done"
      self ! PoisonPill
  }

  def toMongo(path: String, collection: MongoCollection) = {

    //collection.createIndex(MongoDBObject("vessel"))

    val data = Source.fromFile(path).getLines
    data foreach { x =>
      val s = x.split("\\|")
      val time = s(0).toInt
      val vessel = s(1)
      val atom = s"""notCloseToPorts("$vessel","${time.toString}")"""
      val entry = MongoDBObject("time" -> time) ++ ("atom" -> atom) ++ ("vessel" -> vessel)
      collection.insert(entry)
    }
    context.self ! "done"
  }
}


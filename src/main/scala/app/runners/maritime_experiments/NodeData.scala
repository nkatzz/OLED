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

import scala.io.Source
import com.mongodb.{BasicDBList, BasicDBObject}
import com.mongodb.casbah.Imports.MongoDBObject
import com.mongodb.casbah.Imports.DBObject
import com.mongodb.casbah.Imports._
import com.mongodb.casbah.MongoClient
import logic.Examples.Example

/**
  * Created by nkatz on 7/9/17.
  */

class MaritimeDataOptions(
    val llePath: String = "",
    val db: String = "",
    val hlePath: String,
    val speedLimitsPath: String = "/home/nkatz/dev/maritime/brest-data/areas_speed_limits.csv",
    val closeToPortsPath: String,
    val chunkSize: Int = 1,
    val limit: Double = Double.PositiveInfinity.toInt,
    val targetConcept: String = "None",
    val trainingMode: Boolean = true) extends app.runutils.IOHandling.InputSource

case class VesselAnnotationAtom(atom: String, startTime: Int, endTime: Int, var hasBeenChecked: Boolean = false) {
  def getActualAtom(time: Int) = this.atom.replaceAll("ReplaceThisByActualTime", time.toString)
}

class AnnotationPerVessel(val vessel: String = "", var atoms: Vector[VesselAnnotationAtom] = Vector.empty, var currentIndex: Int = 0) {
  def updateAtoms(va: VesselAnnotationAtom) = this.atoms = this.atoms :+ va
  def updateIndex = this.currentIndex = this.currentIndex + 1
  def getCurrentAnnotationInterval = this.atoms(this.currentIndex)
  lazy val sortAtoms: Vector[VesselAnnotationAtom] = this.atoms.sortBy(atom => atom.endTime)
}

class NodeData(
    val hlePath: String,
    val llePath: String,
    val closeToPortsPath: String,
    val targetConcept: String,
    val speedLimitsMap: scala.collection.mutable.Map[String, scala.collection.mutable.Set[String]]) {

  // The key is a vessel and the value is the set of all its annotation atoms wrapped in an AnnotationPerVessel instance
  var HLEsMap = scala.collection.mutable.Map[String, AnnotationPerVessel]()
  // The key is a vessel and the value is the set of all proximity atoms for that vessel.
  var proximityMap = scala.collection.mutable.Map[String, AnnotationPerVessel]()
  // The key is time
  var portsMap = scala.collection.mutable.Map[String, scala.collection.mutable.Set[String]]()

  def updateMap(vessel: String, a: VesselAnnotationAtom, map: scala.collection.mutable.Map[String, AnnotationPerVessel]) = {
    if (map.contains(vessel)) map(vessel).updateAtoms(a)
    else map(vessel) = new AnnotationPerVessel(vessel, Vector(a))
  }

  populateHLEsMap(hlePath, targetConcept)
  populatePortsMap(closeToPortsPath)
  populateProximityMap(llePath)

  def getTrainingData(opts: MaritimeDataOptions): Iterator[Example] = {
    val mongoClient = MongoClient()
    val collection = mongoClient(opts.db)("examples")
    val times = collection.find().sort(MongoDBObject("time" -> 1)).limit(opts.limit.toInt).grouped(opts.chunkSize)
    times map { timeSlice => getDataSlice(timeSlice) }
  }

  def getTestingData(opts: MaritimeDataOptions): Iterator[Example] = {
    val mongoClient = MongoClient()
    val collection = mongoClient(opts.db)("examples")
    val times = collection.find().sort(MongoDBObject("time" -> 1)).drop(opts.limit.toInt).take(10000).grouped(opts.chunkSize)
    times map { timeSlice => getDataSlice(timeSlice) }
  }

  def getDataSlice(objects: Seq[DBObject]) = {

      def convert(o: DBObject) = {
        val obj = o.asInstanceOf[BasicDBObject]
        //val time = obj.get("time").toString
        val atoms = obj.get("lles").asInstanceOf[BasicDBList].toList.map(_.toString).toSet
        val vessels = obj.get("vessels").asInstanceOf[BasicDBList].toList.map(_.toString).toSet
        val areas = obj.get("areas").asInstanceOf[BasicDBList].toList.map(_.toString).toSet
        (atoms, vessels, areas)
      }

      def getTimes = objects.map(o => o.asInstanceOf[BasicDBObject].get("time").toString.toInt)
    val times = getTimes
    val finalExampleStartTime = times.min

    val (lleAtoms, vessels, areas) = objects.foldLeft(Set[String](), Set[String](), Set[String]()) { (accum, o) =>
      val obj = convert(o)
      (accum._1 ++ obj._1, accum._2 ++ obj._2, accum._3 ++ obj._3)
    }

    val hleAtoms = times.flatMap(t => getCurrentVesselsAnnotation(t, vessels, this.HLEsMap)).filter(x => x != "None")
    val proximityAtoms = times.flatMap(t => getCurrentVesselsAnnotation(t, vessels, this.proximityMap)).distinct.filter(x => x != "None")

    val (closeToPortsAtoms, speedLimitAtoms) = times.foldLeft(Set[String](), Set[String]()){ (accum, time) =>

      val closeToPortsAtoms_ = {
        if (this.portsMap.contains(time.toString)) this.portsMap(time.toString)
        else scala.collection.mutable.Set[String]()
      }

      val speedLimitAtoms_ = areas.flatMap { a =>
        if (speedLimitsMap.contains(a)) speedLimitsMap(a)
        else scala.collection.mutable.Set[String]()
      }.filter(p => p != "None")

      (accum._1 ++ closeToPortsAtoms_, accum._2 ++ speedLimitAtoms_)
    }

    val narrative = lleAtoms.toList ++ proximityAtoms.toList ++ closeToPortsAtoms.toList ++ speedLimitAtoms.toList
    new Example(annot = hleAtoms.toList, nar = narrative, _time = finalExampleStartTime.toString)
  }

  def isWithinInterval(i: Int, interval: (Int, Int)) = {
    i >= interval._1 && i <= interval._2
  }

  def checkInterval(time: Int, interval: VesselAnnotationAtom): String = {
    if (isWithinInterval(time.toInt, (interval.startTime, interval.endTime))) {
      interval.getActualAtom(time.toInt)
    } else {
      "None"
    }
  }

  def getCurrentVesselsAnnotation(time: Int, vessels: Set[String], map: scala.collection.mutable.Map[String, AnnotationPerVessel]) = {
    vessels.foldLeft(Set[String]()){ (accum, v) =>
      if (map.contains(v)) {
        val vesselAnnotation = map(v)
        val intervals = vesselAnnotation.atoms
        accum ++ intervals.map(i => checkInterval(time, i))
      } else {
        accum + "None"
      }
    }
  }

  def populateHLEsMap(dataPath: String, hle: String) = {
    println("Generating HLEs map")
    val data = Source.fromFile(dataPath).getLines.filter(x => !x.contains("inf"))
    hle match {
      case "highSpeedIn" | "withinArea" =>
        data foreach { x =>
          val s = x.split("\\|")
          val (startTime, endTime, vessel, area) = (s(4).toInt, s(5).toInt - 1, s(1), s(2))
          val atom = s"""holdsAt($hle("$vessel","$area"),"ReplaceThisByActualTime")"""
          val a = VesselAnnotationAtom(atom, startTime, endTime)
          updateMap(vessel, a, this.HLEsMap)
        }
      case "loitering" | "stopped" | "lowSpeed" | "sailing" =>
        data foreach { x =>
          val s = x.split("\\|")
          val (startTime, endTime, vessel) = (s(3).toInt, s(4).toInt - 1, s(1))
          val atom = s"""holdsAt($hle("$vessel"),"ReplaceThisByActualTime")"""
          val a = VesselAnnotationAtom(atom, startTime, endTime)
          updateMap(vessel, a, this.HLEsMap)
        }
      case "rendezVous" =>
        data foreach { x =>
          val s = x.split("\\|")
          val (startTime, endTime, vessel1, vessel2) = (s(4).toInt, s(5).toInt - 1, s(1), s(0))
          val atom = s"""holdsAt($hle("$vessel1","$vessel2"),"ReplaceThisByActualTime")"""
          val a = VesselAnnotationAtom(atom, startTime, endTime)
          updateMap(vessel1, a, this.HLEsMap)
          updateMap(vessel2, a, this.HLEsMap)
        }
    }
    println("Sorting the HLEs map values")
    //HLEsMap.foreach(x => x._2.atoms.sortBy(z => z.endTime))
    this.HLEsMap foreach { case (k, v) =>
      val v1 = v.atoms.sortBy(z => z.endTime)
      this.HLEsMap(k).atoms = v1
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
      val atom = s"""close("$vessel1","$vessel2","ReplaceThisByActualTime")"""
      val a = VesselAnnotationAtom(atom, startTime, endTime)
      updateMap(vessel1, a, this.proximityMap)
      updateMap(vessel2, a, this.proximityMap)
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
      if (this.portsMap.contains(time)) this.portsMap(time) = this.portsMap(time) += atom
      else this.portsMap(time) = scala.collection.mutable.Set(atom)
    }
  }

}


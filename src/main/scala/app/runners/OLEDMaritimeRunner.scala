package app.runners

import akka.actor.{ActorSystem, Props}
import app.runutils.CMDArgs
import com.mongodb.{BasicDBList, BasicDBObject}
import com.mongodb.casbah.Imports.MongoDBObject
import com.mongodb.casbah.Imports.DBObject
import com.mongodb.casbah.Imports._
import com.mongodb.casbah.MongoClient
import datautils.maritime_data.yet_another_attempt.MaritimeToMongo.{LLEMap, populateLLEsMap, populatePortsMap, populateSpeedLimitsMap, portsMap, speedLimitsMap}
import logic.Examples.Example
import oled.single_core.Master

import scala.io.Source

/**
  * Created by nkatz on 7/5/17.
  */

// highSpeedIn:
//--inpath=/home/nkatz/dev/iled/datasets/MaritimeAegean/highSpeedIn --chunksize=1 --target=highSpeedIn --minseen=100000 --prune=0.9 --ties=0.0005 --delta=0.00000005

// stopped:
//--inpath=/home/nkatz/dev/iled/datasets/MaritimeAegean/stopped --chunksize=1 --target=stopped --minseen=100000 --prune=0.9 --ties=0.0005 --delta=0.00000005


// sailing:
//--inpath=/home/nkatz/dev/iled/datasets/MaritimeAegean/sailing --chunksize=1 --target=sailing --minseen=100000 --prune=0.9 --ties=0.0005 --delta=0.00000005

// lowSpeed
//--inpath=/home/nkatz/dev/iled/datasets/MaritimeAegean/lowSpeed --chunksize=1 --target=lowSpeed --minseen=100000 --prune=0.9 --ties=0.0005 --delta=0.00000005

// To create the lles dbs (ONLY DONE ONCE, THEN DATA ARE FETCHED FROM MONGO) do this:
// 1. populateLLEsMap(dataOpts.llePath)
// 2. llesToMongo
// For example:
/*
      val dataOpts1 = new MaritimeDataOptions(
        llePath = "/home/nkatz/dev/maritime/brest-data/datasets/dataset88.txt",
        hlePath = "/home/nkatz/dev/maritime/brest-data/recognition/1/lowSpeed-no-infs.csv",
        speedLimitsPath = "/home/nkatz/dev/maritime/brest-data/areas_speed_limits.csv",
        closeToPortsPath = "/home/nkatz/dev/maritime/brest-data/recognition/1/close_to_ports.csv",
        chunkSize = 10,
        targetConcept = "lowSpeed",
        limit = 100000.0,
        trainingMode = false)

      populateLLEsMap(dataOpts1.llePath)

      llesToMongo("brest-8-8")
 */

object OLEDMaritimeRunner {


  lazy val highSpeedInDataOptionsTraining = new MaritimeDataOptions(
    llePath = "/home/nkatz/dev/maritime/brest-data/datasets/dataset1.txt",
    db = "brest-1",
    hlePath = "/home/nkatz/dev/maritime/brest-data/recognition/1/highSpeedIn.csv",
    speedLimitsPath = "/home/nkatz/dev/maritime/brest-data/areas_speed_limits.csv",
    closeToPortsPath = "/home/nkatz/dev/maritime/brest-data/recognition/1/close_to_ports.csv",
    chunkSize = 10,
    limit = 10000.0,
    targetConcept = "highSpeedIn",
    hlesMap = HLEsMap,
    proxMap = proximityMap,
    portsMap = portsMap)

  lazy val highSpeedInDataOptionsTesting = new MaritimeDataOptions(
    llePath = "/home/nkatz/dev/maritime/brest-data/datasets/dataset1.txt",
    db = "brest-1",
    hlePath = "/home/nkatz/dev/maritime/brest-data/recognition/1/highSpeedIn.csv",
    speedLimitsPath = "/home/nkatz/dev/maritime/brest-data/areas_speed_limits.csv",
    closeToPortsPath = "/home/nkatz/dev/maritime/brest-data/recognition/1/close_to_ports.csv",
    chunkSize = 100,
    limit = 10000.0,
    targetConcept = "highSpeedIn",
    hlesMap = HLEsMap,
    proxMap = proximityMap,
    portsMap = portsMap)


  lazy val stoppedDataOptions = new MaritimeDataOptions(
    llePath = "/home/nkatz/dev/maritime/brest-data/datasets/dataset1.txt",
    hlePath = "/home/nkatz/dev/maritime/brest-data/recognition/1/stopped-no-infs.csv",
    speedLimitsPath = "/home/nkatz/dev/maritime/brest-data/areas_speed_limits.csv",
    closeToPortsPath = "/home/nkatz/dev/maritime/brest-data/recognition/1/close_to_ports.csv",
    chunkSize = 10,
    targetConcept = "stopped",
    hlesMap = HLEsMap,
    proxMap = proximityMap,
    portsMap = portsMap)

  lazy val sailingDataOptions = new MaritimeDataOptions(
    llePath = "/home/nkatz/dev/maritime/brest-data/datasets/dataset1.txt",
    hlePath = "/home/nkatz/dev/maritime/brest-data/recognition/1/sailing-no-infs.csv",
    speedLimitsPath = "/home/nkatz/dev/maritime/brest-data/areas_speed_limits.csv",
    closeToPortsPath = "/home/nkatz/dev/maritime/brest-data/recognition/1/close_to_ports.csv",
    chunkSize = 10,
    targetConcept = "sailing",
    hlesMap = HLEsMap,
    proxMap = proximityMap,
    portsMap = portsMap)

  lazy val lowSpeedDataOptionsTraining = new MaritimeDataOptions(
      llePath = "/home/nkatz/dev/maritime/brest-data/datasets/dataset1.txt",
      hlePath = "/home/nkatz/dev/maritime/brest-data/recognition/1/lowSpeed-no-infs.csv",
      speedLimitsPath = "/home/nkatz/dev/maritime/brest-data/areas_speed_limits.csv",
      closeToPortsPath = "/home/nkatz/dev/maritime/brest-data/recognition/1/close_to_ports.csv",
      chunkSize = 10,
      targetConcept = "lowSpeed",
      limit = 100000.0,
      trainingMode = true,
      hlesMap = HLEsMap,
      proxMap = proximityMap,
      portsMap = portsMap)

  lazy val lowSpeedDataOptionsTesting= new MaritimeDataOptions(
    llePath = "/home/nkatz/dev/maritime/brest-data/datasets/dataset1.txt",
    hlePath = "/home/nkatz/dev/maritime/brest-data/recognition/1/lowSpeed-no-infs.csv",
    speedLimitsPath = "/home/nkatz/dev/maritime/brest-data/areas_speed_limits.csv",
    closeToPortsPath = "/home/nkatz/dev/maritime/brest-data/recognition/1/close_to_ports.csv",
    chunkSize = 10,
    targetConcept = "lowSpeed",
    limit = 100000.0,
    trainingMode = false,
    hlesMap = HLEsMap,
    proxMap = proximityMap,
    portsMap = portsMap)



  def main(args: Array[String]) = {
    val argsok = CMDArgs.argsOk(args)
    if (!argsok._1) {
      println(argsok._2)
      System.exit(-1)
    } else {

      val runOpts = CMDArgs.getOLEDInputArgs(args)

      val dataOpts1 = highSpeedInDataOptionsTraining
      val dataOpts2 = highSpeedInDataOptionsTesting

      //val dataOpts = stoppedDataOptions
      //val dataOpts = sailingDataOptions
      //val dataOpts = lowSpeedDataOptions

      //val dataOpts = lowSpeedDataOptionsTraining
      //val dataOptsTest = lowSpeedDataOptionsTesting

      val trainingDataOptions = dataOpts1
      val testingDataOptions = dataOpts2
      val trainingDataFunction: MaritimeDataOptions => Iterator[Example] = getData
      val testingDataFunction: MaritimeDataOptions => Iterator[Example] = getData

      populateHLEsMap(dataOpts1.hlePath, dataOpts1.targetConcept)
      populatePortsMap(dataOpts1.closeToPortsPath)
      populateProximityMap(dataOpts1.llePath)
      populateSpeedLimitsMap(dataOpts1.speedLimitsPath)

      ///*
      val system = ActorSystem("HoeffdingLearningSystem")
      val startMsg = if (runOpts.evalth != "None") "EvaluateHandCrafted" else "start"
      system.actorOf(Props(new Master(runOpts, trainingDataOptions, testingDataOptions, trainingDataFunction, testingDataFunction)), name = "Master-Actor") !  startMsg
      //*/
    }
  }



  class MaritimeDataOptions(val llePath: String = "",
                            val db: String = "",
                            val hlePath: String,
                            val speedLimitsPath: String = "/home/nkatz/dev/maritime/brest-data/areas_speed_limits.csv",
                            val closeToPortsPath: String,
                            val chunkSize: Int = 1,
                            val limit: Double = Double.PositiveInfinity.toInt,
                            val targetConcept: String = "None",
                            val trainingMode: Boolean = true,
                            val hlesMap: scala.collection.mutable.Map[String, AnnotationPerVessel],
                            val proxMap: scala.collection.mutable.Map[String, AnnotationPerVessel],
                            val portsMap: scala.collection.mutable.Map[String, scala.collection.mutable.Set[String]]) extends app.runutils.IOHandling.Source




  def getData(opts: MaritimeDataOptions): Iterator[Example] = {

    val mongoClient = MongoClient()
    val collection = mongoClient(opts.db)("examples")

    val times =
      if (opts.trainingMode) {
        //LLEMap.keySet.toVector.sorted.toIterator.take(opts.limit.toInt).grouped(opts.chunkSize)
        collection.find().sort(MongoDBObject("time" -> 1)).limit(opts.limit.toInt).grouped(opts.chunkSize)
      } else {
        //LLEMap.keySet.toVector.sorted.toIterator.drop(opts.limit.toInt).grouped(opts.chunkSize)
        collection.find().sort(MongoDBObject("time" -> 1)).drop(opts.limit.toInt).take(10000).grouped(opts.chunkSize)
      }

    times map { timeSlice => getDataSlice(timeSlice, opts.hlesMap, opts.proxMap, opts.portsMap) }
  }


  def getDataSlice(objects: Seq[DBObject],
                   hlesMap: scala.collection.mutable.Map[String, AnnotationPerVessel],
                   proxMap: scala.collection.mutable.Map[String, AnnotationPerVessel],
                   ports: scala.collection.mutable.Map[String, scala.collection.mutable.Set[String]]) = {

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

    val hleAtoms = times.flatMap(t => getCurrentVesselsAnnotation(t, vessels, hlesMap)).filter(x => x != "None")
    val proximityAtoms = times.flatMap(t => getCurrentVesselsAnnotation(t, vessels, proxMap)).distinct.filter(x => x != "None")

    val (closeToPortsAtoms, speedLimitAtoms) = times.foldLeft(Set[String](), Set[String]()){ (accum, time) =>

      val closeToPortsAtoms_ = {
        if(ports.contains(time.toString)) ports(time.toString)
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
    if ( isWithinInterval(time.toInt, (interval.startTime, interval.endTime)) ) {
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




  case class VesselAnnotationAtom (atom: String, startTime: Int, endTime: Int, var hasBeenChecked: Boolean = false) {
    def getActualAtom(time: Int) = this.atom.replaceAll("ReplaceThisByActualTime", time.toString)
  }

  class AnnotationPerVessel(val vessel: String = "", var atoms: Vector[VesselAnnotationAtom] = Vector.empty, var currentIndex: Int = 0) {
    def updateAtoms(va: VesselAnnotationAtom) = this.atoms = this.atoms :+ va
    def updateIndex = this.currentIndex = this.currentIndex + 1
    def getCurrentAnnotationInterval = this.atoms(this.currentIndex)
    lazy val sortAtoms: Vector[VesselAnnotationAtom] = this.atoms.sortBy(atom => atom.endTime)
  }

  // The key is a vessel and the value is the set of all its annotation atoms wrapped in an AnnotationPerVessel instance
  var HLEsMap = scala.collection.mutable.Map[String, AnnotationPerVessel]()
  // The key is a vessel and the value is the set of all proximity atoms for that vessel.
  var proximityMap = scala.collection.mutable.Map[String, AnnotationPerVessel]()

  def updateMap(vessel: String, a: VesselAnnotationAtom, map: scala.collection.mutable.Map[String, AnnotationPerVessel]) = {
    if (map.contains(vessel)) map(vessel).updateAtoms(a)
    else map(vessel) = new AnnotationPerVessel(vessel, Vector(a))
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
          updateMap(vessel, a, HLEsMap)
        }
      case "loitering" | "stopped" | "lowSpeed" | "sailing" =>
        data foreach { x =>
          val s = x.split("\\|")
          val (startTime, endTime, vessel) = (s(3).toInt, s(4).toInt - 1, s(1))
          val atom = s"""holdsAt($hle("$vessel"),"ReplaceThisByActualTime")"""
          val a = VesselAnnotationAtom(atom, startTime, endTime)
          updateMap(vessel, a, HLEsMap)
        }
      case "rendezVous" =>
        data foreach { x =>
          val s = x.split("\\|")
          val (startTime, endTime, vessel1, vessel2)  = (s(4).toInt, s(5).toInt - 1, s(1), s(0))
          val atom = s"""holdsAt($hle("$vessel1","$vessel2"),"ReplaceThisByActualTime")"""
          val a = VesselAnnotationAtom(atom, startTime, endTime)
          updateMap(vessel1, a, HLEsMap)
          updateMap(vessel2, a, HLEsMap)
        }
    }
    println("Sorting the HLEs map values")
    //HLEsMap.foreach(x => x._2.atoms.sortBy(z => z.endTime))

    HLEsMap foreach { case (k, v) =>
      val v1 = v.atoms.sortBy(z => z.endTime)
      HLEsMap(k).atoms = v1
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
      updateMap(vessel1, a, proximityMap)
      updateMap(vessel2, a, proximityMap)
    }
  }












}

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

package experiments.datautils.maritime_data.first_version_doesnt_work

import com.mongodb.casbah.Imports._
import com.mongodb.casbah.MongoClient
import com.mongodb.casbah.commons.MongoDBObject
import logic.Examples.Example
import scala.io.Source

/**
  * Created by nkatz on 24/4/2017.
  */


/**
  *
  * THIS IS THE OLD VERSION THAT DOES NOT FLATTEN TIME STAMPS.
  * THERE IS AN ERROR IN SUBTRACTING 1 FROM TIMES, YOU DON'T HAVE TO DO THAT.
  *
  *
  *
  *
  * */


object DataPreProcessing {

  /*
  * ----------------------------------------------------------------------
  * HLEs are (closed-open). For example
  *
  * highSpeedIn|237029400|area159431200|true|1451654951|1451655191
  *
  * means that highSpeedIn holds in [1451654951, 1451655191),
  * therefore it is initiated at 1451654950 and terminated at 1451655190.
  *
  * The above will be turned into:
  *
  * holdsAt(highSpeedIn("237029400", area159431200), "1451654951")
  * holdsAt(highSpeedIn("237029400", area159431200), "1451655190")
  *------------------------------------------------------------------------
  *
  * The schema for each LLE we need at this point follows:
  *
  *  gap_start: HappensAt [gap_start 271043753] 1451802715
  *  coord: WE DON'T USE THIS FOR NOW
  *  velocity: HappensAt [velocity 240675000 0 270.00005134150797] 1451802711
  *  change_in_speed_start: HappensAt [change_in_speed_start 237955000] 1451802743
  *  stop_start: HappensAt [stop_start 636013060] 1451802771
  *  change_in_heading: HappensAt [change_in_heading 240096000] 1451802787
  *  isInArea: HappensAt [isInArea 239471800 area300240700] 1451802848
  *  change_in_speed_end: HappensAt [change_in_speed_end 237144200] 1451802872
  *  slow_motion_start: HappensAt [slow_motion_start 240802000] 1451802892
  *  proximity: WE DON'T USE THIS FOR NOW
  *  stop_end: HappensAt [stop_end 356460000] 1451802924
  *  gap_end: HappensAt [gap_end 271043772] 1451802920
  *  leavesArea: HappensAt [leavesArea 239371500 area300674000] 1451802925
  *  slow_motion_end: HappensAt [slow_motion_end 271044099] 1451802937
  *
  * The schema for each HLE is as follows:
  *
  *
  *
  * */



  case class LLE(lleName: String, atom: String, time: String, vessel: String, area: String)



  def main(args: Array[String]) = {


    val mongoClient = MongoClient()
    val dbName = "Maritime-Aegean-whole"


    val LLEcollection = mongoClient(dbName)("lles")

    val trainingExamplesCollection = mongoClient(dbName)("examples")

    val highSpeedCollection = mongoClient(dbName)("high_speed")
    val loiteringCollection = mongoClient(dbName)("loitering")
    val lowSpeedCollection = mongoClient(dbName)("low-speed")
    val rendezvousCollection = mongoClient(dbName)("rendezvous")
    val sailingCollection = mongoClient(dbName)("sailing")
    val stoppedCollection = mongoClient(dbName)("stopped")
    val withinAreaCollection = mongoClient(dbName)("within-area")
    val notCloseToPortsCollection = mongoClient(dbName)("not-close-to-ports")
    val speedLimitsCollections = mongoClient(dbName)("speed-limits")


    ///*
    mongoClient.dropDatabase("Maritime-Aegean")
    storeLLEsToMongo("/home/nkatz/dev/maritime-data/maritime-aegean/core_1/dataset.txt", LLEcollection)
    toMongo_areaHLEs("/home/nkatz/dev/maritime-data/maritime-aegean/core_1/recognition/highSpeedIn.csv", highSpeedCollection) // high-speed
    toMongo_areaHLEs("/home/nkatz/dev/maritime-data/maritime-aegean/core_1/recognition/withinArea.csv", withinAreaCollection) // withinArea
    toMongo_similarHLEs("/home/nkatz/dev/maritime-data/maritime-aegean/core_1/recognition/loitering.csv", loiteringCollection) // loitering
    toMongo_similarHLEs("/home/nkatz/dev/maritime-data/maritime-aegean/core_1/recognition/lowSpeed.csv", lowSpeedCollection) // low-speed
    toMongo_similarHLEs("/home/nkatz/dev/maritime-data/maritime-aegean/core_1/recognition/sailing.csv", sailingCollection) // sailing
    toMongo_similarHLEs("/home/nkatz/dev/maritime-data/maritime-aegean/core_1/recognition/stopped.csv", stoppedCollection) // stopped
    toMongo_rendezvous("/home/nkatz/dev/maritime-data/maritime-aegean/core_1/recognition/rendezVouz.csv", rendezvousCollection) // rendezVouz
    toMongo_notCloseToPorts("/home/nkatz/dev/maritime-data/maritime-aegean/core_1/recognition/close_to_ports.csv", notCloseToPortsCollection)
    toMongo_speedLimits("/home/nkatz/dev/maritime-data/maritime-aegean/core_1/static_data/all_areas/areas_speed_limits.csv", speedLimitsCollections)
    //*/

    /*
    * No need to do this stupid stuff:
    *
    * findDoc(LLEcollection, List(highSpeedCollection, loiteringCollection, lowSpeedCollection, rendezvousCollection, sailingCollection, stoppedCollection, withinAreaCollection), notCloseToPortsCollection, speedLimitsCollections)
    *
    * that tries to pair LLEs and HLEs. Just get the data at learning time
    *
    * */


    /*
    findDoc(LLEcollection, List(highSpeedCollection, loiteringCollection, lowSpeedCollection, rendezvousCollection, sailingCollection, stoppedCollection, withinAreaCollection), notCloseToPortsCollection, speedLimitsCollections)
    */

    /*
    val exmpls = mongoClient(dbName)("examples")
    exmpls.drop()

    val iter = getMaritimeData("Maritime-Aegean-whole", "highSpeedIn", 10)
    var i = 0
    iter foreach { x =>
      println(i)
      val e = x.exmplWithInertia
      val entry = MongoDBObject("time" -> e.time) ++ ("annotation" -> e.annotation) ++ ("narrative" -> e.narrative)
      exmpls.insert(entry)
      i += 1
    }
    */

  }


  def wholeDatasetToMongo(dbName: String) = {

  }


  def getMaritimeData(dbName: String, HLE: String, chunkSize: Int) = {

    def getHLEs(cursor: hleCollection.CursorType, lleTime: Int, initiationPoint: Boolean) = {
      cursor.foldLeft(List[String]()) { (atoms, hleDbObject) =>
        val hle = hleDbObject.asInstanceOf[BasicDBObject].get("hle").toString
        val atom = hle match {
          case "highSpeedIn" | "withinArea" =>
            val vessel = hleDbObject.asInstanceOf[BasicDBObject].get("vessel").toString
            val area = hleDbObject.asInstanceOf[BasicDBObject].get("area").toString
            if (!initiationPoint) s"""holdsAt($hle("$vessel","$area"),"$lleTime")""" else s"""holdsAt($hle("$vessel","$area"),"${lleTime+1}")"""
          case "loitering" | "lowSpeed" | "sailing" | "stopped" =>
            val vessel = hleDbObject.asInstanceOf[BasicDBObject].get("vessel").toString
            if (!initiationPoint) s"""holdsAt($hle("$vessel"),"$lleTime")""" else s"""holdsAt($hle("$vessel"),"${lleTime+1}")"""
          case "rendezVouz" =>
            val v1 = hleDbObject.asInstanceOf[BasicDBObject].get("vessel1").toString
            val v2 = hleDbObject.asInstanceOf[BasicDBObject].get("vessel2").toString
            if (!initiationPoint) s"""holdsAt($hle("$v1","$v2"),"$lleTime")""" else s"""holdsAt($hle("$v1","$v2"),"${lleTime+1}")"""
          case _ => throw new RuntimeException(s"HLE name: $hle not found")
        }
        atoms :+ atom
      }
    }

    val mc = MongoClient()
    val lleCollection = mc(dbName)("lles")
    val portsCollection = mc(dbName)("not-close-to-ports")
    val speedLimitsCollection = mc(dbName)("speed-limits")
    val hleCollection = HLE match {
      case "highSpeedIn" => mc(dbName)("high_speed")
      case "withinArea" => mc(dbName)("within-area")
      case "loitering" => mc(dbName)("loitering")
      case "lowSpeed" => mc(dbName)("low-speed")
      case "sailing" => mc(dbName)("sailing")
      case "stopped" => mc(dbName)("stopped")
      case _ => throw new RuntimeException(s"Don't know this LLE: $HLE")
    }
    lleCollection.createIndex(MongoDBObject("time" -> 1))
    val grouped = lleCollection.find().sort(MongoDBObject("time" -> 1)).grouped(chunkSize)
    val chunked = grouped.map { docs =>
      val (narrative, annotation) = docs.foldLeft(List[String](), List[String]()){ (accum, dbObject) =>
        val (_narrative, _annotation) = (accum._1, accum._2)
        val areas = dbObject.asInstanceOf[BasicDBObject].get("areas").asInstanceOf[BasicDBList].toList.map(x => x.toString)
        val vessels = dbObject.asInstanceOf[BasicDBObject].get("vessels").asInstanceOf[BasicDBList].toList.map(x => x.toString)
        val lles = dbObject.asInstanceOf[BasicDBObject].get("atoms").asInstanceOf[BasicDBList].toList.map(x => x.toString)
        val currentTime = dbObject.asInstanceOf[BasicDBObject].get("time").toString

        val vesselQueries = vessels.map(v => MongoDBObject("vessel" -> v) ++ ("time" -> currentTime) )
        val vs = vesselQueries flatMap ( q => portsCollection.find(q) )
        val portsAtoms = vs.map{ x =>
          val vessel = x.asInstanceOf[BasicDBObject].get("vessel").toString
          s"""notCloseToPorts("$vessel","$currentTime")"""
        }

        val areaQueries = areas.map(a => MongoDBObject("area" -> a)  )
        val as = areaQueries flatMap ( q => speedLimitsCollection.find(q) )
        val speedLimitAtoms = as.map{ x =>
          val area = x.asInstanceOf[BasicDBObject].get("area").toString
          val speed = x.asInstanceOf[BasicDBObject].get("limit").toString
          s"""speedLimit("$area","$speed")"""
        }

        val query1 = ("start_time" $lte currentTime.toInt) ++ ("end_time" $gte currentTime.toInt)
        val query2 = "start_time" $eq currentTime.toInt+1
        val hledocs1 = hleCollection.find(query1)
        val hledocs2 = hleCollection.find(query2)

        val initiationPoints = getHLEs(hledocs2, currentTime.toInt, initiationPoint=true)
        val medianPoints = getHLEs(hledocs1, currentTime.toInt, initiationPoint=false)

        (_narrative ++ lles ++ portsAtoms ++ speedLimitAtoms, (_annotation ++ initiationPoints ++ medianPoints).distinct )
      }
      val mergedExmplTime = docs.head.asInstanceOf[BasicDBObject].get("time").toString
      val _merged = new Example(annot = annotation, nar = narrative, _time = mergedExmplTime)
      //new Exmpl(_id = _merged.time, exampleWithInertia = _merged)
      _merged
    }
    chunked
  }

  /*
  def findDoc(lleCollection: MongoCollection, hleCollections: List[MongoCollection],
              portsCollection: MongoCollection, speedLimitsCollection: MongoCollection, DBToWriteTo: String) = {

    // That's necessary to avoid the 32MB limit with in-memory sort.
    // It also makes streaming the data from DBs much faster.
    lleCollection.createIndex(MongoDBObject("time" -> 1))
    //hleCollections.foreach(x => x.createIndex(MongoDBObject("time" -> 1))) // no "time" field here after all

    lleCollection.find().sort(MongoDBObject("time" -> 1)).foreach { lledoc =>

      var hles = List[String]()

      val lleTime = lledoc.asInstanceOf[BasicDBObject].get("time").toString.toInt

      hleCollections.foreach { hleColl =>
        val query = ("start_time" $lte lleTime) ++ ("end_time" $gte lleTime)
        val hledocs = hleColl.find(query)
        if (hledocs.nonEmpty) {
          for (doc <- hledocs) {
            val hle = doc.asInstanceOf[BasicDBObject].get("hle").toString
            hle match {
              case "highSpeedIn" | "withinArea" =>
                val vessel = doc.asInstanceOf[BasicDBObject].get("vessel").toString
                val area = doc.asInstanceOf[BasicDBObject].get("area").toString
                val atom = s"""holdsAt($hle("$vessel","$area"),$lleTime)"""
                hles = hles :+ atom
              case "loitering" | "lowSpeed" | "sailing" | "stopped" =>
                val vessel = doc.asInstanceOf[BasicDBObject].get("vessel").toString
                val atom = s"""holdsAt($hle("$vessel"),$lleTime)"""
                hles = hles :+ atom
              case "rendezVouz" =>
                val v1 = doc.asInstanceOf[BasicDBObject].get("vessel1").toString
                val v2 = doc.asInstanceOf[BasicDBObject].get("vessel2").toString
                val atom = s"""holdsAt($hle("$v1","$v2"),$lleTime)"""
                hles = hles :+ atom
              case _ => throw new RuntimeException(s"HLE name: $hle not found")
            }
          }
        }
      }

      // Get info for vessels in ports
      val lledocVessels = lledoc.asInstanceOf[BasicDBObject].get("vessels").asInstanceOf[BasicDBList].toList.map(x => x.toString)
      val vesselQueries = lledocVessels.map(v => MongoDBObject("vessel" -> v) ++ ("time" -> lleTime.toString) )
      val t = vesselQueries flatMap { q =>
        portsCollection.find(q)
      }
      //println(getAtomsField(lledoc))
      val portsAtoms = t.map{ x =>
        val vessel = x.asInstanceOf[BasicDBObject].get("vessel").toString
        s"""notCloseToPorts("$vessel","$lleTime")"""
      }

      // Get info on speed limits
      val lledocAreas = lledoc.asInstanceOf[BasicDBObject].get("areas").asInstanceOf[BasicDBList].toList.map(x => x.toString)
      val areaQueries = lledocAreas.map(a => MongoDBObject("area" -> a)  )
      val t1 = areaQueries flatMap { q =>
        speedLimitsCollection.find(q)
      }
      val speedLimitAtoms = t1.map{ x =>
        val area = x.asInstanceOf[BasicDBObject].get("area").toString
        val speed = x.asInstanceOf[BasicDBObject].get("limit").toString
        s"""speedLimit("$area","$speed")"""
      }

      val narrative = getAtomsField(lledoc) ++ portsAtoms ++ speedLimitAtoms
      val annotation = hles
      val entry = MongoDBObject("time" -> lleTime) ++ ("annotation" -> annotation) ++ ("narrative" -> narrative)
      trainingExamplesCollection.insert(entry)
      println(s"Inserted $lleTime")
    }
  }
  */

  def getAtomsField(x: DBObject) = {
    x.asInstanceOf[BasicDBObject].get("atoms").asInstanceOf[BasicDBList].toList.map(x => x.toString)
  }



  def toMongo_notCloseToPorts(path: String, collection: MongoCollection) = {
    val info = Source.fromFile(path).getLines.toList.map{x =>
      val s = x.split("\\|")
      val time = s(0)
      val vessel = s(1)
      (vessel, time)
    }
    info.foreach { x =>
      val (vessel, time) = (x._1, x._2)
      val entry = MongoDBObject("hle" -> "ports") ++ ("vessel" -> vessel) ++ ("time" -> time)
      //println(s"Inserted $entry")
      collection.insert(entry)
    }
  }

  def toMongo_speedLimits(path: String, collection: MongoCollection) = {
    val info = Source.fromFile(path).getLines.toList.map{x =>
      val s = x.split("\\|")
      val area = s(0)
      val speed = s(1)
      (area, speed)
    }
    info.foreach { x =>
      val (area, speed) = (x._1, x._2)
      val entry = MongoDBObject("hle" -> "speed-limits") ++ ("area" -> area) ++ ("limit" -> speed)
      //println(s"Inserted $entry")
      collection.insert(entry)
    }
  }

  /*
   * Handles:
   *
   * highSpeedIn
   * withinArea
   *
   * that have the same schema
   * */
  def toMongo_areaHLEs(path: String, collection: MongoCollection) = {
    val hles = Source.fromFile(path).getLines.toList.map{x =>
      val s = x.split("\\|")
      val startTime = s(4)
      val endTime = (s(5).toInt - 1).toString
      val hle = s(0)
      val vessel = s(1)
      val area = s(2)
      (hle, vessel, area, startTime, endTime)
    }
    hles.foreach { hle =>
      val (hleName, vessel, area, startTime, endTime) = (hle._1, hle._2, hle._3, hle._4, hle._5)
      val entry = MongoDBObject("hle" -> hleName) ++ ("start_time" -> startTime.toInt) ++ ("end_time" -> endTime.toInt) ++ ("vessel" -> vessel) ++ ("area" -> area) ++
        ("text_description" -> "The predicate schema for this HLE is holdsAt(HLEName(Vessel,Area),Time). There are two HLEs that subscribe to this schema: highSpeedIn and withinArea.")
      //println(s"Inserted $entry")
      collection.insert(entry)
    }
  }

  /* Handles the rendezVouz HLE */
  def toMongo_rendezvous(rendezvousPath: String, collection: MongoCollection) = {
    val hles = Source.fromFile(rendezvousPath).getLines.toList.map { x =>
      val s = x.split("\\|")
      val startTime = s(4)
      val endTime = (s(5).toInt - 1).toString
      val hle = s(0)
      val vessel1 = s(1)
      val vessel2 = s(2)
      (hle, vessel1, vessel2, startTime, endTime)
    }
    hles.foreach { hle =>
      val (hleName, vessel1, vessel2, startTime, endTime) = (hle._1, hle._2, hle._3, hle._4, hle._5)
      val entry = MongoDBObject("hle" -> hleName) ++ ("start_time" -> startTime.toInt) ++ ("end_time" -> endTime.toInt) ++ ("vessel1" -> vessel1) ++ ("vessel2" -> vessel2) ++
        ("text_description" -> "The predicate schema for this HLE is holdsAt(rendezVouz(Vessel1,Vessel2),Time).")
      //println(s"Inserted $entry")
      collection.insert(entry)
    }
  }



  /*loitering, low-speed, sailing, stopped have the same schema, so they are handled by the same method */
  /*
  *
  * Handles:
  *
  * loitering
  * low-speed
  * sailing
  * stopped
  *
  * that hace the same schema
  *
  * */
  def toMongo_similarHLEs(path: String, collection: MongoCollection) = {
    def hles = Source.fromFile(path).getLines.toList.map { x =>
      val s = x.split("\\|")
      val startTime = s(3)
      val endTime = (s(4).toInt - 1).toString
      val hle = s(0)
      val vessel = s(1)
      (hle, vessel, startTime, endTime)
    }
    hles.foreach { hle =>
      val (hleName, vessel, startTime, endTime) = (hle._1, hle._2, hle._3, hle._4)
      val entry = MongoDBObject("hle" -> hleName) ++ ("start_time" -> startTime.toInt) ++ ("end_time" -> endTime.toInt) ++ ("vessel" -> vessel) ++
        ("text_description" -> "The predicate schema for this HLE is holdsAt(HLEName(Vessel),Time). The HLEs that subscribe to this schema are: loitering, low-speed, sailing, stopped")
      println(s"Inserted $entry")
      collection.insert(entry)
    }
  }





  def storeLLEsToMongo(datasetPath: String, LLEcollection: MongoCollection) = {

    println("Processing LLES...")

    val hles_raw = Source.fromFile(datasetPath).getLines.toList

    // filter out proximity for now
    val hles_raw_no_proximity = hles_raw.filter(x => !x.contains("HoldsFor") && !x.contains("coord"))

    // Group the LLEs by time
    val grouped = hles_raw_no_proximity.groupBy(x => getTime(x))
    grouped foreach { case (k, v) =>
      val llesPerTimePoint = v.map(z => LLEtoPredicateForm(z))
      val vessels = llesPerTimePoint.map(x => x.vessel).distinct
      val areas = llesPerTimePoint.map(x => x.area).distinct.filter(z => z != "None")
      val atoms = llesPerTimePoint.map(x => x.atom)
      val entry = MongoDBObject("time" -> k.toInt) ++ ("areas" -> areas) ++ ("vessels" -> vessels) ++ ("atoms" -> atoms)
      LLEcollection.insert(entry)
    }
  }



  /* Convert a data point to predicate form. This does not work with proximity for now.
   * This returns the predicate itself, the vessel and the area (if an area is involved).
   *
   * */
  def LLEtoPredicateForm(x: String) = {
    var area = "None"
    var predicate = "None"
    val info = x.split("HappensAt")(1)
    val _info = info.split("\\]")
    val time = _info(1).trim
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
    LLE(lle, predicate, time, vessel, area)
  }

  /* Get the time stamp from a line. */
  def getTime(dataLine: String) = {
    // "HoldsFor" is used for vessel proximity we won't use it now.
    if (!dataLine.contains("HoldsFor")) {
      val info = dataLine.split("HappensAt")(1)
      val _info = info.split("\\]")
      _info(1).trim
    } else {
      "None"
    }
  }

  /* Utility function, returns the (distinct) LLE names from the LLE file.
   * Run it like:
   *
   * val hles_raw = Source.fromFile(datasetPath).getLines.toList
   *  hles_raw.map(x => getLineInfo(x)).distinct foreach println
   *
   * */
  def getLLENames(data: List[String]) = data.map(x => getLLE(x)).distinct foreach println

  def getLLE(dataLine: String) = {
    val info = if(dataLine.contains("HappensAt")) dataLine.split("HappensAt")(1) else dataLine.split("HoldsFor")(1)
    val _info = info.split("\\]")
    val time = _info(1).trim
    val rest = _info(0).split("\\[")(1)
    rest.split(" ")(0)
  }



}

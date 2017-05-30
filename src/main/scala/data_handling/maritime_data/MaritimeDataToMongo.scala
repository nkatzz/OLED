package data_handling.maritime_data

/**
  * Created by nkatz on 4/26/17.
  */


import com.mongodb.casbah.Imports._
import com.mongodb.casbah.MongoClient
import com.mongodb.casbah.commons.MongoDBObject
import logic.Examples.Example
import utils.Exmpl

object MaritimeDataToMongo {


  def main(args: Array[String]) = {

    storeMaritimeData_Whole()

    //val mc = MongoClient()
    //mc("Maritime-Aegean-All_HLEs-Joined")("examples").find().foreach(println)

  }

  def storeMaritimeData_Whole() = {
    val mc = MongoClient()
    val exmpls = getMaritimeData_Whole("Maritime-Aegean-whole", chunkSize = 1, mc)
    val newCollection = mc("Maritime-Aegean-All_HLEs-Joined")("examples")
    var i = 0
    for (x <- exmpls) {
      println(i)
      val e = x.exmplWithInertia
      val entry = MongoDBObject("time" -> e.time) ++ ("annotation" -> e.annotation) ++ ("narrative" -> e.narrative)
      newCollection.insert(entry)
      i += 1
    }
  }

  /* Try to get all data at once, for all HLEs */
  def getMaritimeData_Whole(readFromDB: String, chunkSize: Int, mc: MongoClient) = {

    def getHLEs(cursor: Iterator[DBObject], lleTime: Int, initiationPoint: Boolean) = {
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

    //val mc = MongoClient()
    val lleCollection = mc(readFromDB)("lles")
    val portsCollection = mc(readFromDB)("not-close-to-ports")
    val speedLimitsCollection = mc(readFromDB)("speed-limits")


    val hleCollections = List(mc(readFromDB)("high_speed"), mc(readFromDB)("within-area"),
      mc(readFromDB)("loitering"), mc(readFromDB)("low-speed"), mc(readFromDB)("sailing"), mc(readFromDB)("stopped"))

    /*
    val hleCollection = HLE match {
      case "highSpeedIn" => mc(dbName)("high_speed")
      case "withinArea" => mc(dbName)("within-area")
      case "loitering" => mc(dbName)("loitering")
      case "lowSpeed" => mc(dbName)("low-speed")
      case "sailing" => mc(dbName)("sailing")
      case "stopped" => mc(dbName)("stopped")
      case _ => throw new RuntimeException(s"Don't know this LLE: $HLE")
    }
    */

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
        val hledocs1 = hleCollections.map(c => c.find(query1))
        val hledocs2 = hleCollections.map(c => c.find(query2))

        val initiationPoints = hledocs2.flatMap(x => getHLEs(x, currentTime.toInt, initiationPoint=true))
        val medianPoints = hledocs1.flatMap(x => getHLEs(x, currentTime.toInt, initiationPoint=false))

        (_narrative ++ lles ++ portsAtoms ++ speedLimitAtoms, (_annotation ++ initiationPoints ++ medianPoints).distinct )
      }
      val mergedExmplTime = docs.head.asInstanceOf[BasicDBObject].get("time").toString
      val _merged = new Example(annot = annotation, nar = narrative, _time = mergedExmplTime)
      new Exmpl(_id = _merged.time, exampleWithInertia = _merged)
    }
    chunked
  }









}

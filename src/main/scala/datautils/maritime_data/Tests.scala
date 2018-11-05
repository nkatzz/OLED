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

import com.mongodb.{BasicDBList, BasicDBObject}
import com.mongodb.casbah.{MongoClient, MongoCollection}
import com.mongodb.casbah.Imports._


/**
  * Created by nkatz on 6/23/17.
  */

object Tests extends App {

  val dbName = "maritime-brest"
  val mongoClient = MongoClient()

  val llesCollection = mongoClient(dbName)("lles")
  llesCollection.createIndex(MongoDBObject("time" -> 1))

  /*High speed*/
  val highSpeedCollection = mongoClient(dbName)("high_speed")
  //highSpeedCollection.createIndex(MongoDBObject("interval"))
  /*Within area*/
  val withinAreaCollection = mongoClient(dbName)("within-area")
  //withinAreaCollection.createIndex(MongoDBObject("interval"))
  /*Sailing*/
  val sailingCollection = mongoClient(dbName)("sailing")
  //sailingCollection.createIndex(MongoDBObject("interval"))
  /*Stopped*/
  val stoppedCollection = mongoClient(dbName)("stopped")
  //stoppedCollection.createIndex(MongoDBObject("interval"))
  /*loitering*/
  val loiteringCollection = mongoClient(dbName)("loitering")
  //loiteringCollection.createIndex(MongoDBObject("interval"))
  /*rendezvous*/
  val rendezvousCollection = mongoClient(dbName)("rendezvous")
  //rendezvousCollection.createIndex(MongoDBObject("interval"))
  /*Low speed*/
  val lowSpeedCollection = mongoClient(dbName)("low-speed")
  //lowSpeedCollection.createIndex(MongoDBObject("interval"))

  val speedLimitsCollection = mongoClient(dbName)("speed_limits")
  speedLimitsCollection.createIndex(MongoDBObject("area" -> 1))

  val closeToPortsCollection = mongoClient(dbName)("ports")
  speedLimitsCollection.createIndex(MongoDBObject("time" -> 1))

  val proximityCollection = mongoClient(dbName)("proximity")
  //proximityCollection.createIndex(MongoDBObject("interval"))


  val hleCollections = List(highSpeedCollection, withinAreaCollection, sailingCollection,
    stoppedCollection, loiteringCollection, rendezvousCollection, lowSpeedCollection, proximityCollection)



  val writeToDB = mongoClient("high-speed-stand-alone-db")("examples")
  mongoClient.dropDatabase("high-speed-stand-alone-db")
  writeToDB.createIndex(MongoDBObject("time" -> 1))

  getMaritimeDataInChunks(llesCollection, 5)


  def getMaritimeDataInChunks(llesCollection: MongoCollection, chunkSize: Int) = {

    val startTime = llesCollection.find().sort(MongoDBObject("time" -> 1)).map(x => x.asInstanceOf[BasicDBObject].get("time").toString.toInt).next()
    val endTime = llesCollection.find().sort(MongoDBObject("time" -> -1)).map(x => x.asInstanceOf[BasicDBObject].get("time").toString.toInt).next()

    (startTime to endTime).grouped(chunkSize) foreach { timeSlice =>
      val first = timeSlice.head
      val last = if (timeSlice.length > 1) timeSlice.tail.reverse.head else first
      val lleResults = llesCollection.find("time" $gte first $lte last)

      val (narrativeAtoms, notCloseToPortsAtoms, areas1) = lleResults.foldLeft(List[String](), List[String](), List[String]()) { (accum, o) =>
        val a = getLLEInfo(o)
        val (atom, portAtom, as) = (a._1, a._2, a._3)
        (accum._1 :+ atom, accum._2 ++ portAtom, accum._3 ++ as)
      }

      val (annotAtoms, areas2) = timeSlice.toList.foldLeft(List[String](), List[String]()) { (accum, time) =>

        val results = hleCollections.foldLeft(Iterator[DBObject]()) { (x, collection) =>
          val t = collection.find( MongoDBObject("interval" -> time) )
          x ++ t
        }

        val (anotAtoms_, areas_) = results.foldLeft(List[String](), List[String]()) { (z, o) =>
          val a = getHLEInfo(o, time)
          val (atom, as) = (a._1, a._2)
          (z._1 :+ atom, z._2 ++ as)
        }
        (accum._1 ++ anotAtoms_, accum._2 ++ areas_)
      }

      val areas = (areas1 ++ areas2).distinct.filter(_ != "None")
      val speedLimitAtoms = areas.foldLeft(List[String]()){ (k, area) =>
        val results = speedLimitsCollection.find( MongoDBObject("area" -> area) )
        val resToStrs = results.map ( obj => obj.asInstanceOf[BasicDBObject].get("atom").toString )
        k ++ resToStrs
      }

      println(s"$first-$last")

      //------------------------------
      // INSERT TO THE STAND-ALONE DB
      //------------------------------
      val entry = MongoDBObject("time" -> first) ++ ("annotation" -> annotAtoms) ++ ("narrative" -> (narrativeAtoms++speedLimitAtoms++notCloseToPortsAtoms))
      writeToDB.insert(entry)
    }
  }

  def getLLEInfo(o: DBObject) = {
    val atom = o.asInstanceOf[BasicDBObject].get("lle").toString
    //val vs = o.asInstanceOf[BasicDBObject].get("vessels").asInstanceOf[BasicDBList].toList.map(_.toString)
    val time = o.asInstanceOf[BasicDBObject].get("time").toString.toInt
    val r = closeToPortsCollection.find( MongoDBObject("time" -> time) )
    val notCloseToPortsAtoms = r.map( obj => obj.asInstanceOf[BasicDBObject].get("atom").toString )
    val as = o.asInstanceOf[BasicDBObject].get("areas").asInstanceOf[BasicDBList].toList.map(_.toString)
    (atom, notCloseToPortsAtoms, as)
  }

  def getHLEInfo(o: DBObject, time: Int) = {
    val atom = o.asInstanceOf[BasicDBObject].get("hle").toString.replaceAll("ReplaceThisByActualTime", time.toString)
    //val vs = o.asInstanceOf[BasicDBObject].get("vessels").asInstanceOf[BasicDBList].toList.map(_.toString)
    val as = o.asInstanceOf[BasicDBObject].get("areas").asInstanceOf[BasicDBList].toList.map(_.toString)
    //(atom, vs, as)
    (atom, as)
  }




  /**
    * Returns all documents from a collection, either sorted or not.
    * Use it like getAllDataFromDb(col, "time") foreach { do something }, or e.g.
    * getAllDataFromDb(mongoClient(dbName)("high_speed")).foreach(x => println(x))
    * */
  def getAllDataFromDb(collection: MongoCollection, sortByKeyName: String = ""): collection.CursorType = {
    if (sortByKeyName != "") {
      collection.createIndex(MongoDBObject(sortByKeyName -> 1))
      collection.find().sort(MongoDBObject(sortByKeyName -> 1))
    } else {
      collection.find()
    }
  }

}

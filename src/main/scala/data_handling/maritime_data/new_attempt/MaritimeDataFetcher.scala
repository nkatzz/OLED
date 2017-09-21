package data_handling.maritime_data.new_attempt

import com.mongodb.{BasicDBList, BasicDBObject}
import com.mongodb.casbah.MongoClient
import com.mongodb.casbah.Imports._
import logic.Examples.Example
import scala.collection.immutable.HashSet

/**
  * Created by nkatz on 7/4/17.
  */

object MaritimeDataFetcher extends App {

  val dbName = "maritime-brest"
  val mongoClient = MongoClient()

  private val llesCollection = mongoClient(dbName)("lles")
  llesCollection.createIndex(MongoDBObject("time" -> 1))

  private val highSpeedCollection = mongoClient(dbName)("highSpeedIn")
  highSpeedCollection.createIndex(MongoDBObject("vessel" -> 1))

  //private val withinAreaCollection = mongoClient(dbName)("withinArea")
  //withinAreaCollection.createIndex(MongoDBObject("vessel"))

  private val sailingCollection = mongoClient(dbName)("sailing")
  sailingCollection.createIndex(MongoDBObject("vessel" -> 1))

  private val stoppedCollection = mongoClient(dbName)("stopped")
  stoppedCollection.createIndex(MongoDBObject("vessel" -> 1))

  //private val loiteringCollection = mongoClient(dbName)("loitering")
  //loiteringCollection.createIndex(MongoDBObject("vessel"-> 1))

  private val rendezvousCollection = mongoClient(dbName)("rendezVous")
  rendezvousCollection.createIndex(MongoDBObject("vessel" -> 1))

  private val lowSpeedCollection = mongoClient(dbName)("lowSpeed")
  lowSpeedCollection.createIndex(MongoDBObject("vessel" -> 1))

  private val speedLimitsCollection = mongoClient(dbName)("speed_limits")
  speedLimitsCollection.createIndex(MongoDBObject("vessel" -> 1))

  private val closeToPortsCollection = mongoClient(dbName)("ports")
  closeToPortsCollection.createIndex(MongoDBObject("time" -> 1))

  private val proximityCollection = mongoClient(dbName)("proximity")
  proximityCollection.createIndex(MongoDBObject("vessel" -> 1))

  /*
  val collectionsMap = Map("highSpeedIn" -> highSpeedCollection, "stopped" -> stoppedCollection,
    "lowSpeed" -> lowSpeedCollection, "sailing" -> sailingCollection, "rendezVous" -> rendezvousCollection,
    "loitering" -> loiteringCollection, "withinArea" -> withinAreaCollection)
  */

  //val hleCollections = List(highSpeedCollection, stoppedCollection, lowSpeedCollection, sailingCollection, rendezvousCollection)

  val hleCollections = List(highSpeedCollection)

  val newCollection = mongoClient(dbName)("examples")
  newCollection.dropCollection()
  newCollection.createIndex(MongoDBObject("time"-> 1))


  println("Creating times hash set")
  val times = HashSet() ++ Runner.getAllTimes()

  getData(times)

  def getData(lleActualTimes: HashSet[Int]) = {

    var counter = 0

    lleActualTimes foreach { time =>

      val lleResults = llesCollection.find(MongoDBObject("time" -> time))

      val (narrativeAtoms, notCloseToPortsAtoms, areas_, vessels_) = lleResults.foldLeft(List[String](), List[String](), List[String](), List[String]()) { (accum, o) =>
        val a = getLLEInfo(o)
        val (atom, portAtom, as, vs) = (a._1, a._2, a._3, a._4)
        (accum._1 :+ atom, accum._2 ++ portAtom, accum._3 ++ as, accum._4 ++ vs)
      }

      val vessels = vessels_.distinct

      ///*
      val hleAtoms = hleCollections.foldLeft(List[String]()) { (x, collection) =>
        val r = vessels flatMap { vessel =>
          collection.find( MongoDBObject("vessel" -> vessel) ).map(obj => obj.asInstanceOf[BasicDBObject].get("atom").toString)
        }
        x ++ r
      }
      //*/

      //val hleAtoms = List[String]()

      val areas = areas_.distinct.filter(_ != "None")

      val speedLimitAtoms = areas.foldLeft(List[String]()){ (k, area) =>
        val results = speedLimitsCollection.find( MongoDBObject("area" -> area) )
        val resToStrs = results.map ( obj => obj.asInstanceOf[BasicDBObject].get("atom").toString )
        k ++ resToStrs
      }

      val narrative = narrativeAtoms++speedLimitAtoms++notCloseToPortsAtoms
      if (narrative.nonEmpty) {
        val entry = MongoDBObject("annotation" -> hleAtoms) ++ ("narrative" -> narrative) ++ ("time" -> counter)
        println(counter, time)
      }
      counter += 1
    }
  }

  def getLLEInfo(o: DBObject) = {
    val atom = o.asInstanceOf[BasicDBObject].get("lle").toString
    val vs = o.asInstanceOf[BasicDBObject].get("vessels").asInstanceOf[BasicDBList].toList.map(_.toString)
    val time = o.asInstanceOf[BasicDBObject].get("time").toString.toInt
    val r = closeToPortsCollection.find( MongoDBObject("time" -> time) )
    val notCloseToPortsAtoms = r.map( obj => obj.asInstanceOf[BasicDBObject].get("atom").toString )
    val as = o.asInstanceOf[BasicDBObject].get("areas").asInstanceOf[BasicDBList].toList.map(_.toString)
    (atom, notCloseToPortsAtoms, as, vs)
  }


}

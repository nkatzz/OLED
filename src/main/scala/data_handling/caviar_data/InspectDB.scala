package data_handling.caviar_data

import com.mongodb.BasicDBObject
import com.mongodb.casbah.Imports.MongoDBObject
import com.mongodb.casbah.MongoClient
import logic.Examples.Example

/**
  * Created by nkatz on 3/13/17.
  *
  * Utilities to inspect data
  */

object InspectDB extends App {

  val dbName = "maritime-brest"
  //val dbName = "CAVIAR-MERGED-COPIES-10"
  //val dbName = "caviar"
  //val dbName = "CAVIAR_Real_original"

  val mongoClient = MongoClient()

  val collection = mongoClient(dbName)("examples")



  collection.find().foreach{ x =>
    val e = Example(x)

    println(e.narrative.map(x => x+".").mkString("\n"))
    println(e.annotation.filter(x => x.contains("highSpeedIn")).map(z => z+".").mkString("\n"))
    println("")
    println("")

  }


  /*
  collection.find().foreach{ x =>
    val e = Example(x)
    if (e.time == "507120") {
      if(e.annotation.exists(x => x.contains("meeting"))){
        println("has meeting")
      } else {
        println("not has meeting")
      }
    }
  }
  */

}

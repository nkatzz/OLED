package data_handling

import com.mongodb.casbah.MongoClient
import logic.Examples.Example

/**
  * Created by nkatz on 3/13/17.
  *
  * Utilities to inspect data
  */

object InspectDB extends App {

  val dbName = "CAVIAR_Real_FixedBorders"
  //val dbName = "CAVIAR_Real_original"

  val mongoClient = MongoClient()

  val collection = mongoClient(dbName)("examples")

  /*
  collection.find().foreach{ x =>
    val e = Example(x)
    if (e.annotation.nonEmpty) println(e.time)
  }
  */

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

}

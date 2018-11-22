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

package experiments.datautils.caviar_data

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


  val dbName = "caviar-train"
  //val dbName = "maritime-brest"
  //val dbName = "CAVIAR-MERGED-COPIES-10"
  //val dbName = "caviar"
  //val dbName = "CAVIAR_Real_original"

  val mongoClient = MongoClient()

  val collection = mongoClient(dbName)("examples")

  collection.find().foreach{ x =>
    val e = Example(x)

    if (e.annotation.nonEmpty) println(e)


    /*
    println(e.narrative.map(x => x+".").mkString("\n"))
    println(e.annotation.filter(x => x.contains("highSpeedIn")).map(z => z+".").mkString("\n"))
    println("")
    println("")
    */
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

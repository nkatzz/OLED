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

import com.mongodb.casbah.MongoClient
import com.mongodb.casbah.commons.MongoDBObject
import com.mongodb.casbah.Imports._
import logic.Examples.Example

/**
  * Created by nkatz on 3/13/17.
  *
  * Utilities to inspect data
  */

object InspectDB extends App {

  /*
  val dbName = "caviar"
  //val dbName = "maritime-brest"
  //val dbName = "CAVIAR-MERGED-COPIES-10"
  //val dbName = "caviar"
  //val dbName = "CAVIAR_Real_original"

  val mongoClient = MongoClient()
  val event = "meeting"
  val collection = mongoClient(dbName)("examples")
  collection.find().foreach{ x =>
    val e = Example(x)
    if (e.annotation.nonEmpty) {
      val f = e.annotation.filter(x => x.contains(event))
      println(f)
    }
  }
  */

  val dbName = "caviar-train"
  val newDbName = "caviar-train-1"
  val mongoClient = MongoClient()
  val collection = mongoClient(dbName)("examples")
  var idCounter = 0
  mongoClient.dropDatabase(newDbName)
  val newCollection = mongoClient(newDbName)("examples")

  collection.find().foreach{ x =>
    val e = Example(x)
    val entry = MongoDBObject("time" -> e.time) ++ ("annotation" -> e.annotation) ++ ("narrative" -> e.narrative) ++ ("_id" -> idCounter)
    newCollection.insert(entry)
    idCounter += 1
  }


}

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

package mcts

import com.mongodb.casbah.Imports.{BasicDBList, BasicDBObject}
import com.mongodb.casbah.MongoClient
import com.mongodb.casbah.commons.MongoDBObject
import com.mongodb.casbah.Imports._

/**
  * Created by nkatz on 9/28/17.
  */

object TransformCTM extends App {

  object MongoEntry {
    def apply() = {
      new MongoEntry(Nil, Nil, 0)
    }
  }
  class MongoEntry(val annotation: List[String], val narrative: List[String], val time: Int)

  val count = 0
  val mongoClient = MongoClient()
  //val collection = mongoClient("caviar-whole")("examples")
  val collection = mongoClient("ctm")("examples")
  collection.createIndex(MongoDBObject("example" -> 1))

  val data = collection.find().sort(MongoDBObject("example" -> 1)).foldLeft(Vector[MongoEntry](), 0) { (x, obj) =>
    val (accum, count) = (x._1, x._2)
    val annotation = obj.asInstanceOf[BasicDBObject].get("pos").asInstanceOf[BasicDBList].toList.map(x => x.toString)
    val narrative = obj.asInstanceOf[BasicDBObject].get("nar").asInstanceOf[BasicDBList].toList.map(x => x.toString)
    (accum :+ new MongoEntry(annotation, narrative, count), count + 1)
  }

  data._1.foreach(x => println(x.annotation + "\n" + x.narrative + "\n" + x.time + "\n"))

  mongoClient.dropDatabase("ctm")

  data._1.foreach { x =>
    val entry = MongoDBObject("time" -> x.time) ++ ("annotation" -> x.annotation) ++ ("narrative" -> x.narrative)
    collection.insert(entry)
  }

  println("Done")

}

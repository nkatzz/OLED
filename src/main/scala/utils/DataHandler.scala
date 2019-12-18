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

package utils

import com.mongodb.casbah.MongoClient
import com.mongodb.casbah.commons.MongoDBObject
import logic.Examples.Example

/**
  * Created by nkatz on 6/26/17.
  */

object DataHandler {

  trait InputOptions

  class BasicInputOptionsWrapper(
      val dbName: String,
      val collectionName: String = "examples",
      val chunkSize: Int = 1,
      val targetConcept: String = "None",
      val sortDbByField: String = "None",
      val sort: String = "ascending") extends InputOptions

  def basicDataFunction(options: BasicInputOptionsWrapper): Iterator[Example] = {
    val mc = MongoClient()
    val collection = mc(options.dbName)(options.collectionName)

    collection.createIndex(MongoDBObject(options.sortDbByField -> 1))
    val data = collection.find().sort(MongoDBObject(options.sortDbByField -> 1)).map { x =>
      val e = Example(x)
      if (options.targetConcept == "None") {
        new Example(annot = e.annotation, nar = e.narrative, _time = e.time)
      } else {
        new Example(annot = e.annotation filter (_.contains(options.targetConcept)), nar = e.narrative, _time = e.time)
      }
    }
    val dataChunked = data.grouped(options.chunkSize)
    val dataIterator = dataChunked.map { x =>
      x.foldLeft(Example()) { (z, y) =>
        new Example(annot = z.annotation ++ y.annotation, nar = z.narrative ++ y.narrative, _time = x.head.time)
      }
    }
    dataIterator
  }

}

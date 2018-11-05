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

package app.runutils

import com.mongodb.casbah.commons.MongoDBObject
import com.mongodb.casbah.{MongoClient, MongoCollection}
import logic.Examples.Example
import utils.DataUtils.Interval

/**
  * Created by nkatz on 6/28/17.
  */

object IOHandling {

  // TODO
  // Currently used by the maritime runner
  trait Source

  trait MongoSource extends Source {

    def createIndex(collection: MongoCollection, sort: String = "ascending", sortKey: String = "None"): Unit = {
      sortKey match {
        case "None" =>
        case _ =>
          val i = if (sort == "ascending") 1 else -1
          collection.createIndex(MongoDBObject(sortKey -> i))
      }
    }

    def allData(collection: MongoCollection, sort: String = "ascending", sortKey: String = "None"): collection.CursorType = {
      sortKey match {
        case "None" => collection.find()
        case _ =>
          val i = if (sort == "ascending") 1 else -1
          collection.find().sort(MongoDBObject(sortKey -> i))
      }
    }
  }



  // TODO
  trait FileSource









  /*
  def getData[T <: Source](opts: T, dataFunc: (T) => Iterator[Example]) = {
    dataFunc(opts)
  }
  def run[T <: Source](opts: T, dataFunc: (T) => Iterator[Example]): Iterator[Example] = {
    dataFunc(opts)
  }
  //getData(new DefaultMongoDataOptions(""), getMongoData)
  */


}

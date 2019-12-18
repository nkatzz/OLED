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

import com.mongodb.casbah.MongoClient
import logic.Examples.Example

/**
  * Created by nkatz at 25/10/2018
  */

object IOUtils {

  def main(args: Array[String]) = {
    databaseToFile("caviar-train", "/home/nkatz/Desktop/caviar-train")
  }

  /* Helper method for dumping a mongo DB to a file OLED can read. */

  def databaseToFile(dbName: String, fileName: String) = {
    val mongoClient = MongoClient()
    val collection = mongoClient(dbName)("examples")
    collection.find().foreach{ x =>
      val e = Example(x)
      println(e)
    }
  }

  /* Read data from a file */

  def getDataFromFile(path: String) = {

  }

}

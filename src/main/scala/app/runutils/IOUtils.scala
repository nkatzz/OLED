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

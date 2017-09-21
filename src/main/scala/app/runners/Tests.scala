package app.runners

import com.mongodb.BasicDBObject
import com.mongodb.casbah.Imports.MongoDBObject
import com.mongodb.casbah.MongoClient

/**
  * Created by nkatz on 7/9/17.
  */

object Tests extends App {

  val mongoClient = MongoClient()
  val collection = mongoClient("brest-1")("examples")

  val times = collection.find().sort(MongoDBObject("time" -> 1)).map(x => x.asInstanceOf[BasicDBObject].get("time").toString.toInt)

  //println(collection.count())

  var start = times.next()
  var count = 0
  var finished = false
  while(!finished) {
    val x = times.next()
    if ( x - start >= 300000 ) {
      println(count)
      finished = true
    }
    count += 1
  }

}

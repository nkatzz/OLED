package app.runners

import com.mongodb.BasicDBObject
import com.mongodb.casbah.Imports.MongoDBObject
import com.mongodb.casbah.MongoClient

/**
  * Created by nkatz on 7/9/17.
  */

object Tests extends App {

  splitString("lklvkjxlkjlkjlnvzlxnlkJlc,mn,fdnklandlakm.,c>Z<M", 10, Vector[String]()) foreach println


  def splitString(s: String, l: Int, chunks: Vector[String]): Vector[String] = {
    s.length > l match {
      case true =>
        val first = s.splitAt(l)
        splitString(first._2, l, chunks :+ first._1)
      case _ => chunks :+ s
    }
  }


}

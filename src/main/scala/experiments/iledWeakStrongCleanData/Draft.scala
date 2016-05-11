package experiments.iledWeakStrongCleanData

import java.io.File

import com.mongodb.casbah.Imports._
import com.mongodb.casbah.commons.MongoDBObject
import iled.core.Core
import iled.core.Iled._
import iled.structures.Examples.{ExampleBatch, Example}
import iled.structures.PriorTheory
import iled.utils.{ASP, Database}
import iled.utils.Utils._
import jep.Jep


/**
  * Created by nkatz on 12/15/15.
  */
object Draft extends App{

  val file = "/home/nkatz/Desktop/kernel.txt"
  def write(narrative: List[String]) = iled.utils.Utils.writeLine("\n#include "+"\""+Core.bkFile+"\".\n" + narrative.mkString("\n"),file,"overwrite")
  val _write = (narrative: List[String]) => write(narrative)
  val show: String => Unit = (x: String) => println(show)
  val notHLE = (x: String) => !x.contains("meeting") && !x.contains("moving") && !x.contains("fighting") && !x.contains("leavingObject")
  val isHLE = (x: String) => x.contains("meeting") || x.contains("moving") || x.contains("fighting") || x.contains("leavingObject")
  val db = new Database(Core.fromDB)
  val jep = new Jep()

  var previous = List[String]()


  for (x <- db.collection.find().sort(MongoDBObject("time" -> 1))) {
    val e = Example(x)
    val count = x.asInstanceOf[BasicDBObject].get("count").asInstanceOf[Int]
    val time = e.time.toInt
    if (time == 657000) {
      val stop = "stop"
    }
    val nextTime = time + 40
    val query = MongoDBObject("time" -> nextTime)
    val nextExample = db.collection.findOne(query).get
    val narrative1 = e.narrative.map(_+".")
    val narrative2 = nextExample.asInstanceOf[BasicDBObject].get("narrative").asInstanceOf[BasicDBList].toList.map(x => s"$x.")
    val narrative = narrative1 ++ narrative2 ++ previous.map(_+".")
    val y = _write(narrative)
    val out = ASP.solve("inference",aspInputFile = new File(file), jep = jep)
    previous =
      if (out.nonEmpty){
        out.head.atoms.filter(x => x.contains(nextTime.toString)) map { y => if (isHLE(y)) s"example($y)" else y}
      } else {
        List()
      }
    val current = if (out.nonEmpty) out.head.atoms.filter(x => x.contains(time.toString) && notHLE(x)) else List[String]()
    val currentAnnot = if (out.nonEmpty) out.head.atoms.filter(x => x.contains(time.toString) && isHLE(x)) else List[String]()
    val newNarrative = e.narrative ++ current
    val newAnnotation = currentAnnot
    val updatedRecord = MongoDBObject("count" -> count) ++ ("time" -> time) ++ ("annotation" -> newAnnotation) ++ ("narrative" -> newNarrative)

    val update = db.collection.update( MongoDBObject("time" -> time), updatedRecord)
    println(updatedRecord)
    println(time,update)
  }

  /*
  val narrative =
    for (x <- db.collection.find() ;
         e = Example(x) ;
         narrative = e.narrative.map(_+".") ;
         y = _write(narrative) ;
         out = ASP.solve("inference",aspInputFile = new File(file), jep = jep) ;
         k = show(out.head.atoms.mkString(" "))) yield out.head.atoms.mkString(" ")
 */
}

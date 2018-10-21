package datautils.tracknow

import java.io.File

import scala.io.Source
import scala.util.Try

object Helper extends App {

  def rename(oldName: String, newName: String) = {
    Try(new File(oldName).renameTo(new File(newName))).getOrElse(false)
  }

  /*
  val RTECDataPath = "/media/nkatz/storage/Zelitron-data/RTEC_data"

  val dir = new File(RTECDataPath)

  var types = Set[String]()

  val t0 = System.nanoTime()

  dir.listFiles.sortBy( x => x.getName.split("-")(0).toInt ) foreach { f =>
    val source = Source.fromFile(f.getAbsolutePath)
    try {
      val lines = source.getLines
      lines foreach { x =>
        val ttype = x.split("\\|")(4)
        if (!types.contains(ttype)) types = types + ttype
        //println(types)
      }
    } finally { source.close() }
  }

  println(types)

  val t1 = System.nanoTime()
  println(s"Total time: ${(t1-t0)/1000000000.0}")
  */

  /*
  We need:
  - Total number of LLEs
  - Mean number of LLEs
  - Total number of HLEs
  - Mean number of HLEs
  - Total time
  - Mean time per batch
   */


  def f(s: String) = {
    val z = s.split(": ")(1).split("/")
    (z(0), z(1))
  }

  val resultsFile = "/home/nkatz/Desktop/ZEL-RTEC-results"
  val source = Source.fromFile(new File(resultsFile))
  try {
    val lines = source.getLines
    val results = lines.foldLeft(0.0,List[Double](),0.0,List[Double](),0.0,List[Double]()) { (x, y) =>
      val (llesTotal, llesAvg, hlesTotal, hlesAvg, timeTotal, timeAvg) = (x._1, x._2, x._3, x._4, x._5, x._6)
      if (y.contains("Total/Average number of input LLEs:")) {
        val (total, avg) = f(y)
        (llesTotal+total.toDouble, llesAvg :+ avg.toDouble, hlesTotal, hlesAvg, timeTotal, timeAvg)
      } else if (y.contains("Total/Average number of HLE instances:")) {
        val (total, avg) = f(y)
        (llesTotal, llesAvg, hlesTotal+total.toDouble, hlesAvg:+ avg.toDouble, timeTotal, timeAvg)
      } else if (y.contains("Total/Average time:")) {
        val (total, avg) = f(y)
        (llesTotal, llesAvg, hlesTotal, hlesAvg, timeTotal+total.toDouble, timeAvg:+ avg.toDouble)
      } else {
        (llesTotal, llesAvg, hlesTotal, hlesAvg, timeTotal, timeAvg)
      }
    }
    println(s"Total LLEs number: ${results._1}")
    println(s"Average LLEs number per batch: ${results._2.sum/results._2.length}")
    println(s"Total HLEs number: ${results._3}")
    println(s"Average HLEs number per batch: ${results._4.sum/results._4.length}")
    println(s"Total Time: ${results._5}")
    println(s"Average time per batch: ${results._6.sum/results._6.length}")
  } finally {
    source.close()
  }

}

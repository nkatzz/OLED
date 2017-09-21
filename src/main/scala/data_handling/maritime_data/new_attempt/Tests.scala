package data_handling.maritime_data.new_attempt

import scala.collection.immutable.HashSet
import scala.io.Source

/**
  * Created by nkatz on 7/4/17.
  */

object Tests extends App {

  val path = "/home/nkatz/dev/maritime/nkatz_brest/1-core"
  private val datasetFile = path+"/dataset.txt" // The LLEs file
  private val stoppedFile = path+"/recognition/stopped.csv"
  val times = HashSet() ++ Runner.getAllTimes()

  println(times.size)

  val data = Source.fromFile(stoppedFile).getLines.filter(x => !x.contains("inf"))

  data foreach { x =>
    val s = x.split("\\|")
    val startTime = s(3).toInt
    val endTime = s(4).toInt - 1
    val vessel = s(1)
    val interval  = for (x <- startTime to endTime if times.contains(x)) yield x
    val l = interval.length
    if (l > 40000) {
      println(l, vessel)
    }

  }

}

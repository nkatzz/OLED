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

package experiments.datautils.datacron_5_2018_deliv

import java.io.{BufferedWriter, File, FileWriter}

import intervalTree.IntervalTree

import scala.collection.JavaConversions._
import data._
import logic.Modes.ModeAtom

import scala.collection.mutable.ListBuffer
import scala.io.Source


/*
change_in_heading|1443694493|1443694493|245257000
change_in_speed_start|1443890320|1443890320|259019000
change_in_speed_end|1443916603|1443916603|311018500
coord|1443686670|1443686670|228041600|-4.47298500000000043286|48.38163999999999731472
entersArea|1443875806|1443875806|228017700|18515
leavesArea|1443887789|1443887789|235113366|21501
gap_start|1444316024|1444316024|246043000
gap_end|1445063602|1445063602|269103970
proximity|1445357304|1445354425|1445357304|true|227519920|227521960
velocity|1443665064|1443665064|228374000|19.30468943370810563920|31.39999999999999857891|35.54927442329611153582
slow_motion_start|1444262324|1444262324|228167900
slow_motion_end|1444281397|1444281397|258088080
stop_start|1444031990|1444031990|227705102
stop_end|1444187303|1444187303|259019000
 */

// lle = split(0)
// time = split(1)
// vessel = split(3)




object IntervalHandler extends App {

  /*
  val it = new IntervalTree[Int]()
  it.addInterval(0L,10L,1)
  it.addInterval(20L,30L,2)
  it.addInterval(15L,17L,3)
  it.addInterval(25L,35L,4)
  it += (0L, 29L, 4)
  println(it.range(9L, 30L))
  */

  //val intervalTree = generateIntervalTree("/home/nkatz/dev/Brest-data-5-5-2018/results_critical", List("rendezVous","stopped", "lowSpeed"))

  val intervalTree = generateIntervalTree("/home/nkatz/dev/Brest-data-5-5-2018/brest-no_duration_constraints/results_critical",
    List("rendezVous","stopped", "lowSpeed"))

  ///home/nkatz/dev/Brest-data-5-5-2018/brest-no_duration_constraints/results_critical

  //readDataIntoMiniBatches("/home/nkatz/dev/Brest-data-5-5-2018/Datasets/dataset_RTEC_critical__vagms_preprocess_prox.db", 10, "rendezVous", "asp")

  readDataIntoMiniBatches("/home/nkatz/dev/Brest-data-5-5-2018/Brest-data/Datasets/dataset_RTEC_critical.csv", 10, "rendezVous", "asp")



  // mode is either "asp" or "mln"
  def readDataIntoMiniBatches(dataPath: String, batchSize: Int, targetEvent: String, mode: String) = {

    val f = new File("/home/nkatz/dev/Brest-data-5-5-2018/rendezVous-batchsize-10")
    val writeToFile = new BufferedWriter(new FileWriter(f))

    val data = Source.fromFile(dataPath).getLines.filter(x => !x.startsWith("coord") && !x.startsWith("velocity"))
    val currentBatch = new ListBuffer[String]
    var timesAccum = scala.collection.mutable.SortedSet[Long]()
    var llesAccum = scala.collection.mutable.SortedSet[String]()
    var batchCount = 0
    while(data.hasNext) {
      val newLine = data.next()
      val split = newLine.split("\\|")
      println(split.mkString(" "))
      val time = split.last
      val lle = split.head
      if (!timesAccum.contains(time)) timesAccum += time.toLong
      if (!llesAccum.contains(lle)) llesAccum += lle
      if (timesAccum.size <= batchSize) {
        //currentBatch += newLine
        currentBatch += generateLLEInstances(newLine, mode)
      } else {

        batchCount += 1
        val nexts = timesAccum.sliding(2).map(x => if (mode == "asp") s"next(${x.last},${x.head})" else s"next(${x.last},${x.head})")
        val intervals = intervalTree.range(timesAccum.head, timesAccum.last)

        //val extras = intervals.map(x => HLEIntervalToAtom(x._3, time, targetEvent))

        val extras = timesAccum.flatMap{ timeStamp =>
          val containedIn = intervals.filter(interval => interval._1 <= timeStamp && timeStamp <= interval._2 )
          containedIn.map(x => HLEIntervalToAtom(x._3, timeStamp.toString, targetEvent))
        }

        for (x <- extras) currentBatch += x
        for (x <- nexts) currentBatch += x


        /*
        val (annotation, narrative) = currentBatch.foldLeft(List[String](), List[String]()) { (x, y) =>
          val (anot, nar) = (x._1, x._2)
          if (y.startsWith("holdsAt")) (anot :+ y, nar) else (anot, nar :+ y)
        }
        */

        writeToFile.write(currentBatch.filter(x => x != "None").mkString(" ")+"\n")

        println(batchCount)
        currentBatch.clear()
        timesAccum.clear()
      }
    }
    println(s"All batches: $batchCount")
    println(s"LLEs: $llesAccum")
    writeToFile.close()
  }







  // mode is either "asp" or "mln"
  def generateLLEInstances(line: String, mode: String) = {
    // These have a different schema
    val abnormalLLEs = Set[String]("coord", "entersArea", "leavesArea", "proximity", "velocity")
    val split = line.split("\\|")
    if (! abnormalLLEs.contains(split(0))) {

      // These have a common simple schema:
      // change_in_heading, change_in_speed_start, change_in_speed_end,
      // gap_start, gap_end, slow_motion_start, slow_motion_end, stop_start, stop_end
      val lle = split(0)
      val time = split(2)
      val vessel = split(1)
      if (mode == "asp") s"happensAt($lle($vessel),$time)" else s"HappensAt(${lle.capitalize}_$vessel),$time)"
    } else {

      if (split(0) == "coord") {
        //coord|1443686670|1443686670|228041600|-4.47298500000000043286|48.38163999999999731472
        /*
        val lle = split(0)
        val time = split(1)
        val vessel = split(3)
        val lon = split(4)
        val lat = split(5)
        // Don't return nothing in the MLN case (can't handle the coords syntax)
        if ("mode" == "asp") s"happensAt($lle($vessel,$lon,$lat),$time)" else ""
        */
        // do nothing (we won't use coord).
        "None"

      } else if (split(0) == "entersArea" || split(0) == "leavesArea") {
        //entersArea|1443875806|1443875806|228017700|18515
        val lle = split(0)
        val time = split(3)
        val vessel = split(1)
        val area = split(2)
        if (mode == "asp") s"happensAt($lle($vessel,$area),$time)"
        else s"HappensAt(${lle.capitalize}_${vessel}_$area,$time)"

      } else if (split(0) == "velocity") {

        // do nothing (we won't use velocity)
        "None"

      } else if (split(0) == "proximity") {
        val vessel1 = split(1)
        val vessel2 = split(2)
        val time = split(3)
        if (mode == "asp") s"happensAt(close($vessel1,$vessel2),$time)"
        else s"HappensAt(Close_${vessel1}_$vessel2,$time)"
      } else {
        throw new RuntimeException(s"Unexpected event: $line")
      }
    }
  }





  object HLEInterval {
    def apply(hleLine: String) = {
      val split = hleLine.split("\\|")
      val hle = split(0)
      // rendezVous, tugging
      if (Set("adrift", "aground", "atAnchor", "atAnchorOrMoored", "gap",
        "highSpeedNearCoast", "loitering", "lowSpeed", "maa", "moored", "speedGrThanMax",
        "speedLessThanMin", "stopped", "travelSpeed", "trawling", "trawlSpeed", "underWay", "unusualSpeed").contains(hle)) {
        val vessels = List(split(1))
        val value = split(2)
        val stime = split(3).toLong
        val etime = split(4).toLong
        new HLEInterval(hle, vessels, value, stime, etime)

      } else if (hle == "rendezVous" || hle == "tugging") {
        val vessels = List(split(1), split(2))
        val value = split(3)
        val stime = split(4).toLong
        val etime = split(5).toLong
        new HLEInterval(hle, vessels, value, stime, etime)

      } else if (hle == "withinArea") {
        //withinArea|923166|fishing|true|1448977130|1448977242
        val vessels = List(split(1))
        val value = split(2)
        val stime = split(4).toLong
        val etime = split(5).toLong
        new HLEInterval(hle, vessels, value, stime, etime)

      } else throw new RuntimeException(s"Don't know what to do with $hleLine")
    }
  }

  class HLEInterval(val hle: String, val vessels: List[String], val value: String, val stime: Long, val etime: Long)


  /* Generate an HLE logical atom. The i var carries all the info, the t var is the particular
   * time point of the generated atom. "target" is the name of the target complex event. The
   * target event is turned into a holdsAt predicate, while all others are turned into happensAt predicates. */
  def HLEIntervalToAtom(i: HLEInterval, t: String, target: String) = {

    val functor = if (i.hle == target) "holdsAt" else "happensAt"

    val fluentTerm =
      if (i.value != "true") s"${i.hle}(${(i.vessels :+ i.value).mkString(",")})"
      else s"${i.hle}(${i.vessels.mkString(",")})"

    s"$functor($fluentTerm,$t)"
  }


  def generateIntervalTree(pathToHLEs: String, interestedIn: List[String]) = {

    def getListOfFiles(dir: String):List[File] = {
      val d = new File(dir)
      if (d.exists && d.isDirectory) {
        d.listFiles.filter(f => f.isFile && interestedIn.exists(eventName => f.getName.contains(eventName))).toList
      } else {
        List[File]()
      }
    }

    val files = getListOfFiles(pathToHLEs).map(_.getCanonicalPath)

    val intervalTree = new IntervalTree[HLEInterval]()

    files foreach { file =>
      println(s"Updating interval tree from $file")
      val data = Source.fromFile(file).getLines
      while (data.hasNext) {
        val newLine = data.next()
        val i = HLEInterval(newLine)
        intervalTree.addInterval(i.stime, i.etime, i)
      }
    }

    intervalTree
  }







}


package object data {

  implicit class ITree[T](val tree: IntervalTree[T]) {

    def +=(from: Long, to: Long, data: T): Unit = tree.addInterval(from, to, data)

    def range(from: Long, to: Long): List[(Long, Long, T)] = {
      tree.getIntervals(from, to).toList.map { i =>
        if (from < i.getStart && to > i.getStart) (i.getStart, to, i.getData)
        else if (from >= i.getStart && to > i.getEnd) (from, i.getEnd, i.getData)
        else (from, to, i.getData)
      }
    }

  }
}
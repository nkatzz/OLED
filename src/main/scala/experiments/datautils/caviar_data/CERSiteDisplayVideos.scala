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

package experiments.datautils.caviar_data

import java.io.{BufferedWriter, File, FileWriter, PrintWriter}

import app.runutils.{CMDArgs, Globals, RunningOptions}
import experiments.caviar.FullDatasetHoldOut
import experiments.caviar.FullDatasetHoldOut.MongoDataOptions
import logic.Examples.Example
import logic.{Literal, Theory}
import oled.functions.SingleCoreOLEDFunctions.crossVal
import utils.{ASP, Utils}
import utils.Implicits._

import scala.collection.mutable

object CERSiteDisplayVideos {

  private val videos =
    Vector("caviar-video-1-meeting-moving", "caviar-video-2-meeting-moving", "caviar-video-3",
      "caviar-video-4", "caviar-video-5", "caviar-video-6", "caviar-video-7", "caviar-video-8",
      "caviar-video-9", "caviar-video-10", "caviar-video-11", "caviar-video-12-moving", "caviar-video-13-meeting",
      "caviar-video-14-meeting-moving", "caviar-video-15", "caviar-video-16", "caviar-video-17", "caviar-video-18",
      "caviar-video-19-meeting-moving", "caviar-video-20-meeting-moving", "caviar-video-21-meeting-moving",
      "caviar-video-22-meeting-moving", "caviar-video-23-moving", "caviar-video-24-meeting-moving", "caviar-video-25",
      "caviar-video-26", "caviar-video-27", "caviar-video-28-meeting", "caviar-video-29", "caviar-video-30")

  def main(args: Array[String]) = {
    val argsok = CMDArgs.argsOk(args)
    if (!argsok._1) {
      println(argsok._2)
      System.exit(-1)
    } else {
      val runningOptions = CMDArgs.getOLEDInputArgs(args)
      runHandCrafted(runningOptions)
    }
  }

  def runHandCrafted(runOpts: RunningOptions) = {
    var (totalTPs, totalFPs, totalFNs) = (0, 0, 0)
    videos.filter(x => x.contains("meeting") || x.contains("moving")) foreach { video =>
      println(video)
      val videoNum = video.split("caviar-video-")(1).split("-")(0)

      println(videoNum)
      val testingDataOptions = new MongoDataOptions(dbNames = Vector(video), chunkSize = runOpts.chunkSize, sortDbByField = "time", what = "testing")

      val testingDataFunction: MongoDataOptions => Iterator[Example] = FullDatasetHoldOut.getMongoData

      val data = testingDataFunction(testingDataOptions).next()

      // This returns only the detected CE instances. Comment it out if you only want the tp/fn/fn counts.
      val atomsMeeting = evaluate(data, "/home/nkatz/Desktop/hand-crafted-meeting", runOpts.globals, runOpts, "meeting")
      val atomsMoving = evaluate(data, "/home/nkatz/Desktop/hand-crafted-moving", runOpts.globals, runOpts, "moving")
      val recognizedIntervalsMeeting = findCEIntervals(atomsMeeting)
      val recognizedIntervalsMoving = findCEIntervals(atomsMoving)

      //println(recognizedIntervalsMeeting)
      //println(recognizedIntervalsMoving)

      val annotationAtoms = data.annotation
      val (meetAnnotAtoms, moveAnnotAtoms) = annotationAtoms.foldLeft(List[String](), List[String]()) { (x, y) =>
        if (y.contains("meeting")) {
          (x._1 :+ y, x._2)
        } else if (y.contains("moving")) {
          (x._1, x._2 :+ y)
        } else {
          x
        }
      }

      val groundTruthIntervalsMeeting = findCEIntervals(meetAnnotAtoms)
      val groundTruthIntervalsMoving = findCEIntervals(moveAnnotAtoms)

      println(recognizedIntervalsMeeting)
      println(groundTruthIntervalsMeeting)
      println(recognizedIntervalsMoving)
      println(groundTruthIntervalsMoving)

      val recognizedFile = new File(s"/home/nkatz/dev/CER-site-caviar-videos/video-$videoNum-OLED")
      val bw1 = new BufferedWriter(new FileWriter(recognizedFile))

      val groundTruthFile = new File(s"/home/nkatz/dev/CER-site-caviar-videos/video-$videoNum-GroundTruth")
      val bw2 = new BufferedWriter(new FileWriter(groundTruthFile))

      //val recognizedFile = new PrintWriter(new File(s"s/home/nkatz/dev/CER-site-caviar-videos/video-$videoNum-OLED" ))
      //val groundTruthFile = new PrintWriter(new File(s"s/home/nkatz/dev/CER-site-caviar-videos/video-$videoNum-GroundTruth.txt" ))

      writeIntervals(bw1, recognizedIntervalsMeeting)
      writeIntervals(bw1, recognizedIntervalsMoving)

      writeIntervals(bw2, groundTruthIntervalsMeeting)
      writeIntervals(bw2, groundTruthIntervalsMoving)

      //pw.write("Hello, world")
      bw1.close()
      bw2.close()

      // This returns only the tp/fn/fn counts. Comment it out if you only want the detected CE instances.
      /*
      val (tps, fps, fns, _, _, _) =
        crossVal(Theory(), data = testingDataFunction(testingDataOptions), handCraftedTheoryFile = runOpts.evalth, globals = runOpts.globals, inps = runOpts)

      println(tps, fps, fns)

      totalTPs += tps
      totalFPs += fps
      totalFNs += fns
      */
    }

    //val precision = totalTPs.toDouble/(totalTPs+totalFPs)
    //val recall = totalTPs.toDouble/(totalTPs+totalFNs)
    //val f1score = 2*precision*recall/(precision+recall)
    //println(s"sMicro F1-score: $f1score")

  }

  def writeIntervals(file: BufferedWriter, data: Map[String, List[scala.List[Int]]]) = {
    data foreach { x =>
      val parsed = Literal.parse(x._1)
      val ids = parsed.terms.map(_.tostring.split("id")(1)).mkString(",")
      val first = s"${parsed.predSymbol.tostring}($ids)"
      val second = x._2.map(y => s"(${(y.head/40.0).toInt}, ${(y.last/40.0).toInt})" ).mkString(",")
      val msg = s"$first:[$second]"
      file.write(msg+"\n")
    }
  }

  def evaluate(data: Example,
               handCraftedTheoryFile: String = "",
               globals: Globals,
               inps: RunningOptions,
               CE: String) = {

    val e = data // only 1 entry in the iterator (no chunking)
    val t = globals.INCLUDE_BK(handCraftedTheoryFile)
    val show = s"#show.\n#show holdsAt($CE(X0,X1),X2):holdsAt($CE(X0,X1),X2)."
    val ex = e.tostring
    val program = ex + globals.INCLUDE_BK(globals.BK_CROSSVAL) + t + show
    val f = Utils.getTempFile("isConsistent",".lp")
    Utils.writeLine(program, f, "overwrite")
    val answerSet = ASP.solve(task = Globals.INFERENCE, aspInputFile = f)
    f.delete()
    if (answerSet.nonEmpty) {
      val atoms = answerSet.head.atoms
      atoms
    } else {
      Nil
    }
  }

  def findCEIntervals(ceAtoms: List[String]) = {

    val parsed = ceAtoms.map(atom => Literal.parse(atom))
    val groupedByCEInstance = parsed.groupBy(x => x.terms.head.tostring).map { case (k, v) =>
      //val transform = v.sortBy(x => x.terms.tail.head.tostring.toInt)//.map(_.tostring)
      val times = v.map(x => x.terms.tail.head.tostring.toInt).sorted
      val timesToIntervals = times.tail.foldLeft(times.head, mutable.Stack[List[Int]]()) { (x, currentTime) =>
        val (prevTimePoint, intervals) = (x._1, x._2)
        if (currentTime == prevTimePoint + 40) {
          val last = if (intervals.nonEmpty) intervals.pop() else List.empty[Int]
          if (intervals.size > 1) {
            val stop = "stop"
          }
          val last_+ = last :+ currentTime
          intervals.push(last_+)
          (currentTime, intervals)
        } else {
          (currentTime, intervals)
        }
      }
      (k, timesToIntervals._2.toList)
    }
    groupedByCEInstance

  }

}

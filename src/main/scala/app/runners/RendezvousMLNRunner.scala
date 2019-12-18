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

package app.runners

import java.io.File

import akka.actor.{ActorSystem, Props}
import app.runners.RendezvousMLNRunner.RendezvousMLNDataHandler.RendezvousDataOptions
import app.runutils.CMDArgs
import logic.Examples.Example
import oled.single_core.Dispatcher

import scala.io.Source

object RendezvousMLNRunner {

  /*
  *
  * HappensAt(Change_in_heading_227569380, 776) --> happensAt(change_in_heading("227569380"), 776)
  * HappensAt(Change_in_speed_end_227315110, 774) --> happensAt(change_in_speed_end("227315110"), 774)
  * HappensAt(Change_in_speed_start_227315110, 773) --> happensAt(change_in_speed_start("227315110"), 773)
  * HappensAt(Gap_end_227315110, 770) --> happensAt(gap_end("227315110"), 770)
  * HappensAt(Gap_start_227569380, 769) --> happensAt(gap_start("227569380", 769)
  * HappensAt(Proximity_227315110_227569380, 780) --> happensAt(proximity("227315110","227569380", 780)
  * HappensAt(Slow_motion_start_227315110, 751) --> happensAt(slow_motion_start("227315110"), 751)
  * HappensAt(Slow_motion_end_227315110, 814) ->> happensAt(slow_motion_end("227315110"), 814)
  * HappensAt(Stop_start_227315110, 730) --> happensAt(stop_start("227315110"), 730)
  * HappensAt(Stop_end_227315110, 731) --> happensAt(stop_end("227315110"), 731)
  * HappensAt(Velocity_6_7_227315110, 731) --> happensAt(velocity_6_7_227315110, 731)
  * HoldsAt(Rendezvous_227362110_227069470, 838263) --> holdsAt(rendezvous("227362110","227069470", 838263)
  *
  * */

  def main(args: Array[String]) = {

    val argsok = CMDArgs.argsOk(args)

    if (!argsok._1) {
      println(argsok._2)
      System.exit(-1)
    } else {
      val foldPath = args.map(x => x.split("=")).find(x => x(0) == "--foldpath").getOrElse(throw new RuntimeException("--foldpath missing."))(1)
      val inps = CMDArgs.getOLEDInputArgs(args)
      val trainingDataOptions = new RendezvousDataOptions(foldPath, inps.chunkSize)
      val testingDataOptions = trainingDataOptions

      val trainingDataFunction: RendezvousDataOptions => Iterator[Example] = RendezvousMLNDataHandler.getTrainingData
      val testingDataFunction: RendezvousDataOptions => Iterator[Example] = RendezvousMLNDataHandler.getTestingData
      val system = ActorSystem("HoeffdingLearningSystem")
      val msg = "start"
      //val msg = "EvaluateHandCrafted"
      system.actorOf(Props(new Dispatcher(inps, trainingDataOptions, testingDataOptions, trainingDataFunction, testingDataFunction)), name = "Learner") ! msg
    }
  }

  object RendezvousMLNDataHandler {

    class RendezvousDataOptions(val foldPath: String, val chunkSize: Int) extends app.runutils.IOHandling.MongoSource

    def isPred(s: String) = s.startsWith("HoldsAt") || s.startsWith("HappensAt")

    def getTrainingData(opts: RendezvousDataOptions) = {
      var i = 0
      val trainingSetPath = s"${opts.foldPath}/training/batch"
      val trainingFile = new File(trainingSetPath).listFiles.head
      val chunkSize = opts.chunkSize
      Source.fromFile(trainingFile.getCanonicalPath).getLines.sliding(chunkSize, chunkSize - 1) map { chunk =>
        val (narrative, annotation) = chunk.flatMap(x => x.split("\\|").map(x => convert(x))).foldLeft(List[String](), List[String]()) { (accum, y) =>
          if (y.contains("happens")) (accum._1 :+ y, accum._2) else (accum._1, accum._2 :+ y)
        }
        i += 1
        new Example(annot = annotation, nar = narrative, _time = i.toString)
      }
    }

    def getTestingData(opts: RendezvousDataOptions) = {
      val testingSetPath = s"${opts.foldPath}/testing"
      val annotationPath = s"${new File(new File(testingSetPath).getParent).getParent}/annotation"
      val innerFiles = new File(testingSetPath).listFiles
      val testingFilesNames = innerFiles.map(f => f.getName.substring(0, f.getName.lastIndexOf("."))).toList

      val exmpls = testingFilesNames.foldLeft(List[Example]()) { (p, testingFile) =>
        val narrative = Source.fromFile(testingSetPath + "/" + testingFile + ".db").getLines.toList.filter(isPred _)
        // Find the proper annotation file
        val annotFile = new File(annotationPath).listFiles.toList.filter(f => f.getName.contains(testingFile)).head
        val annotation = Source.fromFile(annotFile).getLines.toList.filter(p => isPred(p)).map(_.replaceAll("\\s", ""))
        val exmpl = new Example(annot = annotation.map(x => convert(x)), nar = narrative.map(x => convert(x)), _time = "-")
        p :+ exmpl
      }
      exmpls.toIterator
    }

  }

  def getTrainingData(dataPath: String, chunkSize: Int) = {
    var i = 0
    Source.fromFile(dataPath).getLines.sliding(chunkSize, chunkSize - 1) map { chunk =>
      val (narrative, annotation) = chunk.flatMap(x => x.split("\\|").map(x => convert(x))).foldLeft(List[String](), List[String]()) { (accum, y) =>
        if (y.contains("happens")) (accum._1 :+ y, accum._2) else (accum._1, accum._2 :+ y)
      }
      i += 1
      new Example(annot = annotation, nar = narrative, _time = i.toString)
    }
  }

  def split1(in: String) = {
    val split = in.split(",")
    val time = split(1).split("\\)")(0).trim
    val pred = split(0).split("\\(")(1)
    val vessel = pred.split("_").reverse.head.trim
    (vessel, time)
  }

  def split2(in: String) = {

    try {
      val split = in.split(",")
      val time = split(1).split("\\)")(0).trim
      val pred = split(0).split("\\(")(1)
      val predSplit = pred.split("_")
      val vessel1 = predSplit(1)
      val vessel2 = predSplit(2)
      (vessel1, vessel2, time)
    } catch {
      case e: ArrayIndexOutOfBoundsException => println(in)
    }

  }

  def split3(in: String) = {
    val split = in.split(",")
    val time = split(1).split("\\)")(0).trim
    val pred = split(0).split("\\(")(1)
    val predSplit = pred.split("_")
    val (lower, upper, vessel) = (predSplit(1), predSplit(2), predSplit(3))
    (lower, upper, vessel, time)
  }

  def convert(in: String) = {
    if (in.contains("Change_in_heading")) {

      val (vessel, time) = split1(in)
      s"""happensAt(change_in_heading("$vessel"),$time)"""

    } else if (in.contains("Change_in_speed_end")) {

      val (vessel, time) = split1(in)
      s"""happensAt(change_in_speed_end("$vessel"),$time)"""

    } else if (in.contains("Change_in_speed_start")) {

      val (vessel, time) = split1(in)
      s"""happensAt(change_in_speed_start("$vessel"),$time)"""

    } else if (in.contains("Gap_end")) {

      val (vessel, time) = split1(in)
      s"""happensAt(gap_end("$vessel"),$time)"""

    } else if (in.contains("Gap_start")) {

      val (vessel, time) = split1(in)
      s"""happensAt(gap_start("$vessel"),$time)"""

    } else if (in.contains("Proximity")) {

      val (vessel1, vessel2, time) = split2(in)
      s"""happensAt(proximity("$vessel1","$vessel2"),$time)"""

    } else if (in.contains("Slow_motion_start")) {

      val (vessel, time) = split1(in)
      s"""happensAt(slow_motion_start("$vessel"),$time)"""

    } else if (in.contains("Slow_motion_end")) {

      val (vessel, time) = split1(in)
      s"""happensAt(slow_motion_end("$vessel"),$time)"""

    } else if (in.contains("Stop_start")) {

      val (vessel, time) = split1(in)
      s"""happensAt(stop_start("$vessel"),$time)"""

    } else if (in.contains("Stop_end")) {

      val (vessel, time) = split1(in)
      s"""happensAt(stop_end("$vessel"),$time)"""

    } else if (in.contains("Velocity")) {

      val (lower, upper, vessel, time) = split3(in)
      s"""happensAt(velocity($lower, $upper, "$vessel"),$time)"""

    } else if (in.contains("HoldsAt")) {

      val (vessel1, vessel2, time) = split2(in)
      s"""holdsAt(rendezvous("$vessel1","$vessel2"),$time)"""

    } else {
      throw new RuntimeException(s"Don't know what to do with $in")
    }
  }

}

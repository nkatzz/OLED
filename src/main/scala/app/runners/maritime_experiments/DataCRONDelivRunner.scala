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

package app.runners.maritime_experiments

import akka.actor.{ActorSystem, Props}
import app.runners.OLEDDefaultRunner.{DefaultMongoDataOptions, getMongoData}
import app.runners.maritime_experiments.DataCRONDelivRunner.dataPath
import app.runutils.CMDArgs
import logic.Examples.Example
import oled.single_core.Master

import scala.io.Source

object DataCRONDelivRunner {

  val dataPath = "/home/nkatz/dev/Brest-data-5-5-2018/rendezVous-batchsize-10"

  def main(args: Array[String]) = {
    ///*
    val argsok = CMDArgs.argsOk(args)
    if (!argsok._1) {
      println(argsok._2)
      System.exit(-1)
    } else {
      val runningOptions = CMDArgs.getOLEDInputArgs(args)

      //val trainingDataOptions = new DataOptions(dataPath, take = 15000)
      val trainingDataOptions = new DataOptions(dataPath, take = 200000)

      //val testingDataOptions = new DataOptions(dataPath, drop = 15000)
      //val testingDataOptions = new DataOptions(dataPath, drop = 30000)

      val testingDataOptions = new DataOptions(dataPath)

      val trainingDataFunction: DataOptions => Iterator[Example] = getData

      val testingDataFunction: DataOptions => Iterator[Example] = getData

      val system = ActorSystem("HoeffdingLearningSystem")
      val startMsg = if (runningOptions.evalth != "None") "eval" else "start"

      system.actorOf(Props(new Master(runningOptions, trainingDataOptions, testingDataOptions,
                                      trainingDataFunction, testingDataFunction)), name = "Master-Actor") ! startMsg
    }
    //*/

    /*
    val data = getData(new DataOptions(dataPath))
    data foreach { x =>

      if (x.narrative.exists(p => p.contains("stop_start"))) println(x.time)

      if (x.time == "1924") {
        val stop = "stop"
      }

      if (x.annotation.nonEmpty) {
        println(x)
        println("")
      }
    }
    */

  }

  private class DataOptions(val dataFilePath: String, val take: Int = 0, val drop: Int = 0) extends app.runutils.IOHandling.InputSource

  def getData(opts: DataOptions) = {
    val dataPath = opts.dataFilePath
    val data = Source.fromFile(dataPath).getLines
    var i = 0
    val iter = data map { currentBatch =>
      val batch = currentBatch.split(" ")
      val (annotation, narrative) = batch.foldLeft(List[String](), List[String]()) { (x, y) =>
        val (anot, nar) = (x._1, x._2)
        if (y.startsWith("holdsAt")) (anot :+ y, nar) else (anot, nar :+ y)
      }
      i += 1
      // drop the first annotation atom: Because rendezVous is defined with holdsFor...
      val _annotation = if (annotation.length > 1) annotation.drop(1) else annotation
      new Example(annot = annotation, nar = narrative, _time = i.toString)
    }
    if (opts.take != 0) {
      iter.take(opts.take)
    } else if (opts.drop != 0) {
      iter.drop(opts.drop)
    } else {
      iter
    }

  }

}

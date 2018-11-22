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

package experiments.datautils.maritime_data

import java.io.{File, PrintWriter}

import experiments.datautils.maritime_data.FinalVersionWithTimeFlattening.getAllTimes

import scala.collection.mutable.ListBuffer
import scala.io._

/**
  * Created by nkatz on 6/27/17.
  */

object CheckOutInfs {


  def main(args: Array[String]) = {
    val path = args(0)
    val hle = args(1)
    // e.g:
    // path = "/home/nkatz/dev/maritime/nkatz_brest/1-core/recognition/sailing.csv"
    // hle = "sailing
    handleInfs(path, hle)
  }



  def handleInfs(path: String, hle: String) = {

    def getStartTime(splittedLine: Array[String], hle: String) = {
      if (List("highSpeedIn", "withinArea", "rendezVouz").contains(hle)) {
        splittedLine(4).toInt
      } else {
        splittedLine(3).toInt
      }
    }

    def getVessel(splittedLine: Array[String]) = splittedLine(1)

    val newPath = path.split("\\.")(0)+"-no-infs.csv"
    utils.Utils.clearFile(newPath)
    val pw = new PrintWriter(new File(newPath))
    val times = getAllTimes()
    val lastTimePoint = times.last

    val infs = Source.fromFile(path).getLines.filter { x => x.split("\\|").last == "inf"}.toVector.distinct
    val nonInfs = Source.fromFile(path).getLines.filter { x => x.split("\\|").last != "inf"}.toVector.distinct

    val buffer = ListBuffer[String]()

    nonInfs.foreach(x => pw.write(x+"\n"))

    for (line <- infs) {
      val splitted = line.split("\\|")
      val vessel = getVessel(splitted)
      val startTime = getStartTime(splitted, hle)
      // Check if the inf closes at some point
      val nonInfLines = Source.fromFile(path).getLines.filter { x => x.split("\\|").last != "inf"}.toList.distinct
      val infClosingLine = nonInfLines.find { otherLine =>
        val otherLineSplitted = otherLine.split("\\|")
        val (v, t) = (getVessel(otherLineSplitted), getStartTime(otherLineSplitted, hle))
        v == vessel && t == startTime
      }.getOrElse("None")
      if (infClosingLine == "None") {
        // If it's not "None" th inf interval closes.
        // We don't have to copy this inf line in this case, we just throw it away.
        // But is it is "None", then the inf interval does not close, so we have to
        // replace the 'inf' in the interval with the last time point in the data.
        // The same line with the same inf interval may appear over and over again.
        // We use a buffer to avoid re-writting it multiple times.
        if (!buffer.contains(vessel)) {
          val lineToReplace = splitted.take(splitted.length - 1).mkString("|")+s"|$lastTimePoint"
          pw.write(lineToReplace+"\n")
          println(vessel)
          buffer += vessel
        }
      }
    }
    pw.close()
  }






}

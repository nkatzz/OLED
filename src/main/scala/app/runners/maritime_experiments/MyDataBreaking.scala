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

import java.io.{File, PrintWriter}

import scala.io.Source

/**
  * Created by nkatz on 7/10/17.
  */

object MyDataBreaking {

  def splitN[A](list: List[A], n: Int): List[List[A]] =
    if(n == 1) List(list) else List(list.head) :: splitN(list.tail, n - 1)



  def main(args: Array[String]) = {
    val vessels = getAllVessels("/home/nkatz/dev/maritime/brest-data/datasets/dataset1.txt")
    println(vessels)
    println(vessels.size)
    val partitioned2 = getVesselSplit(vessels, 2).toList
    val partitioned4 = getVesselSplit(vessels, 4).toList
    val partitioned8 = getVesselSplit(vessels, 8).toList

    //generateSplitData(partitioned2, "/home/nkatz/dev/maritime/brest-data/datasets-my-split", 2)
    //generateSplitData(partitioned4, "/home/nkatz/dev/maritime/brest-data/datasets-my-split", 4)
    //generateSplitData(partitioned8, "/home/nkatz/dev/maritime/brest-data/datasets-my-split", 8)

    geterateSplitData_Annotation(partitioned2,
                                 "/home/nkatz/dev/maritime/brest-data/recognition/1/highSpeedIn.csv",
                                 "/home/nkatz/dev/maritime/brest-data/recognition-my-split",
                                 "highSpeedIn",2)

    geterateSplitData_Annotation(partitioned4,
      "/home/nkatz/dev/maritime/brest-data/recognition/1/highSpeedIn.csv",
      "/home/nkatz/dev/maritime/brest-data/recognition-my-split",
      "highSpeedIn",4)

    geterateSplitData_Annotation(partitioned8,
      "/home/nkatz/dev/maritime/brest-data/recognition/1/highSpeedIn.csv",
      "/home/nkatz/dev/maritime/brest-data/recognition-my-split",
      "highSpeedIn",8)



    geterateSplitData_Annotation(partitioned2,
      "/home/nkatz/dev/maritime/brest-data/recognition/1/close_to_ports.csv",
      "/home/nkatz/dev/maritime/brest-data/recognition-my-split",
      "close_to_ports",2)

    geterateSplitData_Annotation(partitioned4,
      "/home/nkatz/dev/maritime/brest-data/recognition/1/close_to_ports.csv",
      "/home/nkatz/dev/maritime/brest-data/recognition-my-split",
      "close_to_ports",4)

    geterateSplitData_Annotation(partitioned8,
      "/home/nkatz/dev/maritime/brest-data/recognition/1/close_to_ports.csv",
      "/home/nkatz/dev/maritime/brest-data/recognition-my-split",
      "close_to_ports",8)




    geterateSplitData_Annotation(partitioned2,
      "/home/nkatz/dev/maritime/brest-data/recognition/1/stopped.csv",
      "/home/nkatz/dev/maritime/brest-data/recognition-my-split",
      "stopped",2)

    geterateSplitData_Annotation(partitioned4,
      "/home/nkatz/dev/maritime/brest-data/recognition/1/stopped.csv",
      "/home/nkatz/dev/maritime/brest-data/recognition-my-split",
      "stopped",4)

    geterateSplitData_Annotation(partitioned8,
      "/home/nkatz/dev/maritime/brest-data/recognition/1/stopped.csv",
      "/home/nkatz/dev/maritime/brest-data/recognition-my-split",
      "stopped",8)





    geterateSplitData_Annotation(partitioned2,
      "/home/nkatz/dev/maritime/brest-data/recognition/1/lowSpeed.csv",
      "/home/nkatz/dev/maritime/brest-data/recognition-my-split",
      "lowSpeed",2)

    geterateSplitData_Annotation(partitioned4,
      "/home/nkatz/dev/maritime/brest-data/recognition/1/lowSpeed.csv",
      "/home/nkatz/dev/maritime/brest-data/recognition-my-split",
      "lowSpeed",4)

    geterateSplitData_Annotation(partitioned8,
      "/home/nkatz/dev/maritime/brest-data/recognition/1/lowSpeed.csv",
      "/home/nkatz/dev/maritime/brest-data/recognition-my-split",
      "lowSpeed",8)





    geterateSplitData_Annotation(partitioned2,
      "/home/nkatz/dev/maritime/brest-data/recognition/1/sailing.csv",
      "/home/nkatz/dev/maritime/brest-data/recognition-my-split",
      "sailing",2)

    geterateSplitData_Annotation(partitioned4,
      "/home/nkatz/dev/maritime/brest-data/recognition/1/sailing.csv",
      "/home/nkatz/dev/maritime/brest-data/recognition-my-split",
      "sailing",4)

    geterateSplitData_Annotation(partitioned8,
      "/home/nkatz/dev/maritime/brest-data/recognition/1/sailing.csv",
      "/home/nkatz/dev/maritime/brest-data/recognition-my-split",
      "sailing",8)





    geterateSplitData_Annotation(partitioned2,
      "/home/nkatz/dev/maritime/brest-data/recognition/1/loitering.csv",
      "/home/nkatz/dev/maritime/brest-data/recognition-my-split",
      "loitering",2)

    geterateSplitData_Annotation(partitioned4,
      "/home/nkatz/dev/maritime/brest-data/recognition/1/loitering.csv",
      "/home/nkatz/dev/maritime/brest-data/recognition-my-split",
      "loitering",4)

    geterateSplitData_Annotation(partitioned8,
      "/home/nkatz/dev/maritime/brest-data/recognition/1/loitering.csv",
      "/home/nkatz/dev/maritime/brest-data/recognition-my-split",
      "loitering",8)
  }


  def geterateSplitData_Annotation(iter: List[Set[String]], originalAnnotationPath: String, pathForNewAnnotation: String, hle: String, coresNum: Int) = {
    var count = 1
    iter foreach { currentVessels =>
      val file = s"$pathForNewAnnotation/$coresNum$count/$hle.csv"
      println(file)
      val pw = new PrintWriter(new File(file))
      val singleAnnotationCoreData = Source.fromFile(originalAnnotationPath).getLines()
      singleAnnotationCoreData foreach { line =>
        val vessel = line.split("\\|")(1)
        if (currentVessels.contains(vessel)) {
          pw.write(line+"\n")
        }
      }
      count += 1
      pw.close()
    }
  }

  def generateSplitData(iter: Iterator[Set[String]], path: String, coresNum: Int) = {
    var count = 1
    iter foreach { currentVessels =>
      val file = s"$path/dataset$coresNum$count.txt"
      println(file)
      val pw = new PrintWriter(new File(file))
      val singleCoreData = Source.fromFile("/home/nkatz/dev/maritime/brest-data/datasets/dataset1.txt").getLines.filter(p => !p.contains("HoldsFor") && !p.contains("coord"))
      singleCoreData foreach { line =>
        val info = line.split("HappensAt")(1)
        val _info = info.split("\\]")
        val rest = _info(0).split("\\[")(1)
        //val lle = rest.split(" ")(0)
        val vessel = rest.split(" ")(1)
        if (currentVessels.contains(vessel)) {
          pw.write(line+"\n")
        }
      }
      count += 1
      pw.close()
    }
  }

  def getVesselSplit(vessels: Set[String], coresNum: Int) = {
    val partitionSize = (vessels.size.toDouble/coresNum).toInt
    vessels.grouped(partitionSize).take(coresNum)
  }

  def getAllVessels(dataPath: String) = {
    val data = Source.fromFile(dataPath).getLines
    data.foldLeft(Set[String]()) { (accum, x) =>
      if (!x.contains("HoldsFor") && !x.contains("coord")) {
        val info = x.split("HappensAt")(1)
        val _info = info.split("\\]")
        //val time = _info(1).trim.toInt
        val rest = _info(0).split("\\[")(1)
        //val lle = rest.split(" ")(0)
        val vessel = rest.split(" ")(1)
        accum + vessel
      } else {
        accum
      }
    }
  }


}

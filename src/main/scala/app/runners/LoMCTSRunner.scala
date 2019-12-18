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

import akka.actor.{ActorSystem, Props}
import app.runners.MLNDataHandler.MLNDataOptions
import app.runutils.{CMDArgs, Globals}
import logic.Examples.Example
import lomcts.LoMCTS

object LoMCTSRunner {

  // --inpath=/home/nkatz/dev/iled/datasets/CaviarMLN/move
  // --foldpath=/home/nkatz/dev/CAVIAR_MLN/CAVIAR_MLN/move/fold_2

  def main(args: Array[String]) = {

    val argsok = CMDArgs.argsOk(args)

    if (!argsok._1) {
      println(argsok._2)
      System.exit(-1)
    } else {
      val foldPath = args.map(x => x.split("=")).find(x => x(0) == "--foldpath").getOrElse(throw new RuntimeException("--foldpath missing."))(1)
      val inps = CMDArgs.getOLEDInputArgs(args)
      //val trainingDataOptions = new MLNDataOptions(foldPath, inps.chunkSize, take = 10000)
      val trainingDataOptions = new MLNDataOptions(foldPath, inps.chunkSize)
      val testingDataOptions = new MLNDataOptions(foldPath, inps.chunkSize)
      val trainingDataFunction: MLNDataOptions => Iterator[Example] = MLNDataHandler.getTrainingData
      val testingDataFunction: MLNDataOptions => Iterator[Example] = MLNDataHandler.getTestingData

      /*-----------------------------------------------*/
      Globals.glvalues("perfect-fit") = "false"
      Globals.glvalues("smallest-nonempty") = "true"
      /*-----------------------------------------------*/

      //val msg = "eval"
      val msg = "start"

      if (msg == "eval" && inps.evalth == "None") {
        throw new RuntimeException("No theory file provided (start msg = eval)")
      }

      val system = ActorSystem("LoMCTSLearningSystem")
      system.actorOf(Props(new LoMCTS(inps, trainingDataOptions, testingDataOptions,
                                      trainingDataFunction, testingDataFunction)), name = "lomcts") ! msg

    }

  }

}

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

package oled.mwua

import akka.actor.Actor
import app.runutils.RunningOptions
import logic.Clause
import oled.functions.SingleCoreOLEDFunctions._
import oled.mwua.MessageTypes.{FinishedBatchMsg, ProcessBatchMsg}
import org.slf4j.LoggerFactory

/**
  * Created by nkatz at 1/11/2018
  */

class Worker(val inps: RunningOptions) extends Actor {

  private val logger = LoggerFactory.getLogger(self.path.name)

  def receive = {

    case msg: ProcessBatchMsg =>
      val p = utils.Utils.time { processExample(msg.theory, msg.batch, msg.targetClass, inps, logger, learningWeights = false) }
      val (r, batchTime) = (p._1, p._2)
      val fmsg = new FinishedBatchMsg(r._1, r._2, r._3, r._4, r._5, r._6, batchTime, msg.targetClass)
      sender ! fmsg

  }
}

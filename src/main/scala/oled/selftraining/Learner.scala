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

package oled.selftraining

import akka.actor.Actor
import app.runutils.IOHandling.InputSource
import app.runutils.RunningOptions
import logic.Examples.Example

/**
  * Created by nkatz at 24/11/2018
  */

class Learner[T <: InputSource](val inps: RunningOptions,
                                val trainingDataOptions: T,
                                val testingDataOptions: T,
                                val trainingDataFunction: T => Iterator[Example],
                                val testingDataFunction: T => Iterator[Example]) extends Actor {

  def receive= {

    case exmpl: DataBatch => ???

  }

}

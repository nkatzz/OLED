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

package oled.single_core

import akka.actor.{Actor, PoisonPill, Props}
import app.runutils.IOHandling.{MongoSource, InputSource}
import app.runutils.RunningOptions
import com.typesafe.scalalogging.LazyLogging
import logic.Examples.Example

/**
  * Created by nkatz on 9/14/16.
  */

class Master[T <: InputSource](
    inps: RunningOptions,
    trainingDataOptions: T,
    testingDataOptions: T,
    trainingDataFunction: T => Iterator[Example],
    testingDataFunction: T => Iterator[Example]) extends Actor with LazyLogging {

  def receive = {

    case "eval" =>
      context.actorOf(Props(
        new Dispatcher(inps, trainingDataOptions, testingDataOptions, trainingDataFunction, testingDataFunction)),
                      name = s"Dispatcher-Actor-eval-mode") ! "eval"

    case "start" =>
      context.actorOf(Props(
        new Dispatcher(inps, trainingDataOptions, testingDataOptions, trainingDataFunction, testingDataFunction)),
                      name = s"Dispatcher-Actor-learning-mode") ! "start"

  }

}


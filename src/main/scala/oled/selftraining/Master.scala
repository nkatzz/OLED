package oled.selftraining

import akka.actor.Actor
import app.runutils.IOHandling.Source
import app.runutils.RunningOptions
import logic.Examples.Example

class Master[T <: Source](val inps: RunningOptions,
                          val trainingDataOptions: T,
                          val testingDataOptions: T,
                          val trainingDataFunction: T => Iterator[Example],
                          val testingDataFunction: T => Iterator[Example]) extends Actor {

  def receive = {

    case "go" => ???

  }

}

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

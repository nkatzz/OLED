package oled.single_core

import akka.actor.{Actor, PoisonPill, Props}
import app.runutils.IOHandling.{MongoSource, Source}
import app.runutils.RunningOptions
import com.typesafe.scalalogging.LazyLogging
import logic.Examples.Example



/**
  * Created by nkatz on 9/14/16.
  */



class Master[T <: Source](inps: RunningOptions,
                          trainingDataOptions: T,
                          testingDataOptions: T,
                          trainingDataFunction: T => Iterator[Example],
                          testingDataFunction: T => Iterator[Example]) extends Actor with LazyLogging{

  def receive = {

    case "EvaluateHandCrafted" =>
      context.actorOf(Props(new Dispatcher(inps, trainingDataOptions, testingDataOptions, trainingDataFunction, testingDataFunction)), name = s"Dispatcher-Actor-eval-mode") ! "EvaluateHandCrafted"


    case "start" =>
      context.actorOf(Props(new Dispatcher(inps, trainingDataOptions, testingDataOptions, trainingDataFunction, testingDataFunction)), name = s"Dispatcher-Actor-learning-mode") ! "start"

  }

}


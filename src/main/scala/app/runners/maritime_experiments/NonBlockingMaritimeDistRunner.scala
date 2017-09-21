package app.runners.maritime_experiments

import akka.actor.{ActorSystem, Props}
import app.runners.maritime_experiments.MultiCoreMaritimeRunner.getOptions
import app.runutils.{CMDArgs, RunningOptions}
import logic.Examples.Example
import oled.non_blocking.Dispatcher

/**
  * Created by nkatz on 7/10/17.
  */


object NonBlockingMaritimeDistRunner {

  def main(args: Array[String]) = {

    val argsok = CMDArgs.argsOk(args)
    if (!argsok._1) {
      println(argsok._2)
      System.exit(-1)
    } else {

      val runOpts = CMDArgs.getOLEDInputArgs(args)

      val opts = getOptions("highSpeedIn", 10, 5000, 2)

      val speedLimitsMap = app.runners.maritime_experiments.SingleCoreMaritimeRunner.populateSpeedLimitsMap(opts.head.speedLimitsPath, scala.collection.mutable.Map[String, scala.collection.mutable.Set[String]]())

      val p = prepare(runOpts, opts, speedLimitsMap)

      val testingFunction: MaritimeDataOptions => Iterator[Example] = p.head._1.getTestingData

      val testingOptions = opts.head

      val message = "go"

      // Start the actor system
      val system = ActorSystem("distributed-oled")

      system.actorOf(Props( new Dispatcher( opts zip p.map(x => x._2) , runOpts, 2, testingOptions, testingFunction) ), name = "TopLevelDispatcher") ! message


    }

  }


  def prepare(runOpts: RunningOptions, opts: List[MaritimeDataOptions],
              speedLimitsMap: scala.collection.mutable.Map[String, scala.collection.mutable.Set[String]]) = {

    opts.map { opt =>
      val nodeData = new NodeData(opt.hlePath, opt.llePath, opt.closeToPortsPath, opt.targetConcept, speedLimitsMap)
      val trainingFunction: MaritimeDataOptions => Iterator[Example] = nodeData.getTrainingData
      (nodeData, trainingFunction)
    }

    //val testingFunction: MaritimeDataOptions => Iterator[Example] = nodeData.getTestingData


  }


}

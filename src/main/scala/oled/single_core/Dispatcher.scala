package oled.single_core

import akka.actor.{Actor, PoisonPill, Props}
import app.runutils.IOHandling.Source
import app.runutils.RunningOptions
import com.typesafe.scalalogging.LazyLogging
import jep.Jep
import logic.Examples.Example
import logic.{LogicUtils, Theory}
import utils.Implicits._
import oled.functions.SingleCoreOLEDFunctions.crossVal

/**
  * Created by nkatz on 28/2/2016.
  */

class Dispatcher[T <: Source](inps: RunningOptions,
                              trainingDataOptions: T,
                              testingDataOptions: T,
                              trainingDataFunction: T => Iterator[Example],
                              testingDataFunction: T => Iterator[Example]) extends Actor with LazyLogging {

  var size = 2 // two processes are started, one for learning the initiatedAt part and one for the terminatedAt
  var theories = List[(Theory,Double)]()
  var merged = Theory()
  var time = 0.0
  //var done = false

  def receive = {

    case "EvaluateHandCrafted" =>
      if (!inps.evalth.isFile) {
        logger.error(s"${inps.evalth} is not a file.")
      } else {
        println(s"Evaluating hand-crafted theory from ${inps.evalth}")
        val crossValJep = new Jep()
        //crossValJep.runScript(GlobalValues.ASPHandler)

        val data = testingDataFunction(testingDataOptions)

        val (tps,fps,fns,precision,recall,fscore) =
          crossVal(merged, crossValJep, data = data, handCraftedTheoryFile = inps.evalth, globals = inps.globals, inps = inps)
        logger.info(s"\ntps: $tps\nfps: $fps\nfns: $fns\nprecision: $precision\nrecall: $recall\nf-score: $fscore\ntraining time:" +
          s"$time\ntheory size: 0.0")
        crossValJep.close()
      }
      context.system.terminate()


    case "start" =>
      context.actorOf(Props(new TheoryLearner(inps, trainingDataOptions, testingDataOptions, trainingDataFunction, testingDataFunction, "initiated")), name = s"initiated-learner-${this.##}") ! "go"
      context.actorOf(Props(new TheoryLearner(inps, trainingDataOptions, testingDataOptions, trainingDataFunction, testingDataFunction, "terminated")), name = s"terminated-learner-${this.##}") ! "go"

    case x: (Theory,Double) =>
      theories = theories :+ x
      size -= 1
      sender ! PoisonPill // kill the child actor
      if(size == 0) {
        // merge the theories and do cross-validation
        val first = theories.head
        val second = if (theories.tail.nonEmpty) theories.tail.head else (Theory(), 0.0)
        merged = first._1.clauses ++ second._1.clauses
        val theorySize = merged.clauses.foldLeft(0)((x,y) => x + y.body.length + 1)
        time = Math.max(first._2,second._2)

        logger.info("Performing cross-validation")

        //=============================================
        val crossValJep = new Jep()
        //crossValJep.runScript(GlobalValues.ASPHandler)
        //=============================================

        val data = testingDataFunction(testingDataOptions)
        val (tps,fps,fns,precision,recall,fscore) = crossVal(merged, crossValJep, data=data, globals = inps.globals, inps = inps)

        logger.info(s"\ntps: $tps\nfps: $fps\nfns: $fns\nprecision: $precision\nrecall: $recall\nf-score: $fscore\ntraining time: " +
          s"$time\ntheory size: $theorySize")
        val merged_ = Theory(LogicUtils.compressTheory(merged.clauses))
        logger.info(s"\nDone. Theory found:\n ${merged_.showWithStats}")
        crossValJep.close()
        //context.parent ! new ResultsContainer(tps.toFloat,fps.toFloat,fns.toFloat,precision,recall,fscore,theorySize.toFloat,time,merged)
        //println("sent results, shutting down")
        context.system.terminate()
      }
  }



}

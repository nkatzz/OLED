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

import java.io.File

import akka.actor.{Actor, PoisonPill, Props}
import app.runutils.IOHandling.Source
import app.runutils.{Debug, Globals, RunningOptions}
import com.typesafe.scalalogging.LazyLogging
import logic.Examples.Example
import logic.{LogicUtils, Theory}
import utils.Implicits._
import oled.functions.SingleCoreOLEDFunctions.crossVal
import oled.weightlearn.EvalAfterWeightLearning
import utils.Utils

/**
  * Created by nkatz on 28/2/2016.
  */

class Dispatcher[T <: Source](inps: RunningOptions,
                              trainingDataOptions: T,
                              testingDataOptions: T,
                              trainingDataFunction: T => Iterator[Example],
                              testingDataFunction: T => Iterator[Example]) extends Actor with LazyLogging {

  private var size = inps.globals.MODEHS.size // One process for each target concept.
  private var theories = List[(Theory,Double)]()
  private var merged = Theory()
  private var time = 0.0

  private val weightLearning = Globals.glvalues("weight-learning").toBoolean

  def receive = {

    case "eval" =>
      if (!inps.evalth.isFile) {
        logger.error(s"${inps.evalth} is not a file.") ; System.exit(-1)
      } else {

        println(s"Evaluating theory from ${inps.evalth}")
        val data = testingDataFunction(testingDataOptions)

        if (!weightLearning) {

          val (tps, fps, fns, precision, recall, fscore) = crossVal(merged,
            data = data, handCraftedTheoryFile = inps.evalth, globals = inps.globals, inps = inps)

          logger.info(s"\ntps: $tps\nfps: $fps\nfns: $fns\nprecision: " +
            s"$precision\nrecall: $recall\nf-score: $fscore\ntraining time:" +
            s"$time\ntheory size: 0.0")


        } else {
          //private val mlnClauses = theory.clauses.map(x => x.to_MLNClause())
          val mlnClauses = scala.io.Source.fromFile(inps.evalth).getLines.filter(p => !p.startsWith("//") && p.trim != "").toList
          val evaluator = new EvalAfterWeightLearning(mlnClauses, data)
          val (tps, fps, fns) =  evaluator.getCounts()
          logger.info(s"\nEvaluation (weight learning):\ntps: $tps\nfps: $fps\nfns: $fns")
        }

      }
      context.system.terminate()

    case "start" =>

      if (!weightLearning) { // !weightLearning  // quick & dirty stuff to run this
        if (inps.withEventCalculs) {
          context.actorOf(Props(
            new TheoryLearner(inps, trainingDataOptions, testingDataOptions, trainingDataFunction, testingDataFunction, "initiated")),
            name = s"initiated-learner-${this.##}") ! "go"

          context.actorOf(Props(
            new TheoryLearner(inps, trainingDataOptions, testingDataOptions, trainingDataFunction, testingDataFunction, "terminated")),
            name = s"terminated-learner-${this.##}") ! "go"
        } else {
          context.actorOf(Props(
            new TheoryLearner(inps, trainingDataOptions, testingDataOptions, trainingDataFunction, testingDataFunction, "None")),
            name = s"learner-${this.##}") ! "go"
        }

      } else {

        context.actorOf(Props(new woled.Learner(inps, trainingDataOptions, testingDataOptions, trainingDataFunction, testingDataFunction, "initiated")), name = s"learner-${this.##}") ! "go"

        // This is for running the old version without actual MPE inference
        /*
        if (! inps.parallelClauseEval) {
          context.actorOf(Props(new oled.weightlearn.WeightedTheoryLearner(inps, trainingDataOptions, testingDataOptions, trainingDataFunction, testingDataFunction, "initiated")), name = s"initiated-learner-${this.##}") ! "go"
          context.actorOf(Props(new oled.weightlearn.WeightedTheoryLearner(inps, trainingDataOptions, testingDataOptions, trainingDataFunction, testingDataFunction, "terminated")), name = s"terminated-learner-${this.##}") ! "go"
        } else {
          context.actorOf(Props(new oled.weightlearn.parallel.WeightedTheoryLearner(inps, trainingDataOptions, testingDataOptions, trainingDataFunction, testingDataFunction, "initiated")), name = s"initiated-learner-${this.##}") ! "go"
          context.actorOf(Props(new oled.weightlearn.parallel.WeightedTheoryLearner(inps, trainingDataOptions, testingDataOptions, trainingDataFunction, testingDataFunction, "terminated")), name = s"terminated-learner-${this.##}") ! "go"
        }
        */

      }

    case x: (Theory, Double) =>
      theories = theories :+ x
      size -= 1
      sender ! PoisonPill // kill the child actor

      //logger.info(s"Error:\n${Globals.errorProb}")

      if(size == 0) {

        // merge the theories and do cross-validation
        val first = theories.head
        val second = if (theories.tail.nonEmpty) theories.tail.head else (Theory(), 0.0)
        merged = first._1.clauses ++ second._1.clauses

        val theorySize = merged.clauses.foldLeft(0)((x,y) => x + y.body.length + 1)
        time = Math.max(first._2,second._2)

        val data = testingDataFunction(testingDataOptions)

        logger.info("Evaluating on the test set")

        if (!weightLearning) {

          /* THIS MAY TAKE TOO LONG FOR LARGE AND COMPLEX THEORIES!! */
          logger.info("Compressing theory...")
          val merged_ = Theory(LogicUtils.compressTheory(merged.clauses))
          logger.info(s"\nDone. Theory found:\n ${merged_.showWithStats}")

          if (inps.saveTheoryTo != "") {
            Utils.writeToFile(new File(inps.saveTheoryTo), "overwrite") {p => Vector(merged_.tostring).foreach(p.println)}
          }

          val (tps,fps,fns,precision,recall,fscore) = crossVal(merged_, data=data, globals = inps.globals, inps = inps)

          logger.info(s"\ntps: $tps\nfps: $fps\nfns: " +
            s"$fns\nprecision: $precision\nrecall: $recall\nf-score: $fscore\ntraining time: " +
            s"$time\ntheory size: $theorySize")

          /*
          println(s"\ntps: $tps\nfps: $fps\nfns: " +
            s"$fns\nprecision: $precision\nrecall: $recall\nf-score: $fscore\ntraining time: " +
            s"$time\ntheory size: $theorySize")
          */


          //val test = merged_.withTypePreds(inps.globals)

          context.system.terminate()

        } else { // cross-validation after weight learning with AdaGrad

          logger.info(s"\nAll clauses:\n${merged.clauses.map(x => x.to_MLNClause()).mkString("\n")}")

          val merged_ = Theory(LogicUtils.compressTheory(merged.clauses))
          //val merged_ = Theory(LogicUtils.compressTheory_RemoveSubsumers(merged.clauses))

          val _mlnClauses = merged_.clauses.filter(x => x.mlnWeight > 0.0 && x.seenExmplsNum >= inps.minEvalOn && x.score >= inps.pruneThreshold)

          val theorySize = _mlnClauses.clauses.foldLeft(0)((x,y) => x + y.body.length + 1)

          //val _mlnClauses = merged_.clauses.filter(x => x.score >= inps.pruneThreshold && x.seenExmplsNum > 2000)

          // Keep negative weights
          //val _mlnClauses = merged_.clauses.filter(x => x.seenExmplsNum > 2000)

          val mlnClausesString = _mlnClauses.map(x => x.to_MLNClause())

          val evaluator = new EvalAfterWeightLearning(mlnClausesString, data)
          val (tps, fps, fns) =  evaluator.getCounts()
          val precision = tps.toDouble/(tps.toDouble + fps.toDouble)
          val recall = tps.toDouble/(tps.toDouble + fns.toDouble)
          val fscore = 2*precision*recall/(precision+recall)

          logger.info(s"\n\nMLN theory (kept clauses):\n${mlnClausesString.mkString("\n")}")

          logger.info(s"\n\nKept clauses in ASP format:\n${_mlnClauses.showWithStats}")

          logger.info(s"\nWeight learning evaluation:\ntps: $tps\nfps: $fps\nfns: " +
            s"$fns\nprecision: $precision\nrecall: $recall\ntheory size: ${}\nf-score: $fscore\ntraining time: $time")

          val msg = s"\nMLN theory (kept clauses):\n${mlnClausesString.mkString("\n")}\ntps: $tps\nfps: $fps\nfns: " +
            s"$fns\nprecision: $precision\nrecall: $recall\nf-score: $fscore\ntraining time: $time\ntheory size: $theorySize\n"

          Utils.writeLine(msg, s"/home/nkatz/Desktop/weight-learning-experiments/weight-learn-results-δ=" +
              s"${inps.delta}-ties=${inps.breakTiesThreshold}-adaδ=${inps.adaGradDelta}-adaλ=${inps.adaRegularization}-adaη=" +
              s"${inps.adaLearnRate}-winsize=${inps.chunkSize}", "append")

          context.system.terminate()
        }
      }
  }





}

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

package oled.non_blocking

import akka.actor.{Actor, Props}
import app.runutils.IOHandling.InputSource
import app.runutils.{Globals, RunningOptions}
import com.typesafe.scalalogging.LazyLogging
import logic.Examples.Example
import logic.{Clause, LogicUtils, Theory}
import oled.distributed.Structures.{FinalTheoryMessage, Initiated, Terminated}
import oled.functions.DistributedOLEDFunctions.crossVal



/**
  * Created by nkatz on 3/14/17.
  *
  *
  * This actor starts two top-level actors to coordinate learning
  * the initiated and the terminated part of the theory respectively.
  *
  */

class Dispatcher[T <: InputSource](val dataOptions: List[(T, T => Iterator[Example])],
                                   val inputParams: RunningOptions,
                                   val tasksNumber: Int,
                                   testingDataOptions: T,
                                   testingDataFunction: T => Iterator[Example]) extends Actor with LazyLogging {

  private var counter = tasksNumber
  private var initTheory = List[Clause]()
  private var termTheory = List[Clause]()
  private var initTrainingTime = ""
  private var termTrainingTime = ""
  private var theory = List[Clause]() // this is for future use with single-predicate learning
  private var theoryTrainingTime = ""
  private var initTotalMsgNum = 0
  private var initTotalMsgSize = 0L
  private var termTotalMsgNum = 0
  private var termTotalMsgSize = 0L

  def updateMessages(m: FinalTheoryMessage, what: String) = {
    what match {
      case "init" =>
        initTotalMsgNum = m.totalMsgNum
        initTotalMsgSize = m.totalMsgSize
      case "term" =>
        termTotalMsgNum = m.totalMsgNum
        termTotalMsgSize = m.totalMsgSize
      case _ => logger.info("UNKNOWN MESSAGE!")
    }
  }

  def receive = {
    case "go" =>
      context.actorOf(Props(new TopLevelActor(dataOptions, inputParams, new Initiated) ), name = "InitTopLevelActor") ! "go"
      context.actorOf(Props(new TopLevelActor(dataOptions, inputParams, new Terminated) ), name = "TermTopLevelActor") ! "go"

    /*---------------------------------------------------------------------------*/
    // For debugging (trying to see if the sub-linear speed-up is due to blocking)
    /*---------------------------------------------------------------------------*/
    case "go-no-communication" =>
      context.actorOf(Props(new TopLevelActor(dataOptions, inputParams, new Initiated) ), name = "InitTopLevelActor") ! "go-no-communication"
      context.actorOf(Props(new TopLevelActor(dataOptions, inputParams, new Terminated) ), name = "TermTopLevelActor") ! "go-no-communication"

    case msg: FinalTheoryMessage =>
      msg.targetPredicate match {
        case x: Initiated =>
          this.initTheory = msg.theory
          this.initTrainingTime = msg.trainingTime
          updateMessages(msg, "init")

        case x: Terminated =>
          this.termTheory = msg.theory
          this.termTrainingTime = msg.trainingTime
          updateMessages(msg, "term")

        case _ =>
          this.theory =  msg.theory
          this.theoryTrainingTime = msg.trainingTime
          //updateMessages(msg)
      }
      counter -= 1
      if (counter == 0) {
        logger.info(s"\n\nThe initiated part of the theory is\n${Theory(this.initTheory).showWithStats}\nTraining" +
          s" time: $initTrainingTime\nTotal messages: $initTotalMsgNum\nTotal message size: $initTotalMsgSize")
        logger.info(s"\n\nThe terminated part of the theory is\n${Theory(this.termTheory).showWithStats}\nTraining" +
          s" time: $termTrainingTime\nTotal messages: $termTotalMsgNum\nTotal message size: $termTotalMsgSize")

        /*
         * Cross-validation...
         * */

        val merged_ = Theory(this.initTheory ++ this.termTheory)
        val compressed = Theory(LogicUtils.compressTheory(merged_.clauses))

        /*------------------*/
        // DEBUGGING-TESTING
        /*------------------*/
        //val filtered = Theory(compressed.clauses.filter(x => x.tps > 50))
        val filtered = compressed

        val data = testingDataFunction(testingDataOptions)

        val (tps,fps,fns,precision,recall,fscore) =
          crossVal(filtered, data = data, handCraftedTheoryFile = inputParams.evalth, globals = inputParams.globals, inps = inputParams)

        val time = Math.max(this.initTrainingTime.toDouble, this.termTrainingTime.toDouble)

        val theorySize = filtered.clauses.foldLeft(0)((x,y) => x + y.body.length + 1)

        logger.info(s"\ntps: $tps\nfps: $fps\nfns: $fns\nprecision:" +
          s" $precision\nrecall: $recall\nf-score: $fscore\ntraining time: " +
          s"$time\ntheory size: $theorySize\n" +
          s"Total number of messages: ${initTotalMsgNum+termTotalMsgNum}\n" +
          s"Total message size: ${initTotalMsgSize+termTotalMsgSize}")

        logger.info(s"\nDone. Theory found:\n ${filtered.showWithStats}")
        logger.info(s"Mean time per batch: ${Globals.timeDebug.sum/Globals.timeDebug.length}")
        logger.info(s"Total batch time: ${Globals.timeDebug.sum}")

        context.system.terminate()
      }
  }


  def showTheory(t: Theory) = {
    val showClause  = (c: Clause) => {
      s"score (${if (c.head.functor=="initiatedAt") "precision" else "recall"}):" +
        s"${c.score}, tps: ${c.tps}, fps: ${c.fps}, fns: ${c.fns} Evaluated on: ${c.seenExmplsNum} examples\n$$c.tostring}"
    }
    t.clauses.map(x => showClause(x)).mkString("\n")
  }

  /*
  def crossVal() = {
    val merged_ = Theory(this.initTheory ++ this.termTheory)
    val compressed = Theory(LogicUtils.compressTheory(merged_.clauses))

    /*------------------*/
    // DEBUGGING-TESTING
    /*------------------*/
    //val filtered = Theory(compressed.clauses.filter(x => x.tps > 50))
    val filtered = compressed
    val crossValJep = new Jep()

    val data = testingDataFunction(testingDataOptions)

    val (tps,fps,fns,precision,recall,fscore) = crossVal(filtered, crossValJep, data = data, handCraftedTheoryFile = inps.evalth, globals = inps.globals, inps = inps)

    val time = Math.max(this.initTrainingTime.toDouble, this.termTrainingTime.toDouble)

    val theorySize = filtered.clauses.foldLeft(0)((x,y) => x + y.body.length + 1)

    logger.info(s"\ntps: $tps\nfps: $fps\nfns: $fns\nprecision:" +
      s" $precision\nrecall: $recall\nf-score: $fscore\ntraining time: " +
      s"$time\ntheory size: $theorySize\n" +
      s"Total number of messages: ${initTotalMsgNum+termTotalMsgNum}\n" +
      s"Total message size: ${initTotalMsgSize+termTotalMsgSize}")

    logger.info(s"\nDone. Theory found:\n ${filtered.showWithStats}")
    logger.info(s"Mean time per batch: ${Globals.timeDebug.sum/Globals.timeDebug.length}")
    logger.info(s"Total batch time: ${Globals.timeDebug.sum}")
    crossValJep.close()
  }
  */
}

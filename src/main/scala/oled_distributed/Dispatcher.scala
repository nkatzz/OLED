package oled_distributed

import akka.actor.{Props, Actor}
import app.{Globals, InputParameters}
import com.typesafe.scalalogging.LazyLogging
import logic.{Theory, Clause}
import oled_distributed.Structures.{Terminated, Initiated, FinalTheoryMessage}
import utils.Exmpl

/**
  * Created by nkatz on 3/14/17.
  *
  *
  * This actor starts two top-level actors to coordinate learning
  * the initiated and the terminated part of the theory respectively.
  *
  */

class Dispatcher(val databases: List[String],
                 val getDataFunction: (String, String, Int) => Iterator[Exmpl],
                 val inputParams: InputParameters) extends Actor with LazyLogging {

  var counter = 2
  var initTheory = List[Clause]()
  var termTheory = List[Clause]()
  var initTrainingTime = ""
  var termTrainingTime = ""
  var theory = List[Clause]() // this is for future use with single-predicate learning
  var theoryTrainingTime = ""

  def receive = {
    case "go" =>
      context.actorOf(Props( new TopLevelActor(databases, Utils.getDataFromDB, inputParams, targetConcept = new Initiated) ), name = "InitTopLevelActor") ! "go"
      context.actorOf(Props( new TopLevelActor(databases, Utils.getDataFromDB, inputParams, targetConcept = new Terminated) ), name = "TermTopLevelActor") ! "go"

    case msg: FinalTheoryMessage =>
      msg.targetPredicate match {
        case x: Initiated =>
          this.initTheory = msg.theory
          this.initTrainingTime = msg.trainingTime

        case x: Terminated =>
          this.termTheory = msg.theory
          this.termTrainingTime = msg.trainingTime

        case _ =>
          this.theory =  msg.theory
          this.theoryTrainingTime = msg.trainingTime
      }
      counter -= 1
      if (counter == 0) {
        logger.info(s"\n\nThe initiated part of the theory is\n${Theory(this.initTheory).showWithStats}\nTraining" +
          s" time: $initTrainingTime")
        logger.info(s"\n\nThe terminated part of the theory is\n${Theory(this.termTheory).showWithStats}\nTraining time: $termTrainingTime")
        context.system.shutdown()
      }
  }


  def showTheory(t: Theory) = {
    val showClause  = (c: Clause) => {
      s"score (${if (c.head.functor=="initiatedAt") "precision" else "recall"}):" +
        s"${c.score}, tps: ${c.tps}, fps: ${c.fps}, fns: ${c.fns} Evaluated on: ${c.seenExmplsNum} examples\n$$c.tostring}"
    }
    t.clauses.map(x => showClause(x)).mkString("\n")
  }


}

package mcts.parallel

import akka.actor.{Actor, PoisonPill}
import app.runutils.Globals
import app.runutils.IOHandling.Source
import com.typesafe.scalalogging.LazyLogging
import jep.Jep
import logic.Examples.Example
import logic.Theory


/**
  * Created by nkatz on 9/22/17.
  */

class ScorerSlave[T <: Source](globals: Globals, jep: Jep, options: T,
                               dataFunction: T => Iterator[Example]) extends Actor with LazyLogging {

  def receive = {
    case theory: Theory =>
      logger.info(s"Scoring\n${theory.tostring}\n")
      Eval.crossVal(theory, jep, dataFunction(options), globals)
      sender ! theory
      //self ! PoisonPill
  }

}

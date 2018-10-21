package mcts.parallel

import akka.actor.{Actor, Props}
import app.runutils.Globals
import app.runutils.IOHandling.Source
import logic.Examples.Example
import logic.Theory

/**
  * Created by nkatz on 9/22/17.
  */

class ScorerMaster[T <: Source](globals: Globals,
                                options: T,
                                dataFunction: T => Iterator[Example]) extends Actor {

  private var theoriesCount = 0

  private var scoredTheories = List[Theory]()

  private def stringTheory(t: Theory) = t.clauses.map(x => x.withTypePreds(globals).tostring).mkString("\n")

  private val cores = Runtime.getRuntime.availableProcessors

  private val f1 = (t: Theory) => t.stats._6

  def receive = {
    case theories: Vector[Theory] =>
      theoriesCount = theories.length
      scoredTheories = List[Theory]()
      val jobsPerCore = math.ceil(cores.toDouble/theories.length).toInt
      var i = 0
      //theories.map(x => stringTheory(x)).grouped(jobsPerCore) foreach { jobs =>
      theories.grouped(jobsPerCore) foreach { jobs =>
        jobs foreach { theory =>
          context.actorOf(Props(new ScorerSlave(globals, options, dataFunction)), name = s"scorer-slave-$i") ! theory
          i += 1
        }
      }

    case theory: Theory =>
      scoredTheories = scoredTheories :+ theory
      theoriesCount -= 1
      if (theoriesCount == 0) {
        //val bestTheory = scoredTheories.maxBy( x => f1(x) )

        val bestTheory = scoredTheories.sortBy(x => -f1(x)).head

        println(s"${bestTheory.tostring} ${f1(bestTheory)}")

      }
  }





}

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

import akka.actor._
import app.runutils.IOHandling.InputSource
import app.runutils.RunningOptions
import com.madhukaraphatak.sizeof.SizeEstimator
import logic.Examples.Example
import logic.{Clause, Theory}
import oled.distributed.Structures._
import org.slf4j.LoggerFactory

/**
  * Created by nkatz on 2/15/17.
  */

class TopLevelActor[T <: InputSource](
    val dataOptions: List[(T, T => Iterator[Example])],
    val inputParams: RunningOptions,
    val targetConcept: TargetConcept) extends Actor {

  import context._

  var actorsPoolSize = 0
  var nodesCounter = 0

  var startTime = 0L
  var endTime = 0L

  /* This function starts the learning Nodes. */
  def getActorsPool() = {
    val NodeActorNames = (1 to dataOptions.length).toList map (i => s"Node-$i-${targetConcept.toString}")
    val nodeActorsPool = (NodeActorNames zip this.dataOptions) map { node =>
      val (nodeName, nodeOptions, nodeDataDunction) = (node._1, node._2._1, node._2._2)
      val otherActors = NodeActorNames.filter(_ != nodeName)
      context.actorOf(Props(new Node(otherActors, targetConcept, inputParams, nodeOptions, nodeDataDunction)), name = nodeName)
    }
    nodeActorsPool
  }

  val actorsPool: List[ActorRef] = getActorsPool()

  val actorNames: List[String] = actorsPool.map(x => x.path.name)

  var nodeHavingTheSlot = "" // that's only for logging

  def getOtherActorNames(actorName: String) = actorNames.filter(name => name != actorName)
  def getOtherActorRefs(a: String) = getOtherActorNames(a) map (actorName => context.actorSelection(s"${self.path}/$actorName"))

  private var finalTheories = List[Theory]() // these should all be the same

  private val logger = LoggerFactory.getLogger(self.path.name)

  private var messages = List[Long]()

  def updateMessages(m: AnyRef) = {
    val size = SizeEstimator.estimate(m)
    messages = messages :+ size
  }

  private var childrenMsgNums = List[Int]()
  private var childrenMsgSizes = List[Long]()

  def receive = {
    case "go" =>
      this.actorsPoolSize = actorsPool.length
      this.nodesCounter = actorsPool.length
      Thread.sleep(4000)
      this.startTime = System.nanoTime()
      actorsPool foreach (a => a ! "go")

    case "go-no-communication" =>
      this.actorsPoolSize = actorsPool.length
      this.nodesCounter = actorsPool.length
      Thread.sleep(4000)
      this.startTime = System.nanoTime()
      actorsPool foreach (a => a ! "go-no-communication")
    //become(replyHandler)

    case msg: NodeDoneMessage =>
      acceptNewDoneMsg(msg)

    case msg: NodeTheoryMessage =>
      acceptNewLearntTheory(msg)

  }

  def acceptNewDoneMsg(msg: NodeDoneMessage) = {
    this.actorsPoolSize -= 1
    logger.info(s"Node ${msg.sender} is done. ${this.actorsPoolSize} nodes remaining")
    if (this.actorsPoolSize == 0) {
      logger.info("All processing nodes are done")
      val theoryRequest = new TheoryRequestMessage(self.path.name)
      this.actorsPool foreach (a => a ! theoryRequest)
    }
  }

  def acceptNewLearntTheory(msg: NodeTheoryMessage) = {
    this.nodesCounter -= 1
    logger.info(s"Node ${msg.sender} sent:\n${msg.theory.clauses.map(x => x.showWithStats + s"evaluated on ${x.seenExmplsNum} exmpls | refs: ${x.refinements.length}").mkString("\n")}")
    this.finalTheories = this.finalTheories :+ msg.theory
    this.childrenMsgNums = this.childrenMsgNums :+ msg.msgNum
    this.childrenMsgSizes = this.childrenMsgSizes :+ msg.msgSize
    if (this.nodesCounter == 0) {
      this.endTime = System.nanoTime()
      this.actorsPool.foreach(_ ! PoisonPill)
      val totalTime = (this.endTime - this.startTime) / 1000000000.0
      logger.info(s"Total training time: $totalTime sec")
      val totalMsgNum = childrenMsgNums.sum + messages.length
      val totalMsgSize = childrenMsgSizes.sum + messages.sum
      context.parent ! new FinalTheoryMessage(getFinalTheory(), totalTime.toString, totalMsgNum, totalMsgSize, targetConcept)
    }
  }

  def getFinalTheory() = {
    this.finalTheories.head.clauses.foldLeft(List[Clause]()){ (accum, clause) =>
      val clauseCopies = this.finalTheories.tail.flatMap(theory => theory.clauses.filter(c => c.uuid == clause.uuid))
      if (clauseCopies.length + 1 != this.finalTheories.length) {
        logger.info(s"\nCLAUSE\n${clause.tostring} (uuid: ${clause.uuid}) \nIS NOT FOUND IS SOME FINAL THEORY")
      }
      val sumCounts = clauseCopies.foldLeft(clause.tps, clause.fps, clause.fns, clause.seenExmplsNum) { (x, y) =>
        (x._1 + y.tps, x._2 + y.fps, x._3 + y.fns, x._4 + y.seenExmplsNum)
      }
      clause.tps = sumCounts._1
      clause.fps = sumCounts._2
      clause.fns = sumCounts._3
      clause.seenExmplsNum = sumCounts._4
      if (clause.seenExmplsNum > inputParams.minEvalOn && clause.score >= inputParams.pruneThreshold) {
        accum :+ clause
      } else {
        accum
      }
    }
  }

}

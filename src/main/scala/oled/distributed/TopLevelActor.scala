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

package oled.distributed

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


class TopLevelActor[T <: InputSource](val dataOptions: List[(T, T => Iterator[Example])],
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
      context.actorOf(Props( new Node(otherActors, targetConcept, inputParams, nodeOptions, nodeDataDunction) ), name = nodeName)
    }
    nodeActorsPool
  }


  val actorsPool: List[ActorRef] = getActorsPool()

  val actorNames: List[String] = actorsPool.map(x => x.path.name)

  private var queuedExpandingNodes = scala.collection.mutable.Queue[QueuedExpandingNode]()
  var nodeHavingTheSlot = "" // that's only for logging

  def getOtherActorNames(actorName: String) = actorNames.filter(name => name != actorName)
  def getOtherActorRefs(a: String) = getOtherActorNames(a) map (actorName => context.actorSelection(s"${self.path}/$actorName"))

  private var finalTheories = List[Theory]() // these should all be the same

  /*
  * The logger for this class. Getting a logger this way instead of mixin-in the LazyLogging trait allows to
  * name the logger after a particular class instance, which is helpful for tracing messages
  * between different instances of the same class.
  * */
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
      become(replyHandler)

      /*--------------------------------------------------------------------------------------*/
      // For debugging
      //context.actorOf(Props( new PingActor(this.actorNames) ), name = "Pinging-Actor") ! "go"
    /*--------------------------------------------------------------------------------------*/

    case request: SpecializationTicketRequest =>
      become(requestHandler)
      // re-send the message to self to be processed
      self ! request
  }


  def replyHandler: Receive = {

    // Upon receiving a reply, the flow continues by either sending a specialization
    // ticket to the next enqueued node, or (if the queue is empty), by freeing the specialization slot
    // and becoming a requestHandler to serve further expansion requests (see comment at the handleReply() method).
    case reply: ExpansionFinished =>
      handleReply()

    // When an expansion request while this actor is in a replyHandler state (and therefore another node is
    // specializing), the request is enqueued to be processed when the expansion slot opens
    case request: SpecializationTicketRequest =>
      this.queuedExpandingNodes.enqueue(new QueuedExpandingNode(request.senderName, request.otherNodeNames))
      logger.info(s"Node ${request.senderName} is enqueued for expansion. The queue now is ${this.queuedExpandingNodes.map(x => x.senderName).mkString("  ")}")
      context.actorSelection(s"${self.path}/${request.senderName}") ! new AnotherNodeIsSpecializing(this.nodeHavingTheSlot)

    // This message is received by an enqueued expansion node that eventually received
    // its specialization ticket, but all of its candidates have already been specialized
    // in the meantime. The aborting node has already switched to normal state to continue processing
    // so no message is required to be sent to it. The flow continues by either sending a specialization
    // ticket to the next enqueued node, or (if the queue is empty), by freeing the specialization slot
    // and becoming a requestHandler to serve further expansion requests (see comment at the handleReply() method).
    case abort: ExpansionAbortMsg =>
      logger.info(s"Node ${abort.abortingNodeName} aborted expansion (all candidates already specialized).")
      val others = getOtherActorRefs(abort.abortingNodeName)
      others foreach (_ ! new PrioritizedNodeAbortedExpansion(abort.abortingNodeName))
      handleReply()

    case msg: NodeDoneMessage =>
      acceptNewDoneMsg(msg)

    case msg: NodeTheoryMessage =>
      acceptNewLearntTheory(msg)
  }






  def requestHandler: Receive = {

    // When in a requestHandler state, the expansion slot (one expanding node at a time) is free,
    // so simply sent a SpecializationTicket upon receiving a request when at this state.
    case request: SpecializationTicketRequest =>
      logger.info(s"Received a specialization ticket request from ${request.senderName}")
      processNewRequest(request.senderName)

    // When a node supervised by this top-level actor, wrap things up
    case msg: NodeDoneMessage =>
      acceptNewDoneMsg(msg)

    // When all nodes supervised by this top-level actor are done, wrap things up
    case msg: NodeTheoryMessage =>
      acceptNewLearntTheory(msg)
  }




  def processNewRequest(requestingNodeName: String) = {
    val others = getOtherActorRefs(requestingNodeName)
    this.nodeHavingTheSlot = requestingNodeName // that's only for logging
    others foreach (_ ! new AnotherNodeIsSpecializing(requestingNodeName))
    become(replyHandler)
    context.actorSelection(s"${self.path}/$requestingNodeName") ! new SpecializationTicket(self.path.name)
    logger.info(s"Sent the ticket to $requestingNodeName")
  }

  def handleReply() = {
    if(this.queuedExpandingNodes.nonEmpty) {
      val nextInQueue = this.queuedExpandingNodes.dequeue()
      logger.info(s"Sending specialization ticket to queued node ${nextInQueue.senderName}")
      processNewRequest(nextInQueue.senderName)
    } else {
      this.nodeHavingTheSlot = "none" // that's only for logging
      become(requestHandler)
    }
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
      val totalTime = (this.endTime - this.startTime)/1000000000.0
      logger.info(s"Total training time: $totalTime sec")
      val totalMsgNum = childrenMsgNums.sum + messages.length
      val totalMsgSize = childrenMsgSizes.sum + messages.sum
      context.parent ! new FinalTheoryMessage(getFinalTheory(), totalTime.toString, totalMsgNum, totalMsgSize, targetConcept)
    }
  }

  /*
  def getFinalTheory() = {
    val uuids = this.finalTheories.head.clauses.map(_.uuid)
    val withAccumScores = uuids.foldLeft(List[Clause]()) { (accumed, newUUID) =>
      val copies = this.finalTheories.flatMap(theory => theory.clauses.filter(p => p.uuid == newUUID))
      if (copies.length != this.finalTheories.length) throw new RuntimeException("Produced non-identical theories")

      val (tps, fps, fns, exmpls) = copies.foldLeft(0, 0, 0, 0) { (x, y) =>
        (x._1 + y.tps, x._2 + y.fps, x._3 + y.fns, x._4 + y.seenExmplsNum)
      }
      copies.head.tps = tps
      copies.head.fps = fps
      copies.head.fns = fns
      copies.head.seenExmplsNum = exmpls
      accumed :+ copies.head
    }
    // Poor-man's pruning. Not online, not time-consuming (and a bit cheating) offline,
    // just a quick filtering to see how we're doing
    val filteredTheory = withAccumScores.filter(p =>
      p.seenExmplsNum > inputParams.minSeenExmpls && p.score > inputParams.postPruningThreshold)

    filteredTheory
  }
  */


  def getFinalTheory() = {
    this.finalTheories.head.clauses.foldLeft(List[Clause]()){ (accum, clause) =>
      val clauseCopies = this.finalTheories.tail.flatMap(theory => theory.clauses.filter(c => c.uuid == clause.uuid))
      if(clauseCopies.length + 1 != this.finalTheories.length) {
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






  /* Modify to handle data in intervals. */
  /*
  def getActorsPool() = {
    val taskNames = (1 to this.taskIntervals.length).map(i => s"dataset-$i").toList
    val NodeActorNames = taskNames map (db => s"Node-$db-${targetConcept.toString}")
    val globalsPool = taskNames.map(t => new Globals(inputParams.entryPath, t))
    val nodeActorsPool = (NodeActorNames, globalsPool, this.taskIntervals).zipped.toList map { x =>
      val (nodeName, global, nodeIntervals) = (x._1, x._2, x._3)
      val otherActors = NodeActorNames.filter(_ != nodeName)
      context.actorOf(Props(
        new Node(otherActors, masterDB, targetConcept, global, getDataFunction, inputParams, nodeIntervals)
      ), name = nodeName)
    }
    nodeActorsPool
  }
  */





}

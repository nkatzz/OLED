package oled.distributed

import akka.actor.{Actor, PoisonPill}
import app.runutils.IOHandling.Source
import app.runutils.RunningOptions
import com.madhukaraphatak.sizeof.SizeEstimator
import logic.Examples.Example
import logic.{Clause, Theory}
import oled.distributed.Structures._
import org.slf4j.LoggerFactory
import oled.functions.DistributedOLEDFunctions._

/**
  * Created by nkatz on 2/15/17.
  */

/* Represents a processing node in OLED's distributed setting. */

class Node[T <: Source](val otherNodesNames: List[String],
                        val targetConcept: TargetConcept,
                        val inputParameters: RunningOptions,
                        val trainingDataOptions: T,
                        val trainingDataFunction: T => Iterator[Example]) extends Actor {

  import context.become

  private val initorterm = targetConcept match {
    case x: Initiated => "initiatedAt"
    case x: Terminated => "terminatedAt"
  }

  // Get the training data from the current database
  def getTrainData = trainingDataFunction(trainingDataOptions)

  private var data = Iterator[Example]()
  private var currentTheory = List[Clause]()

  // This variable stores the replies from other nodes in response to a StatsRequest from this node
  private var statsReplies = List[StatsReply]()
  private var statsRepliesCount = 0

  // Control learning iterations over the data
  private var repeatFor = inputParameters.repeatFor

  /* FOR LOGGING-DEBUGGING */
  def showCurrentClauseUUIDs = s"(${this.currentTheory.length}): ${this.currentTheory.map(x => x.uuid).mkString("  ")}"
  /* FOR LOGGING-DEBUGGING */
  def showCurrentExpansionCandidatesUUIDS =
    s"(${getCurrentExpansionCandidates.length}): ${getCurrentExpansionCandidates.map(x => x.uuid).mkString("  ")}"
  /* FOR LOGGING-DEBUGGING */
  def showAlreadySpecialized = s"(${this.specializedSoFar.length}): ${this.specializedSoFar.mkString("  ")}"
  /* LOGGING-DEBUGGING */
  val showDebugMsgInLogs = false
  /* LOGGING-DEBUGGING */
  def showClausesDebugMsg = {
    if (showDebugMsgInLogs) {
      s"\nCurrent theory contains:" +
        s" $showCurrentClauseUUIDs\nExpansion candidates: $showCurrentExpansionCandidatesUUIDS\nAlready specialized: $showAlreadySpecialized\n"
    } else {""}
  }

  def getCurrentExpansionCandidates = {
    this.currentExpansionCandidates.filter(p => !this.specializedSoFar.contains(p.uuid))
  }

  // Monitor current state (that's just for debugging)
  var state = "starting"
  // for logging
  def showState = s"[in ${this.state} state] "
  // for logging
  val NORMAL_STATE = "normal"
  // for logging
  val EXPANSION_NODE_WAITING_STATE = "expansion node waiting"
  // for logging
  val STATS_REQUEST_SENDER_WAITING_STATE = "stats request sender waiting"
  // for logging
  val STATS_REQUEST_RECEIVER_WAITING_STATE = "stats request receiver waiting"
  // for logging
  val EXPANSION_NODE_NON_PRIORITY_STATE = "expansion node non-priority"
  // for logging
  def logNormalState = this.state = NORMAL_STATE
  def logExpansionNodeState = this.state = EXPANSION_NODE_WAITING_STATE
  def logRequestSenderState = this.state = STATS_REQUEST_SENDER_WAITING_STATE
  def logRequestReceiverState = this.state = STATS_REQUEST_RECEIVER_WAITING_STATE
  def logNonPriorityState = this.state = EXPANSION_NODE_NON_PRIORITY_STATE

  // This variable stores the uuid's of clauses that have been already specialized.
  // When this node finds an expansion candidate, it only proceeds to the necessary actions
  // to specialize that candidate, if its uuid is not found in this list. This is to avoid
  // a situation when this node infers that a clause C must be specialized, while it has
  // also received a similar request for C from another node. In that case, if C gets
  // specialized in the other node, there is no point in trying to specialize it again.
  private var specializedSoFar = List[String]()

  private var currentExpansionCandidates = List[Clause]()

  var finishedAndSentTheory = false

  /*
  * The logger for this class. Getting a logger this way instead of mixin-in the LazyLogging trait allows to
  * name the logger after a particular class instance, which is helpful for tracing messages
  * between different instances of the same class.
  * */
  private val slf4jLogger = LoggerFactory.getLogger(self.path.name)

  def logger_info(msg: String) = this.slf4jLogger.info(s"$showState $msg $showClausesDebugMsg")
  def logger_dubug(msg: String) = this.slf4jLogger.debug(s"$showState $msg $showClausesDebugMsg")

  private var messages = List[Long]()

  def updateMessages(m: AnyRef) = {
    val size = SizeEstimator.estimate(m)
    messages = messages :+ size
  }

  def receive = {
    // Start processing data. This message is received from the top level actor
    case "go" => start()
    //case "go-no-communication" => runNoCommunication()
    case _: ShutDown => self ! PoisonPill
  }

  def getNextBatch = {
    if (data.isEmpty) Iterator[Example]()
    else Utils.getNextBatch(data, inputParameters.processBatchBeforeMailBox)
  }

  def start() = {
    this.repeatFor -= 1
    logger_info(s"$showState Getting training data from db ${this.inputParameters.train}")
    // Get the training data into a fresh iterator
    this.data = getTrainData
    if (this.data.isEmpty) {
      slf4jLogger.error(s"DB ${inputParameters.train} is empty.")
      System.exit(-1)
    }
    // start at normal state
    become(normalState)
    // and send the first batch to self
    self ! getNextBatch
  }




  def normalState: Receive = {
    /*
     * This method encapsulates the normal behaviour of a Node instance. In its normal state a node can either:
     *
     * 1. Receive a batch of examples for processing. In this case it sends to itself the
     *    result of this processing in the form of a BatchProcessResult instance.
     *
     * 2. Receive a A BatchProcessResult instance (from itself). In this case it sends
     *    all newly generated clauses  to other nodes and requests stats for all clauses
     *    that are about to be expanded.
     *
     * 3. Receive one or more new clauses generated at some other node. In this case it simply adds these
     *    clauses to its current theory
     *
     * 4. Receive a request for stats for some candidates for expansion. In this case the node sends
     *    the requested stats and goes into a waiting state, where it waits until it has a reply on
     *    whether the candidates were actually expanded or not.
     *
     * */

    // for debugging, to check the actor's state
    case p: Ping => logger_info(s"Pinged by ${p.senderName}")

    case _: ShutDown => self ! PoisonPill

    case "start-over" =>
      logger_info(s"$showState Starting a new training iteration (${this.repeatFor - 1} iterations remaining.)")
      start() // re-starts according to the repeatFor parameter

    case chunk: Iterator[Example] =>
      // Receive a small example batch for processing. This is received from self.
      // If the batch is non-empty it is processed in the regular way, otherwise, if the batch is empty,
      // that means that the training data have been exhausted, so (based on the repeatFor parameter)
      // we either proceed to a new training iteration, or we wrap things up
      // (post-pruning if onlinePruning is not on etc).
      logNormalState
      if (chunk.isEmpty) {
        logger_info(s"$showState Finished the data")
        if (this.repeatFor > 0) {
          self ! "start-over"
        } else if (this.repeatFor == 0) {
          this.finishedAndSentTheory = true
          logger_info(s"$showState Sending the theory to the top-level actor")
          context.parent ! new NodeDoneMessage(self.path.name)
        } else {
          throw new RuntimeException("This should never have happened (repeatfor is now negative?)")
        }
      } else {
        self ! processBatch(chunk, this.slf4jLogger)
      }


    case result: BatchProcessResult =>
      logNormalState
      // Receive the result of processing a data batch.
      // If new clauses are generated, send them to other nodes
      if (result.newClauses.nonEmpty) {
        this.currentTheory = this.currentTheory ++ result.newClauses
        val copies = result.newClauses.map(x => Utils.copyClause(x))
        logger_info(s"Generated new clauses, sending them over...")
        Utils.getOtherActors(context, otherNodesNames) foreach { x =>
          val cls = new NewClauses(copies, self.path.name)
          updateMessages(cls)
          x ! cls
        }
      }

      // Handle expansion candidates
      if (result.toExpandClauses.nonEmpty) {
        this.currentExpansionCandidates = result.toExpandClauses.filter(p => !this.specializedSoFar.contains(p.uuid))
        logger_info(s"Found candidates for expansion. Requesting a specialization ticket from the top-level actor")
        // Switch to a waiting state
        become(expansionNodeWaitingState)
        // ...and request a specialization ticket from the top-level actor
        val ticket = new SpecializationTicketRequest(self.path.name, otherNodesNames)
        updateMessages(ticket)
        context.parent ! ticket
      }
      // When everything is done, send a request to self to process next batch. This request will be processed in time.
      // For example, if the node has entered a a waiting state, the new batch will be processed after all the logic in
      // the waiting state has been executed.
      if (!this.finishedAndSentTheory) self ! getNextBatch

    case nc: NewClauses =>
      logNormalState
      // Receive one or more new clauses. Simply add them to the current
      // theory for processing. This message is sent from another node
      // Add the copies to the current theory

      this.currentTheory = this.currentTheory ++ nc.newClauses
      logger_info(s"Received new clauses from ${nc.senderName}")

      /*This may be useful but needs some thinking to work*/
      /*
      if (!inputParameters.compressNewRules) {
        this.currentTheory = this.currentTheory ++ nc.newClauses
        logger_info(s"Received new clauses from ${nc.senderName}")
      } else {
        val bottomClauses = this.currentTheory.map(x => x.supportSet.clauses.head)
        for (clause <- nc.newClauses) {
          val bottom = clause.supportSet.clauses.head
          if (!bottomClauses.exists(x => x.thetaSubsumes(bottom) && bottom.thetaSubsumes(x))) {
            this.currentTheory = this.currentTheory :+ clause
            logger_info(s"Received new clause from ${nc.senderName} (compressNewClause is on)")
          } else {
            logger_info(s"Received new clause from ${nc.senderName} but it was dropped (compressNewClause is on)")
          }
        }
      }
      */


    case request: StatsRequest =>
      logNormalState
      // When a processing node receives a StatsRequest, it sends a StatsReply to the
      // the sender of the request, containing all necessary information (counts) for
      // the sender to calculate updated Hoeffding bound heuristics. It then enters a
      // statsRequestReceiverwaitingState where it waits for the verdict on whether each
      // candidate for expansion has been expanded or not.
      handleStatsRequest(request)
      become(statsRequestReceiverWaitingState)

    case msg: AnotherNodeIsSpecializing =>
      //logNormalState
      //logger.info(s"$showState Node ${msg.specializingNodeName} has found expansion candidates. Switching to a statsRequestReceiverWaitingState")
      //become(statsRequestReceiverWaitingState)

    /*
    * This is a request for the learnt theory from the coordinating TopLevelActor
    * */
    case msg: TheoryRequestMessage =>
      val msgNum = this.messages.length
      val msgSize = this.messages.sum
      sender ! new NodeTheoryMessage(Theory(this.currentTheory), msgNum, msgSize, self.path.name)

    case x: SpecializationTicket => println("this shouldn't have happened")
  }



  def expansionNodeWaitingState: Receive = {
    /*
    * A node enters this state whenever it detects some candidate for expansion clauses. It then notifies the
    * top-level actor and enters this state, where it waits either for a specialization ticket (meaning that
    * the top-level actor has prioritized this actor to proceed with handling its candidates), or a "no-go" response,
    * meaning that the top-level actor has prioritized some other actor to proceed with its own expansion candidates.
    * Note that in the latter case, that "other actor" has also detected some expansion candidates simultaneously, or
    * in very close temporal proximity with this actor. The top-level actor received both requests almost simultaneously,
    * and it randomly prioritized one of the requesting actors to proceed. In detail, the behaviour of a node while in this
    * state is determined by the types of message it receives and the way its handles these messages:
    *
    * 1. Receive a SpecializationTicket from the top-level actor. In this case this node has been prioritized by the top-level actor
    *    to proceed with handling its expansion candidates. Then this node sends out a StatsRequest to the other nodes requesting counts for
    *    its candidates. It then switches to a statsRequestSenderWaitingState, where it waits for the replies from the other nodes.
    *
    * 2. Receive an AnotherNodeIsSpecializing message from the top-level actor. This is just a "letting-you-kow" message that another node
    *    has been prioritized over this one to send out its stats request. In that case, this node simply switches to a expansionNodeNonPriorityState
    *    where it can handle the stats request from the prioritized node.

    *     to a normal state, as it would have done
    *    if it were in a statsRequestReceiverWaitingState. Instead, it remains in this state and wait for its own specialization ticket.
    * */

    // for debugging, to check the actor's state
    case p: Ping => logger_info(s"Pinged by ${p.senderName}")

    case _: ShutDown => self ! PoisonPill

    case go: SpecializationTicket =>
      logExpansionNodeState
      val candidates = getCurrentExpansionCandidates
      if (candidates.nonEmpty) {
        logger_info(s"Just got a specialization ticket.")

        // send a StatsRequest msg to all other nodes
        val otherNodes = Utils.getOtherActors(context, otherNodesNames)

        val x = new StatsRequest(candidates map (_.uuid), self.path.name)
        println(SizeEstimator.estimate(x))

        otherNodes foreach { x =>
          val request = new StatsRequest(candidates map (_.uuid), self.path.name)
          updateMessages(request)
          x ! request
        }
        /*
        otherNodesNames foreach { name =>
          val actor = Utils.getActorByName(context, name)
          actor ! new StatsRequest(candidates map (_.uuid), self.path.name)
          println(s"${self.path.name} sent the stats request to $name")
        }
        */
        // Then enter a waiting state. Wait until all replies from other actors are collected.
        this.statsRepliesCount = otherNodes.length // clear the reply counter
        this.statsReplies = List[StatsReply]()     // clear the reply storage
        become(statsRequestSenderWaitingState)
      } else {
        //logger_info(s"Got a specialization ticket, but all candidates have already been specialized.")
        this.currentExpansionCandidates = Nil
        context.parent ! new ExpansionAbortMsg(self.path.name)
        become(normalState)
      }

    case nogo: AnotherNodeIsSpecializing =>
      logExpansionNodeState
      logger_info(s"Postponing expansion, queued by node ${nogo.specializingNodeName}.")
      become(expansionNodeNonPriorityState)

    /*
    case request: StatsRequest =>
      logExpansionNodeState
      become(expansionNodeNonPriorityState)
      logger_info("THIS SHOULD NEVER HAVE HAPPENED")
      handleStatsRequest(request)
    */

    case request: StatsRequest =>
      logExpansionNodeState
      //become(expansionNodeNonPriorityState)
      //logger_info(s"RECEIVED A StatsRequest FROM ${request.senderName}, SWITCHING TO NON-PRIORITY STATE & FW THE REQUEST THERE")
      become(expansionNodeNonPriorityState)
      self ! request
      //handleStatsRequest(request)

  }

  def expansionNodeNonPriorityState: Receive = {
    /*
    * A node enters this state when it is in a expansionNodeWaitingState and it  receives an AnotherNodeIsSpecializing
    * message from the top-level actor. The latter is just a "letting-you-kow" message that another node has been
    * prioritized over this one to send out its stats request. In this case, this node serves the prioritized node by
    * providing replies to the received stats request and it waits for the verdicts on the prioritized node's candidates
    * (expansion/no expansion). A node's behaviour while in this state is as follows:
    *
    * 1. Receive a StatsRequest. This is sent out from the prioritized node. This node treats the request in the regular way,
    *    responding with a StatsReply to the sender of the StatsRequest. It does not switch to statsRequestReceiverWaitingState.
    *    It remains in this state, since all functionality of the statsRequestReceiverWaitingState (receiving an ExpansionReply
    *    from the sender of the StatsRequest, and replacing any specialized clauses in this.currentHypothesis) is available
    *    in the current state too. Remaining in the current state allows this node to serve the node currently prioritized for
    *    expansion, while waiting for its turn to get its own SpecializationTicket from the top-level actor.
    *
    * 2. Receive an ExpansionReply from the node currently prioritized for expansion, containing a verdict on its candidates
    *   (expansion/no expansion). When this node receives an ExpansionReply it handles it normally by replacing the specialized
    *   clauses in its current theory (see the handleExpansionReply method for details). After that, this node switches
    *   back to a expansionNodeWaitingState, where it will either get its own turn to proceed with its candidates, or it will
    *   get another AnotherNodeIsSpecializing, leading the node back to this state. Eventually, its Specialization ticket will
    *   be processed, leading the node back to normalState.
    *
    * */

    // for debugging, to check the actor's state
    case p: Ping => logger_info(s"Pinged by ${p.senderName}")

    case _: ShutDown => self ! PoisonPill

    case request: StatsRequest =>
      logNonPriorityState
      // Handle the stats request in the regular way and send back the requested counts.
      handleStatsRequest(request)

    case msg: AnotherNodeIsSpecializing =>
      logNonPriorityState
      // do nothing here, just process the next message from the mailbox
      logger_info(s"Got an AnotherNodeIsSpecializing message. Node ${msg.specializingNodeName} is prioritized for clause expansion")

    case reply: ExpansionReply =>
      logNonPriorityState
      // Handle the expansion reply in the regular way
      handleExpansionReply(reply)
      become(expansionNodeWaitingState)

    case _: PrioritizedNodeAbortedExpansion => become(expansionNodeWaitingState)
  }

  def statsRequestSenderWaitingState: Receive = {
    /*
     * This method encapsulates the behaviour of a processing node while it is waiting for replies on
     * a stats request it has sent out to other actors. While in this state, the node can only process
     * a StatsReply type of message. The actor waits in this state until all replies from all nodes
     * are collected. Then, for each clause in the candidates list (the list of expansion candidates that
     * are input to this method) the node re-calculates expansion hueristics with the accumulated counts
     * from all nodes for this clause. Finally, it sends an ExpansionReply to all other nodes (which in the
     * meantime have been waiting for these expansion replies in statsRequestReceiverwaitingState).
     * The expansion reply contains a list of intact rules and a list of expanded rules. Each receiver
     * of the expansion reply updates its current theory accordingly.
     * */

    // ---------------------------------------------------------------------------------------------------------------
    // This is just for debugging. Receiving a stats request while in a statsRequestSenderWaitingState
    // means that another node is also in the same state, which is a deadlock, since both actors (this and the other)
    // are waiting for StatsReplies, which will never arrive...
    // ---------------------------------------------------------------------------------------------------------------

    // for debugging, to check the actor's state
    case p: Ping => logger_info(s"Pinged by ${p.senderName}")

    case _: ShutDown => self ! PoisonPill

    case x: StatsRequest =>
      logRequestSenderState
      throw new RuntimeException("XXXXXXXXXX: received a stats" + " request while in a statsRequestSenderWaitingState. Something's really wrong!")

    case reply: StatsReply =>
      logRequestSenderState
      logger_info(s"Received a StatsReply from node ${reply.sender}: $reply")
      this.statsRepliesCount -= 1
      this.statsReplies = this.statsReplies :+ reply
      if (this.statsRepliesCount == 0) {
        // all replies have been received from all nodes.
        // Time to decide which candidates will eventually be expanded.
        val (delta, ties, minseen) = (inputParameters.delta,inputParameters.breakTiesThreshold,inputParameters.minSeenExmpls)
        val checked = getCurrentExpansionCandidates map (clause => Utils.expand_?(clause, this.statsReplies, delta, ties, minseen, showState, self.path.name, inputParameters, slf4jLogger))

        val (expanded, notExpanded) = checked.foldLeft(List[Clause](), List[Clause]()){ (x, y) =>
          val (expandAccum, notExpandAccum) = (x._1, x._2)
          val (expandedFlag, clause)  = (y._1, y._2)
          if (expandedFlag) (expandAccum :+ clause, notExpandAccum) else (expandAccum, notExpandAccum :+ clause)
        }
        // replace the expanded in the current theory
        if (expanded.nonEmpty) {
          expanded.foreach { expanded =>
            val theoryWithout = this.currentTheory.filter(p => p.uuid != expanded.parentClause.uuid)
            if (this.currentTheory.length == theoryWithout.length) {
              // then the parent clause of the expanded one is not found in current theory, which is an error...
              //throw new RuntimeException(s"$showState Cannot find parent clause in current theory")
            }
            val theoryWith = theoryWithout :+ expanded
            // Remember the uuid's of expanded clauses
            this.specializedSoFar = this.specializedSoFar :+ expanded.parentClause.uuid
            this.currentTheory = theoryWith
          }
        }
        // Clear the currentExpansionCandidates variable
        this.currentExpansionCandidates = Nil


        // finally, send the reply the all other actors (which are currently in a statsReceiverwaitingState)
        val otherNodes = Utils.getOtherActors(context, otherNodesNames)
        otherNodes foreach { x =>
          val reply = new ExpansionReply(notExpanded, expanded.map(p => Utils.copyClause(p)), self.path.name)
          updateMessages(reply)
          x ! reply
        }
        // notify the top-level actor
        context.parent ! new ExpansionFinished(notExpanded.map(_.uuid), expanded.map(_.parentClause.uuid), self.path.name, otherNodesNames)
        // ...and switch back to normal state to continue processing.
        // Before switching, send to self a request to process a new batch. This request will be processed
        // once all messages the have been accumulated in the mailbox, while the node was in the
        // statsRequestSenderWaitingState, have been processed (so the actor continues processing).
        if (!this.finishedAndSentTheory) self ! getNextBatch
        become(normalState)
      }
  }

  def statsRequestReceiverWaitingState: Receive = {

    // for debugging, to check the actor's state
    case p: Ping => logger_info(s"Pinged by ${p.senderName}")

    case _: ShutDown => self ! PoisonPill

    case reply: ExpansionReply =>
      logRequestReceiverState
      handleExpansionReply(reply)
      // when the replacement of expanded clauses finishes, switch back to normal state.
      // Also send a request to process a new batch (once in normal state and the mailbox is empty).
      become(normalState)
      if (!this.finishedAndSentTheory) self ! getNextBatch
  }

  /* Handle a stats request */
  def handleStatsRequest(request: StatsRequest) = {
    logger_info(s"Received a StatsRequest from node ${request.senderName}")
    val statsObject =
      (for (uuid <- request.candidatesIds) yield
        /*
        this.currentTheory.find(c => c.uuid == uuid).getOrElse(
          throw new RuntimeException(s"$showState Could not find expansion candidate with uuid $uuid in the current theory"))
        */
        this.currentTheory.find(c => c.uuid == uuid).getOrElse(Clause())
        ).map(a => a.uuid -> Stats( ClauseStats(a.tps, a.fps, a.fns, a.seenExmplsNum),
        a.refinements.map(r => r.uuid -> ClauseStats(r.tps, r.fps, r.fns, r.seenExmplsNum)).toMap )).toMap
    val reply = new StatsReply(statsObject, self.path.name)
    updateMessages(reply)
    Utils.getActorByName(context, request.senderName) ! reply
  }

  /* Handle an expansion reply */
  def handleExpansionReply(reply: ExpansionReply) = {
    logger_info(s"Received an ExpansionReply from node ${reply.senderName}")
    // We don't do anything for intact clauses. But we need to replace the expanded clauses in the
    // current theory.
    if (reply.expandedClauses.nonEmpty) {
      reply.expandedClauses.foreach { expanded =>
        val theoryWithout = this.currentTheory.filter(p => p.uuid != expanded.parentClause.uuid)
        if (this.currentTheory.length == theoryWithout.length) {
          // then the parent clause of the expanded one is not found in current theory, which is an error...
          //throw new RuntimeException(s"$showState Cannot find parent clause in current theory")
        }
        val theoryWith = theoryWithout :+ expanded
        // Remember the uuid's of expanded clauses
        this.specializedSoFar = this.specializedSoFar :+ expanded.parentClause.uuid
        this.currentTheory = theoryWith
      }
    }
  }

  /*
  *
  * Process a small batch of examples. This method returns two lists:
  * The first contains all new rules that were created from the input
  * batch, while the second list contains all rules that are about to be
  * expanded.
  * */
  def processBatch(exmpls: Iterator[Example], logger: org.slf4j.Logger): BatchProcessResult = {

    def filterTriedRules(newRules: List[Clause]) = {
      val out = newRules.filter{ newRule =>
        val bottomClauses = this.currentTheory.map(x => x.supportSet.clauses.head)
        val bottom = newRule.supportSet.clauses.head
        !bottomClauses.exists(x => x.thetaSubsumes(bottom) && bottom.thetaSubsumes(x))
      }
      if (out.length != newRules.length) logger.info("Dropped new clause (repeated bottom clause)")
      out
    }

    val out = utils.Utils.time {
      var newTopTheory = Theory(this.currentTheory)
      val (newRules_, expansionCandidateRules_) = exmpls.foldLeft(List[Clause](), List[Clause]()) { (x, e) =>
        var (newRules, expansionCandidateRules) = (x._1, x._2)
        val startNew = newTopTheory.growNewRuleTest(e, initorterm, this.inputParameters.globals)
        if (startNew) {
          newRules = generateNewRules(newTopTheory, e, initorterm, this.inputParameters.globals, this.otherNodesNames)

          // Just to be on the same side...
          newRules.filter(x => x.head.functor == this.initorterm)

          newTopTheory = Theory(newTopTheory.clauses ++ newRules)
        }
        if (newTopTheory.clauses.nonEmpty) {
          newTopTheory.scoreRules(e, this.inputParameters.globals)
        }
        for (rule <- newTopTheory.clauses) {
          val (delta, ties, seen) = (inputParameters.delta, inputParameters.breakTiesThreshold, inputParameters.minSeenExmpls)
          if (shouldExpand(rule, delta, ties, seen)) {
            expansionCandidateRules = expansionCandidateRules :+ rule
          }
        }
        (newRules, expansionCandidateRules)
      }
      (newRules_, expansionCandidateRules_)
    }
    val (newRules, expansionCandidateRules, time) = (out._1._1, out._1._2, out._2)
    //Globals.timeDebug = Globals.timeDebug :+ time
    if (inputParameters.compressNewRules) {
      new BatchProcessResult(filterTriedRules(newRules), expansionCandidateRules)
    } else {
      new BatchProcessResult(newRules, expansionCandidateRules)
    }


  }




}
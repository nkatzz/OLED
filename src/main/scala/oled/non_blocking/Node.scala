package oled.non_blocking

import akka.actor.{Actor, PoisonPill}
import app.runutils.IOHandling.Source
import app.runutils.RunningOptions
import com.madhukaraphatak.sizeof.SizeEstimator
import logic.Examples.Example
import logic.{Clause, Theory}
import oled.distributed.Structures._
import oled.functions.NonBlockingOLEDFunctions._
import org.slf4j.LoggerFactory

/**
  * Created by nkatz on 2/15/17.
  */

/* Represents a processing node in OLED's distributed setting. */

class Node[T <: Source](val otherNodesNames: List[String],
                        val targetConcept: TargetConcept,
                        val inputParameters: RunningOptions,
                        val trainingDataOptions: T,
                        val trainingDataFunction: T => Iterator[Example]) extends Actor {

  private val initorterm = targetConcept match {
    case x: Initiated => "initiatedAt"
    case x: Terminated => "terminatedAt"
  }

  // Get the training data from the current database
  def getTrainData: Iterator[Example] = trainingDataFunction(trainingDataOptions)

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


  // This variable stores the uuid's of clauses that have been already specialized.
  // When the node finds an expansion candidate, it only proceeds to the necessary actions
  // to specialize that candidate, if its uuid is not found in this list. This is to avoid
  // a situation when the node infers that a clause C must be specialized, while it has
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

    case "start-over" =>
      logger_info(s"$showState Starting a new training iteration (${this.repeatFor - 1} iterations remaining.)")
      start() // re-starts according to the repeatFor parameter

    case _: ShutDown => self ! PoisonPill

    case p: Ping => logger_info(s"Pinged by ${p.senderName}")

    case chunk: Iterator[Example] =>
      processNewChunck(chunk)

    case result: BatchProcessResult =>
      handleBatchResult(result)

    case reply: StatsReply =>
      handleStatsReply(reply)

    case reply: ExpansionReply =>
      handleExpansionReply(reply)
      if (!this.finishedAndSentTheory) self ! getNextBatch

    case nc: NewClauses =>
      this.currentTheory = this.currentTheory ++ nc.newClauses
      logger_info(s"Received new clauses from ${nc.senderName}")

    case request: StatsRequest => handleStatsRequest(request)

    case _ : TheoryRequestMessage =>
      val msgNum = this.messages.length
      val msgSize = this.messages.sum
      sender ! new NodeTheoryMessage(Theory(this.currentTheory), msgNum, msgSize, self.path.name)

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
    // Send the first batch to self
    self ! getNextBatch
  }



  def processNewChunck(chunk: Iterator[Example]) = {
    if (! this.finishedAndSentTheory) {
      if (chunk.isEmpty) {
        logger_info(s"Finished the data")
        if (this.repeatFor > 0) {
          self ! "start-over"
        } else if (this.repeatFor == 0) {
          this.finishedAndSentTheory = true
          context.parent ! new NodeDoneMessage(self.path.name)
          logger_info(s"Sent the theory to the top-level actor")
        } else {
          throw new RuntimeException("This should never have happened (repeatfor is now negative?)")
        }
      } else {
        self ! processBatch(chunk, this.slf4jLogger)
      }
    }
  }

  def handleBatchResult(result: BatchProcessResult) = {
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
      val candidates = getCurrentExpansionCandidates
      if (candidates.nonEmpty) {
        logger_info(s"Found candidates for expansion: ${candidates.map(x => x.uuid).mkString(" ")} Requesting stats")
        // send a StatsRequest msg to all other nodes
        val otherNodes = Utils.getOtherActors(context, otherNodesNames)

        val x = new StatsRequest(candidates map (_.uuid), self.path.name)
        println(s"Msg size: ${SizeEstimator.estimate(x)}")

        otherNodes foreach { x =>
          val request = new StatsRequest(candidates map (_.uuid), self.path.name)
          updateMessages(request)
          x ! request
        }
        this.statsRepliesCount = otherNodes.length // clear the reply counter
        this.statsReplies = List[StatsReply]()     // clear the reply storage
      }
    }

    // When everything is done, send a request to self to process next batch.
    if (!this.finishedAndSentTheory) self ! getNextBatch
  }



  def handleStatsReply(reply: StatsReply) = {
    logger_info(s"Received a StatsReply from node ${reply.sender}: $reply")
    this.statsRepliesCount -= 1
    this.statsReplies = this.statsReplies :+ reply
    if (this.statsRepliesCount == 0) {
      // all replies have been received from all nodes.
      // Time to decide which candidates will eventually be expanded.
      val (delta, ties, minseen) = (inputParameters.delta,inputParameters.breakTiesThreshold,inputParameters.minSeenExmpls)
      val checked = getCurrentExpansionCandidates map (clause =>
        Utils.expand_?(clause, this.statsReplies, delta, ties, minseen, showState, self.path.name, inputParameters, slf4jLogger, blocking = false))

      val (expanded, notExpanded) = checked.foldLeft(List[Clause](), List[Clause]()){ (x, y) =>
        val (expandAccum, notExpandAccum) = (x._1, x._2)
        val (expandedFlag, clause)  = (y._1, y._2)
        if (expandedFlag) (expandAccum :+ clause, notExpandAccum) else (expandAccum, notExpandAccum :+ clause)
      }
      // replace the expanded in the current theory
      if (expanded.nonEmpty) {
        expanded.foreach { expanded =>
          val theoryWithout = this.currentTheory.filter(p => p.uuid != expanded.parentClause.uuid)
          /*
          if (this.currentTheory.length == theoryWithout.length) {
            // then the parent clause of the expanded one is not found in current theory, which is an error...
            throw new RuntimeException(s"$showState Cannot find parent clause in current theory")
          }
          */
          val theoryWith = theoryWithout :+ expanded
          // Remember the uuid's of expanded clauses
          this.specializedSoFar = this.specializedSoFar :+ expanded.parentClause.uuid
          this.currentTheory = theoryWith
        }
      }
      // Clear the currentExpansionCandidates variable
      this.currentExpansionCandidates = Nil
      // finally, send the reply to all other actors (which are currently in a statsReceiverwaitingState)
      val otherNodes = Utils.getOtherActors(context, otherNodesNames)
      otherNodes foreach { x =>
        val reply = new ExpansionReply(notExpanded, expanded.map(p => Utils.copyClause(p)), self.path.name)
        updateMessages(reply)
        x ! reply
      }
      if (!this.finishedAndSentTheory) self ! getNextBatch
    }
  }





  /* Handle a stats request */
  def handleStatsRequest(request: StatsRequest) = {
    logger_info(s"Received a StatsRequest from node ${request.senderName}")
    val statsObject =
      (for (uuid <- request.candidatesIds) yield
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
        /*
        if (this.currentTheory.length == theoryWithout.length) {
          // then the parent clause of the expanded one is not found in current theory, which is an error...
          throw new RuntimeException(s"$showState Cannot find parent clause in current theory")
        }
        */
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
        for (rule <- newTopTheory.clauses){

          //.filter(p => !this.getCurrentExpansionCandidates.contains(p))) {

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
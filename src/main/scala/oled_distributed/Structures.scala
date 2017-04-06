package oled_distributed

import logic.{Theory, Clause}
import oled_distributed.Structures.TargetConcept

/**
  * Created by nkatz on 2/23/17.
  */
object Structures {

  class NodeDoneMessage(val sender: String)
  class NodeTheoryMessage(val theory: Theory, val sender: String)
  class TheoryRequestMessage(val sender: String)

  /*
   * This message represents the result of processing a small batch of examples.
   * It contains a (potentially empty) set of new clauses, generated from this
   * batch and a (potentially empty) set of clauses that are candidates for expansion.
   * Such a message is sent from a Node instance to its self.
   * */
  class BatchProcessResult(val newClauses: List[Clause], val toExpandClauses: List[Clause])

  /*
   * This class represents a message sent to all nodes
   * from a node that generates one or more new clauses.
   * */
  class NewClauses(val newClauses: List[Clause], val senderName: String)

  /*
   * This class represents a message sent to all nodes from a node that has one
   * of more clauses that are about to be expanded to their best-scoring
   * specialization. This message serves as a request to the other nodes to send
   * back (to the sender of the msg) their counts for the particular expansion candidates.
   * The message contains (in addition to the sender's name for debugging), a list
   * of the expansion candidates' uuid's that suffice for each receiver node to track the candidates.
   * */
  class StatsRequest(val candidatesIds: List[String], val senderName: String)

  /*
   * This class represents a message sent from a node that checks if a clause expansion is still valid,
   * after it has received additional counts for that clause from all other nodes. This message is a reply
   * sent to all the other nodes and it contains both the candidates that were eventually expanded and those
   * that did not. The receiver actors of such a message such act accordingly (retain all the non-expanded
   * clauses and replace the parents of the expanded ones with the expanded clauses in their current theories).
   * */
  class ExpansionReply(val intactClauses: List[Clause], val expandedClauses: List[Clause], val senderName: String)

  //class ExpandedClause(parentId: String, refinement: Clause)

  /*
   * This class encapsulates the stats sent from all other
   * nodes as a response to an expansion request. statsMap is
   * a (k,v) map where k is the uuid for each clause that is candidate
   * for expansion and v is a stats object for that candidate.
   * So, in response to one StatsRequest from a particular
   * node (requesting the stats for a set of expansion candidates),
   * each other node sends one StatsReply, containing the stats for each such candidate.
   * The sender field is the id (name) of the Node actor that sends the StatsReply
   * */
  class StatsReply(val statsMap: Map[String, Stats], val sender: String) {
    // utility getter method
    def getClauseStats(id: String) = {
      statsMap.getOrElse(id, throw new RuntimeException(s"Clause $id not found in stats map"))
    }
  }

  /* just a container. parentStats is an object carrying the counts for a parent clause and refinementsStats is
  *  a (k,v) map where k is the uuid of a parent clause's refinement and v is an object carrying the counts for
  *  that refinement. */
  class Stats(val parentStats: ClauseStats, val refinementsStats: Map[String, ClauseStats])

  class ClauseStats(val tps: Int, val fps: Int, val fns: Int, val Nexmls: Int)

  class SpecializationTicket(val senderName: String)

  class SpecializationTicketRequest(val senderName: String, val otherNodeNames: List[String])

  class AnotherNodeIsSpecializing(val specializingNodeName: String)

  class ExpansionFinished(val intactClausesIds: List[String], val expandedClausesIds: List[String], val senderName: String, val otherNodeNames: List[String])

  class QueuedExpandingNode(val senderName: String, val otherNodesNames: List[String])

  class FinalTheoryMessage(val theory: List[Clause], val trainingTime: String, val targetPredicate: TargetConcept)

  class ExpansionAbortMsg(val abortingNodeName: String)

  class PrioritizedNodeAbortedExpansion(val abortingNodeName: String)

  class Ping(val senderName: String)

  class ShutDown(val senderName: String)

  abstract class TargetConcept
  case class Initiated() extends TargetConcept {override def toString() = "initiated"}
  case class Terminated() extends TargetConcept {override def toString() = "terminated"}
  case class WhatEver() extends TargetConcept{override def toString() = ""}

}


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

import logic.{Clause, Theory}

/**
  * Created by nkatz on 2/23/17.
  */
object Structures {

  class NodeDoneMessage(val sender: String)
  class NodeTheoryMessage(val theory: Theory, val msgNum: Int, val msgSize: Long, val sender: String)
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
    def getClauseStats(id: String, blocking: Boolean = true) = {
      if (blocking) {
        //statsMap.getOrElse(id, throw new RuntimeException(s"Clause $id not found in stats map"))
        statsMap.getOrElse(id, Stats())
        statsMap.getOrElse(id, Stats())
      } else {
        statsMap.getOrElse(id, Stats())
      }
    }
  }

  /* just a container. parentStats is an object carrying the counts for a parent clause and refinementsStats is
  *  a (k,v) map where k is the uuid of a parent clause's refinement and v is an object carrying the counts for
  *  that refinement. */

  case class Stats(parentStats: ClauseStats = ClauseStats(), refinementsStats: Map[String, ClauseStats] = Map[String, ClauseStats]())

  case class ClauseStats(tps: Int = 0, fps: Int = 0, fns: Int = 0, Nexmls: Int = 0)

  class SpecializationTicket(val senderName: String)

  class SpecializationTicketRequest(val senderName: String, val otherNodeNames: List[String])

  class AnotherNodeIsSpecializing(val specializingNodeName: String)

  class ExpansionFinished(val intactClausesIds: List[String], val expandedClausesIds: List[String], val senderName: String, val otherNodeNames: List[String])

  class QueuedExpandingNode(val senderName: String, val otherNodesNames: List[String])

  class FinalTheoryMessage(val theory: List[Clause], val trainingTime: String,
      val totalMsgNum: Int, val totalMsgSize: Long,
      val targetPredicate: TargetConcept)

  class ExpansionAbortMsg(val abortingNodeName: String)

  class PrioritizedNodeAbortedExpansion(val abortingNodeName: String)

  class Ping(val senderName: String)

  class ShutDown(val senderName: String)

  abstract class TargetConcept
  case class Initiated() extends TargetConcept { override def toString() = "initiated" }
  case class Terminated() extends TargetConcept { override def toString() = "terminated" }
  case class WhatEver() extends TargetConcept { override def toString() = "" }

}


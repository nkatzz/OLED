package oled_distributed

import akka.actor.{ActorContext, ActorSelection}
import com.mongodb.casbah.{MongoCollection, MongoClient}
import com.mongodb.casbah.commons.MongoDBObject
import logic.{Theory, Literal, PosLiteral, Clause}
import logic.Examples.Example
import oled_distributed.Structures.{Stats, StatsReply}
import utils.Exmpl

/**
  * Created by nkatz on 2/15/17.
  */

object Utils {

  def getCaviarData(mc: MongoClient, dbName: String, chunkSize: Int): Iterator[List[String]] = {
    val collection = mc(dbName)("examples")
    collection.find().map(x => Example(x)).grouped(chunkSize).map( x =>
      x.foldLeft(List[String]())((z, y) => z ++ y.annotation ++ y.narrative) )
  }

  /* utility function for retrieving data */
  def getDataFromDB(dbName: String, HLE: String, chunkSize: Int): Iterator[Exmpl] = {
    // No worry about removing prior annotation from the examples, since in any case inertia
    // is not used during learning. Even if a pair is passed where in both times
    // there is positive annotation, the first positive example will be covered by
    // the initalTime axiom, while the second positive will be covered by abduction (no inertia).
    val mc = MongoClient()
    val collection = mc(dbName)("examples")

    val data = collection.find().sort(MongoDBObject("time" -> 1)).map { x =>
      val e = Example(x)
      new Example(annot = e.annotation filter (_.contains(HLE)), nar = e.narrative, _time = e.time)
    }
    val dataChunked = data.grouped(chunkSize)
    val dataIterator = dataChunked.map { x =>
      val merged = x.foldLeft(Example()) { (z,y) =>
        new Example(annot = z.annotation ++ y.annotation, nar = z.narrative ++ y.narrative, _time = x.head.time)
      }
      new Exmpl(_id = merged.time, exampleWithInertia = merged)
    }
    //val shuffled = scala.util.Random.shuffle(dataIterator)
    //shuffled
    dataIterator
  }

  def getExmplIteratorSorted(collection: MongoCollection) = {
    collection.find().sort(MongoDBObject("time" -> 1))
  }

  def getExmplIteratorShuffle(collection: MongoCollection) = {

  }

  // Utility function, returns a list of other Node actors
  def getOtherActors(context: ActorContext, otherNodesNames: List[String]): List[ActorSelection] = {
    otherNodesNames map(actorName => context.actorSelection(s"${context.parent.path}/$actorName"))
  }

  def getActorByName(context: ActorContext, name: String) = {
    context.actorSelection(s"${context.parent.path}/$name")
  }

  // Utility function, returns a new small example batch for processing
  def getNextBatch(data: Iterator[Exmpl], processBatchBeforeMailBox: Int, dbName: String) = {
    data.take(processBatchBeforeMailBox)
  }

  /*
  * Decide if a clause will be expanded or not, after taking into account the new counts
  * from all nodes. clause is the clause in question, replies is a list of StatsReply objects
  * received from all nodes and the remaining parameters are for calculating the hoeffding bound.
  * This method returns a (b, c) tuple, where b is true of false, according to whether the input
  * clause will be expanded or not and c either the input clause (if b = false) or its best
  * specialization (if b = true).
  * */
  def expand_?(clause: Clause, replies: List[StatsReply], delta: Double,
               breakTiesThreshold: Double, minSeenExmpls: Int, currentNodeState: String, nodeName: String) = {

    val repliesGroupedByNode = (for (r <- replies) yield (r.sender, r.getClauseStats(clause.uuid))).toMap

    // update the counts per node for each node, for this clause and for each one of its refinements
    repliesGroupedByNode.keys foreach { node =>
      updateCountsPerNode(clause, node, repliesGroupedByNode, currentNodeState, nodeName)
    }

    // Re-check the clause for expansion
    Functions.expandRule(clause, delta, breakTiesThreshold, minSeenExmpls, nodeName)
  }

  /*
   * Returns the new counts (by subtracting the old ones from the received ones)
   * for clause c and for node nodeName. The output is a new stats object with the counts,
   * along with nodeName (in order to update c.previousCountsPerNode). The replies map
   * is a (k,v) map where k is a node id and v is a stats object sent from node k for clause c.
   * */
  def updateCountsPerNode(clause: Clause, nodeName: String, replies: Map[String, Structures.Stats], currentNodeState: String, currentlyOnNode: String): Unit = {
    val receivedStats = replies.getOrElse(nodeName,
        throw new RuntimeException(s"$currentNodeState Could not find node's name $nodeName as key in the nodes-stats map. The map is $replies")
    )
    val parentClauseStats = receivedStats.parentStats
    val refinementsStats = receivedStats.refinementsStats
    clause.countsPerNode(nodeName) = parentClauseStats // update the countsPerNode map
    clause.updateTotalCounts(currentlyOnNode) // Update the accumulated counts variables

    // just to be on the safe side...
    if (refinementsStats.size != clause.refinements.length) {
      throw new RuntimeException(s"$currentNodeState Problem with refinements reply!")
    }

    clause.refinements.foreach{ ref =>
      val refStats = refinementsStats.getOrElse(ref.uuid, throw new RuntimeException(s"$currentNodeState Refinement ${ref.uuid} not found in the returned stats map"))
      ref.countsPerNode(nodeName) = refStats // update the refinement's countsPerNode map
      ref.updateTotalCounts(currentlyOnNode) // Update the accumulated counts variables
    }
  }

  def copyClause(c: Clause) = {

    def basicopy(clause: Clause) = {
      val copy_ = Clause(head = clause.head, body = clause.body, uuid = clause.uuid)
      //copy_.uuid = clause.uuid
      copy_.tps = clause.tps
      copy_.fps = clause.fps
      copy_.fns = clause.fns
      copy_.seenExmplsNum = clause.seenExmplsNum
      copy_.countsPerNode = clause.countsPerNode
      //copy_.generatedAtNode = clause.generatedAtNode

      // don't copy these, there's no need (nothing changes in the parent clause or the support set) and copying
      // it makes it messy to retrieve ids in other nodes
      copy_.parentClause = clause.parentClause
      copy_.supportSet = clause.supportSet
      copy_
    }
    val copy = basicopy(c)
    val refinementsCopy = c.refinements.map(ref => basicopy(ref))
    copy.refinements = refinementsCopy
    copy
  }

}


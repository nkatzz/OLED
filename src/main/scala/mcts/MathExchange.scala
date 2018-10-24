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

package mcts

import app.runutils.Globals
import com.mongodb.casbah.Imports.{BasicDBList, BasicDBObject}
import com.mongodb.casbah.MongoClient
import logic.Examples.Example
import logic.Theory
import com.mongodb.casbah.Imports._
import com.typesafe.scalalogging.LazyLogging
import mcts.HillClimbing._

/**
  * Created by nkatz on 9/22/17.
  */

/**
  * This is just a test to run MCTS with the MathExchange data
  * */

object MathExchange extends App with LazyLogging {

  Globals.glvalues("perfect-fit") = "false"

  val chunkSize = 10

  val globals = new Globals("/home/nkatz/dev/MathExchange-for-OLED")

  val data = {
    val mongoClient = MongoClient()
    val collection = mongoClient("MathExchange")("examples")
    val exmpls = collection.find().foldLeft(List[Example]()){ (accum, dbObj) =>
      val time = dbObj.asInstanceOf[BasicDBObject].get("time").toString
      val annotation = dbObj.get("annotation").asInstanceOf[BasicDBList].toList.map(x => x.toString)
      val narrative = dbObj.get("narrative").asInstanceOf[BasicDBList].toList.map(x => x.toString)
      accum :+ new Example(annot = annotation, nar = narrative, _time = time)
    }
    val chunked = exmpls.sliding(chunkSize, chunkSize-1).toList
    chunked map { x =>
      val merged = x.foldLeft(Example()) { (z, y) =>
        new Example(annot = z.annotation ++ y.annotation, nar = z.narrative ++ y.narrative, _time = x.head.time)
      }
      merged
    }
  }

  val bottomTheory = HillClimbing.constructBottomTheory(data.toIterator, globals)

  println(bottomTheory.tostring)

  val iterations = 10

  val exploreRate = 1.0/Math.sqrt(2)

  val f1 = (t: Theory) => t.stats._6

  val rootNode = RootNode()

  generateAndScoreChildren(rootNode, bottomTheory, globals, data, 0)



  val bestNode = (1 to iterations).foldLeft(rootNode.asInstanceOf[TreeNode]) { (theorySearchedLast, iterCount) =>
    logger.info(s"Iteration $iterCount")
    val bestChild = rootNode.descendToBestChild(exploreRate)
    logger.info(s"Best leaf node selected (MCTS score: ${bestChild.getMCTSScore(exploreRate)} | id: ${bestChild.id}):\n${bestChild.theory.tostring}")
    val newNodes = generateAndScoreChildren(bestChild, bottomTheory, globals, data, iterCount)
    val bestChildNode = newNodes.maxBy( x => f1(x.theory) )

    bestChildNode.propagateReward(f1(bestChildNode.theory))

    if (theorySearchedLast.theory == Theory()) {
      logger.info(s"Best theory so far (F1-score ${f1(bestChildNode.theory)} | id: ${bestChildNode.id}):\n${bestChildNode.theory.tostring}")
      bestChildNode
    } else {
      if (f1(bestChildNode.theory) > f1(theorySearchedLast.theory)) {
        logger.info(s"Best theory so far (F1-score ${f1(bestChildNode.theory)} | id: ${bestChildNode.id}):\n${bestChildNode.theory.tostring}")
        bestChildNode//.theory
      } else {
        logger.info(s"Best theory so far (F1-score ${f1(theorySearchedLast.theory)} | id: ${theorySearchedLast.id}):\n${theorySearchedLast.theory.tostring}")
        theorySearchedLast
      }
    }
  }

  logger.info("Done")
  logger.info("Cross-validation...")
  val theory_ = Theory(bestNode.theory.clauses).compress
  crossVal(theory_, data.toIterator, "", globals) // generate new theory to clear the stats counter
  logger.info(s"F1-score on test set: ${theory_.stats._6}")



  def generateAndScoreChildren(fromNode: TreeNode, bottomTheory: Theory, gl: Globals, data: List[Example], iterationCount: Int) = {
    require(fromNode.isLeafNode())
    val newTheories = generateChildrenNodes(fromNode.theory, bottomTheory, data.toIterator, gl)
    scoreNodes(newTheories, gl, data)
    // The depth is used in the id generation of the children nodes.
    val depth = fromNode.getDepth() + 1
    val newNodes = newTheories.foldLeft(1, Vector[InnerNode]()) { (x, theory) =>
      val (newNodeCount, newNodes) = (x._1, x._2)
      val id = s"$iterationCount-$depth-$newNodeCount"
      val newNode = InnerNode(id, theory, fromNode)
      (newNodeCount + 1, newNodes :+ newNode)
    }._2
    newNodes foreach { node =>
      // Add each theory's f1-score to the corresponding node's rewards vector
      // and increment the node's visits counter.
      node.updateRewards(node.theory.stats._6)
      node.incrementVisits()
      // Finally, add the new node as a child to the parent node.
      fromNode.addChild(node)
    }

    /** FOR DEBUGGING ONLY */
    //println(newNodes.map(x => x.theory.tostring + " " + x.theory.stats._6).foreach(x => println(x+"\n")))

    newNodes
  }

  def scoreNodes(children: Vector[Theory], gl: Globals, data: List[Example]) = {
    logger.info("Scoring children nodes")
    children.foreach { childNode =>
      crossVal(childNode, data.toIterator, "", gl)
    }
    //children.foreach(x => println(x.tostring + " " + x.stats._6))
  }

}

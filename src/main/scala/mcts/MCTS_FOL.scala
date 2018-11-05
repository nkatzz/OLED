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

import app.runners.MLNDataHandler
import app.runners.MLNDataHandler.MLNDataOptions
import app.runutils.Globals
import com.typesafe.scalalogging.LazyLogging
import logic.Theory
import mcts.HillClimbing.{constructBottomTheory, generateChildrenNodes, getData, scoreNodes, crossVal}


/**
  * Created by nkatz on 9/19/17.
  */


object MCTS_FOL extends LazyLogging{

  /* TODO Need to implement the propagation to all theories that subsume the best child */

  def main(args: Array[String]) = {
    runCaviarMNL()
  }


  def runCaviarMNL() = {

    Globals.glvalues("perfect-fit") = "false"

    val foldPath = "/home/nkatz/dev/CAVIAR_MLN/CAVIAR_MLN/move/fold_2"

    val chunkSize = 50

    val opts = new MLNDataOptions(foldPath, chunkSize)

    val globals = new Globals("/home/nkatz/dev/iled/datasets/CaviarMLN/move", "")

    val bottomTheory = constructBottomTheory(getData(opts), globals)

    val iterations = 4

    val exploreRate = 1.0/Math.sqrt(2) //0.005 //

    val f1 = (t: Theory) => t.stats._6

    val rootNode = RootNode()

    // Generate the 1rst-level children.
    generateAndScoreChildren(rootNode, bottomTheory, globals, opts, 0)

    // Descent down the tree a few times and return the best
    // theory from the last iteration.
    val bestNode = (1 to iterations).foldLeft(rootNode.asInstanceOf[TreeNode]) { (theorySearchedLast, iterCount) =>
      logger.info(s"Iteration $iterCount")
      val bestChild = rootNode.descendToBestChild(exploreRate)
      logger.info(s"Best leaf node selected (MCTS score: ${bestChild.getMCTSScore(exploreRate)} | id: ${bestChild.id}):\n${bestChild.theory.tostring}")
      val newNodes = generateAndScoreChildren(bestChild, bottomTheory, globals, opts, iterCount)
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
    val testSet = MLNDataHandler.getTestingData(opts)
    val theory_ = Theory(bestNode.theory.clauses).compress
    crossVal(theory_, testSet, "", globals) // generate new theory to clear the stats counter
    logger.info(s"F1-score on test set: ${theory_.stats._6}")

  }

  def generateAndScoreChildren(fromNode: TreeNode, bottomTheory: Theory, gl: Globals, opts: MLNDataOptions, iterationCount: Int) = {
    require(fromNode.isLeafNode())
    val newTheories = generateChildrenNodes(fromNode.theory, bottomTheory, getData(opts), gl)
    scoreNodes(newTheories, gl, opts)
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








}

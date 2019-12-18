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

package oled.mwua.bandits

import logic.Clause

/**
  * Created by nkatz at 17/2/2019
  */

class PartialExpertTree(val bottomRule: Clause) {

  val rootNode = new RootNode(Clause(bottomRule.head))

}

trait TreeNode {
  var avgReward = 0.0
  var visits = 0
  var children: Vector[InnerNode] = Vector[InnerNode]()
  val rule = Clause() // the rule represented by this node
  val isRootNode: Boolean = false

  def addChild(x: InnerNode): Unit = children = children :+ x

  def isLeafNode() = this.children.isEmpty

  // Running mean reward
  def updateMeanReward(newReward: Double) = {
    avgReward = ((avgReward*visits) + newReward)/(visits+1)
    visits += 1
  }

  def getBestChild(exploreRate: Double): InnerNode = this.children.maxBy(x => x.getMCTSScore(exploreRate))

  /* Abstract methods */
  def getMCTSScore(exploreRate: Double): Double

  def getDepth(): Int

  def getAncestorsPath(): Vector[TreeNode]

  def propagateReward(reward: Double) = {
    val ancestors = getAncestorsPath()
    ancestors foreach { node =>
      node.updateMeanReward(reward)
    }
  }
}

class RootNode(override val rule: Clause) extends TreeNode {

  override val isRootNode = true
  this.visits = 1 // increment its visits upon generation.
  override def getMCTSScore(exploreRate: Double): Double = 0.0
  override def getDepth(): Int = 0
  override def getAncestorsPath() = Vector[TreeNode]()

  def descendToBestChild(exploreRate: Double) = {
    var reachedLeaf = false
    var bestChild = this.getBestChild(exploreRate)
    while(! reachedLeaf) {
      if (! bestChild.isLeafNode()) {
        bestChild = bestChild.getBestChild(exploreRate)
      } else {
        reachedLeaf = true
      }
    }
    bestChild
  }

}

class InnerNode(override val rule: Clause, val parentNode: TreeNode) extends TreeNode {

  override def getMCTSScore(exploreRate: Double) = {
    avgReward + exploreRate * Math.sqrt(2*Math.log(parentNode.visits)/visits)
  }


  override def getDepth() = {
    var reachedRoot = false
    var parent = this.parentNode
    var depth = 1
    while (!reachedRoot) {
      parent match {
        case _ : InnerNode =>
          depth = depth+1
          parent = parent.asInstanceOf[InnerNode].parentNode
        case _ : RootNode => reachedRoot = true
      }
    }
    depth
  }

  override def getAncestorsPath() = {
    var reachedRoot = false
    var parent = this.parentNode
    var ancestors = Vector[TreeNode]()
    while (!reachedRoot) {
      parent match {
        case _ : InnerNode =>
          ancestors = ancestors :+ parent
          parent = parent.asInstanceOf[InnerNode].parentNode
        case _ : RootNode =>
          ancestors = ancestors :+ parent
          reachedRoot = true
      }
    }
    ancestors
  }
}


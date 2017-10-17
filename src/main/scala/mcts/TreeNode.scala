package mcts

import logic.Theory

/**
  * Created by nkatz on 9/19/17.
  */

trait TreeNode {
  var visits = 0

  var rewards: Vector[Double] = Vector[Double]()

  var children: Vector[InnerNode] = Vector[InnerNode]()

  val theory: Theory = Theory()

  val id: String = ""

  def updateRewards(x: Double): Unit = this.rewards = this.rewards :+ x

  def updateVisits(v: Int): Unit = this.visits = v

  def incrementVisits(): Unit = this.visits = this.visits + 1

  def meanReward(): Double = this.rewards.sum / this.visits

  def addChild(x: InnerNode): Unit = this.children = this.children :+ x

  def getBestChild(exploreRate: Double): InnerNode = this.children.maxBy(x => x.getMCTSScore(exploreRate))

  def isLeafNode() = this.children.isEmpty

  val isRootNode: Boolean = false

  /* Abstract methods */

  def getMCTSScore(exploreRate: Double): Double

  def getDepth(): Int

  def getAncestorsPath(): Vector[TreeNode]

  def propagateReward(reward: Double) = {
    val ancestors = getAncestorsPath()
    ancestors foreach { node =>
      node.updateRewards(reward)
      node.incrementVisits()
    }
  }
}

/**
  *
  * The id of a node is a string of the form 2-3-12, where 2 is the iteration number,
  * 3 is the "depth" of the node and 12 is a counter of the the other nodes generated at this round.
  *
  * */


case class RootNode(override val id: String, override val theory: Theory) extends TreeNode {

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

object RootNode {

  def apply() = {
    new RootNode("0", Theory())
  }

}

case class InnerNode(override val id: String, override val theory: Theory, parentNode: TreeNode) extends TreeNode {

  override val isRootNode = false

  override def getMCTSScore(exploreRate: Double) = meanReward() + exploreRate * Math.sqrt(2*Math.log(this.parentNode.visits)/this.visits)

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
    var ancestors = Vector[TreeNode]()//Vector(parent)
    while (!reachedRoot) {
      parent match {
        case _ : InnerNode =>
          ancestors = ancestors :+ parent
          parent = parent.asInstanceOf[InnerNode].parentNode
        case _ : RootNode =>
          ancestors = ancestors :+ parent
          reachedRoot = true
      }
      //ancestors = ancestors :+ parent
    }
    ancestors
  }

  /*
  def propagateReward(reward: Double) = {
    val ancestors = getAncestorsPath()
    ancestors foreach { node =>
      node.updateRewards(reward)
      node.incrementVisits()
    }
  }
  */

}





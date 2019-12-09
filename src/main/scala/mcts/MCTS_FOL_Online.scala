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

import java.io.File
import java.util.UUID

import app.runners.MLNDataHandler
import app.runners.MLNDataHandler.MLNDataOptions
import app.runutils.Globals
import com.typesafe.scalalogging.LazyLogging
import iled.ILED
import logic.Examples.Example
import logic.{Literal, PriorTheory, Rules, Theory}
import mcts.HillClimbing.{getData, crossVal}
import utils.{ASP, Utils}
import xhail.Xhail


/**
  * Created by nkatz on 9/19/17.
  */


object MCTS_FOL_Online extends LazyLogging{

  /* TODO Need to implement the propagation to all theories that subsume the best child */

  def main(args: Array[String]) = {
    runCaviarMNL()
  }


  def runCaviarMNL() = {

    /*-----------------------------------------------*/
    Globals.glvalues("perfect-fit") = "false"
    Globals.glvalues("smallest-nonempty") = "true"
    /*-----------------------------------------------*/

    val foldPath = "/home/nkatz/dev/CAVIAR_MLN/CAVIAR_MLN/move/fold_2"
    val chunkSize = 500
    val opts = new MLNDataOptions(foldPath, chunkSize)
    val globals = new Globals("/home/nkatz/dev/iled/datasets/CaviarMLN/move")

    val exploreRate = 0.005 //1.0/Math.sqrt(2) //

    val rootNode = RootNode()




    val repeatFor = 1


    // Generate the 1rst-level child.
    //generateAndScoreChildren(rootNode, trainingData.next(), jep, globals, opts, 0)
    var exmplsCount = 1

    for (x <- 1 to repeatFor) {
      val trainingData = getData(opts)
      while(trainingData.hasNext) {
        val exmpl = trainingData.next()

        // First, generate a new kernel set from the new example and a new theory from it,
        // without assuming any prior theory T. If there is no equivalent theory in the first
        // level of children of the root node, add T as a node there (so, expand the tree horizontally)
        val newLevelOneNode = generateChildNode(rootNode.theory, exmpl, globals)
        if (newLevelOneNode != Theory()) {
          if (rootNode.children.forall(p => !(p.theory.thetaSubsumes(newLevelOneNode) && newLevelOneNode.thetaSubsumes(p.theory)) )) {
            logger.info("Added new child at level 1 (tree expanded horizontally).")
            val score = scoreNode(newLevelOneNode, exmpl, globals, opts)
            val n = InnerNode("new-level-1-node", newLevelOneNode, rootNode)
            n.updateRewards(score)
            n.incrementVisits()
            rootNode.addChild(n)
            n.propagateReward(score)
          }
        }

        if (rootNode.children.nonEmpty) {
          val bestChild = rootNode.descendToBestChild(exploreRate)
          //logger.info(s"Best leaf node selected (MCTS score: ${bestChild.getMCTSScore(exploreRate)} | id: ${bestChild.id}):\n${bestChild.theory.tostring}")
          val newNode = generateAndScoreChildren(bestChild, exmpl, globals, opts, exmplsCount)

          if (newNode != bestChild) {
            logger.info(s"\nLeaf node: (MCTS score: ${bestChild.getMCTSScore(exploreRate)} |" +
              s" mean f1-score: ${bestChild.theory.fscore} | visits: ${bestChild.visits} | id: ${bestChild.id}):\n${bestChild.theory.tostring}\nwas expanded " +
              s"to (MCTS score: ${newNode.getMCTSScore(exploreRate)} |" +
              s" mean f1-score: ${newNode.theory.fscore} | visits: ${newNode.visits} | id: ${newNode.id}):\n${newNode.theory.tostring}\n")
          }


        }

        exmplsCount += 1
      }

    }



    var finalTheory = rootNode.descendToBestChild(exploreRate).theory

    if (finalTheory == Theory()) finalTheory = rootNode.descendToBestChild(exploreRate).parentNode.theory

    logger.info(s"Final theory:\n${finalTheory.tostring}")

    logger.info("Done")
    logger.info("Cross-validation...")
    val testSet = MLNDataHandler.getTestingData(opts)
    val theory_ = Theory(finalTheory.clauses).compress // generate new theory to clear the stats counter
    crossVal(theory_, testSet, "", globals)
    logger.info(s"F1-score on test set: ${theory_.stats._6} | (tps, fps, fns) = (${theory_.stats._1}, ${theory_.stats._2}, ${theory_.stats._3})")

  }

  def generateAndScoreChildren(fromNode: TreeNode, exmpl: Example, gl: Globals, opts: MLNDataOptions, iterationCount: Int) = {
    require(fromNode.isLeafNode())
    val childTheory = generateChildNode(fromNode.theory, exmpl, gl)
    // The depth is used in the id generation of the children nodes.
    val depth = fromNode.getDepth() + 1
    if (childTheory != Theory()) {

      val score1 = scoreNode(childTheory, exmpl, gl, opts)
      val score2 = scoreNode(fromNode.theory, exmpl, gl, opts)

      if (score1 > score2) {
        val id = s"$iterationCount-$depth-1"
        val newNode = InnerNode(id, childTheory, fromNode)
        val score = score1 - score2
        newNode.updateRewards(score)
        newNode.incrementVisits()
        fromNode.addChild(newNode)
        newNode.propagateReward(score)
        newNode
      } else {
        scoreAreReturnNode(fromNode, exmpl, gl, opts)
      }
    } else {
      scoreAreReturnNode(fromNode, exmpl, gl, opts)
    }

  }

  def scoreAreReturnNode(node: TreeNode, exmpl: Example, gl: Globals, opts: MLNDataOptions) = {
    val score = scoreNode(node.theory, exmpl, gl, opts)
    node.updateRewards(score)
    node.incrementVisits()
    node.propagateReward(score)
    node
  }

  def scoreNode(node: Theory, exmpl: Example, gl: Globals, opts: MLNDataOptions) = {
    //logger.info("Scoring child node")
    evaluateTheory(node, exmpl, "", gl)
    //evaluateTheory(parentNode, exmpl, jep, "", gl)
    val score = node.fscore
    score
  }

  def evaluateTheory(theory: Theory, e: Example, handCraftedTheoryFile: String = "", globals: Globals): Unit = {

    val varbedExmplPatterns = globals.EXAMPLE_PATTERNS_AS_STRINGS
    val coverageConstr = s"${globals.TPS_RULES}\n${globals.FPS_RULES}\n${globals.FNS_RULES}"
    val t =
      if(theory != Theory()) {
        theory.clauses.map(x => x.withTypePreds(globals).tostring).mkString("\n")
      } else {
        globals.INCLUDE_BK(handCraftedTheoryFile)
      }
    val show = globals.SHOW_TPS_ARITY_1 + globals.SHOW_FPS_ARITY_1 + globals.SHOW_FNS_ARITY_1
    val ex = e.tostring
    val program = ex + globals.INCLUDE_BK(globals.BK_CROSSVAL) + t + coverageConstr + show
    val f = Utils.getTempFile(s"eval-theory-${UUID.randomUUID().toString}-${System.currentTimeMillis()}", ".lp")
    Utils.writeLine(program, f.getCanonicalPath, "overwrite")
    val answerSet = ASP.solve(task = Globals.INFERENCE, aspInputFile = f)
    if (answerSet.nonEmpty) {
      val atoms = answerSet.head.atoms
      atoms.foreach { a=>
        val lit = Literal.parse(a)
        //val inner = lit.terms.head
        lit.predSymbol match {
          case "tps" => theory._tps += 1
          case "fps" => theory._fps += 1
          case "fns" => theory._fns += 1
        }
      }
    }
  }


  def generateChildNode(currentNode: Theory, currentExample: Example, gl: Globals) = {
    //logger.info("Generating children nodes")
    //println(s"Generating children at example ${currentExample.time}")
    val isSat = ILED.isSAT(currentNode, currentExample, ASP.check_SAT_Program, gl)
    if (isSat) {
      Theory()
    } else {
      val interpretation = currentExample.annotationASP ++ currentExample.narrativeASP
      val infile = Utils.getTempFile("example", ".lp")
      Utils.writeToFile(infile, "overwrite") { p => interpretation.foreach(p.println) }
      val bk = gl.BK_WHOLE_EC
      val (_, varKernel) = Xhail.runXhail(fromFile=infile.getAbsolutePath, kernelSetOnly=true, bkFile=bk, globals=gl)
      val aspFile: File = utils.Utils.getTempFile("aspInduction", ".lp")
      val (_, use2AtomsMap, defeasible, use3AtomsMap, _, _) =
        ASP.inductionASPProgram(kernelSet = Theory(varKernel),
          priorTheory = currentNode, examples = currentExample.toMapASP, aspInputFile = aspFile, globals = gl)
      val answerSet = ASP.solve("iled", use2AtomsMap ++ use3AtomsMap, aspFile, currentExample.toMapASP)
      if (answerSet != Nil) {
        val newRules = Rules.getNewRules(answerSet.head.atoms, use2AtomsMap)
        ILED.updateSupport(newRules, Theory(varKernel))
        val icRules = Rules.getInconsistentRules(answerSet.head.atoms, currentNode, use3AtomsMap)
        val retainedRules = Theory(currentNode.clauses.filter(x => icRules.forall(y => y.rule != x)))
        //iled.ILED.updateSupport(retainedRules, bottomTheory) //no need for that, each rule has one rule in its support, its bottom clause, this doesn't change
        val refinedRules = icRules.map(x => x.initialRefinement)
        val newTheory = new PriorTheory(retainedRules, newRules, Theory(refinedRules)).merge
        newTheory
        /*
        if (theories.exists(theory => theory.thetaSubsumes(newTheory) && newTheory.thetaSubsumes(theory))) theories else theories :+ newTheory
        } else {
          theories
        }
        */
      } else {
       Theory()
      }
    }

  }








}

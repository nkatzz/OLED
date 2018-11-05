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

package lomcts

import java.io.File
import java.util.UUID
import akka.actor.Actor
import app.runutils.{Globals, RunningOptions}
import app.runutils.IOHandling.Source
import com.typesafe.scalalogging.LazyLogging
import iled.ILED
import logic.Examples.Example
import logic.{Literal, PriorTheory, Rules, Theory}
import mcts.{InnerNode, RootNode, TreeNode}
import utils.{ASP, Utils}
import xhail.Xhail


class LoMCTS[T <: Source](inps: RunningOptions,
                          trainingDataOptions: T,
                          testingDataOptions: T,
                          trainingDataFunction: T => Iterator[Example],
                          testingDataFunction: T => Iterator[Example]) extends Actor with LazyLogging {

  private var trainingData = trainingDataFunction(trainingDataOptions)

  var continue = true

  val upto = 10

  val scoreThemFor = 80

  var bestTheorySoFar = Theory()

  val globals = inps.globals

  val exploreRate = 0.005 //1.0/Math.sqrt(2) //
  
  val rootNode = RootNode()

  def receive = {
    case "start" => run()
  }

  def run() = {

    var iterationCount = 0

    while (iterationCount < 2000) {

      logger.info(s"Iteration: $iterationCount")

      val exmpl = getNextBatch()

      // First, generate a new kernel set from the new example and a new theory from it,
      // without assuming any prior theory T. If there is no equivalent theory in the first
      // level of children of the root node, add T as a node there (so, expand the tree horizontally)
      //---------------------------------------------------------------------------------------------
      // NOTE: Perhaps a strategy worth exploring is to randomly select a (random) number of existing
      // bottom clauses and include them in the new theory generated from the new example.
      //---------------------------------------------------------------------------------------------
      val newLevelOneNode = generateChildNode(rootNode.theory, exmpl, globals)
      if (newLevelOneNode != Theory()) {
        if (rootNode.children.forall(p =>
          !(p.theory.thetaSubsumes(newLevelOneNode) && newLevelOneNode.thetaSubsumes(p.theory)) )) {
          logger.info("Added new child at level 1 (tree expanded horizontally).")
          val score = scoreNode(newLevelOneNode, exmpl, globals)
          val n = InnerNode("new-level-1-node", newLevelOneNode, rootNode)
          n.updateRewards(score)
          n.incrementVisits()
          rootNode.addChild(n)
          n.propagateReward(score)
        }
      }

      // Continue to an MCTS round
      if (rootNode.children.nonEmpty) {
        val bestChild = rootNode.descendToBestChild(exploreRate)

        val newNode = generateAndScoreChildren(bestChild, exmpl, globals, iterationCount)

        if (newNode != bestChild) {
          logger.info(s"\nLeaf node: (MCTS score: ${bestChild.getMCTSScore(exploreRate)} |" +
            s" mean f1-score: ${bestChild.theory.fscore} | visits: ${bestChild.visits} | id: ${bestChild.id}):\n${bestChild.theory.tostring}\nwas expanded " +
            s"to (MCTS score: ${newNode.getMCTSScore(exploreRate)} |" +
            s" mean f1-score: ${newNode.theory.fscore} | visits: ${newNode.visits} | id: ${newNode.id}):\n${newNode.theory.tostring}\n")
          bestTheorySoFar = bestChild.theory
        } else {
          logger.info("Failed to find a better revision")
        }

      }
      iterationCount += 1
    }

    var finalTheory = bestTheorySoFar

    logger.info(s"Final theory:\n${finalTheory.tostring}")

    logger.info("Done")
    logger.info("Cross-validation...")
    val testSet = testingDataFunction(testingDataOptions)
    val theory_ = Theory(finalTheory.clauses).compress // generate new theory to clear the stats counter
    crossVal(theory_, testSet, "", globals)

    val x = theory_.precision

    logger.info(s"F1-score on test set: ${theory_.fscore} | (tps, fps, fns) = (${theory_._tps}, ${theory_._fps}, ${theory_._fns})")

  }

  def crossVal(t: Theory, data: Iterator[Example], handCraftedTheoryFile: String = "", globals: Globals) = {
    while (data.hasNext) {
      val e = data.next()
      evaluateTheory(t, e, handCraftedTheoryFile, globals)
    }
    //val stats = t.stats
    //(stats._1, stats._2, stats._3, stats._4, stats._5, stats._6)
  }

  def generateAndScoreChildren(fromNode: TreeNode, exmpl: Example, gl: Globals, iterationCount: Int) = {

    require(fromNode.isLeafNode())

    // This generates minimal revisions from the current leaf node using a fixed number of interpretations
    // to guide the revision generation process. I should also try other strategies, such as e.g. generating
    // all revisions (including non-minimal ones) from a single example; or using additionally other revisions
    // from other examples.
    val candidateChildren = (1 to upto).foldLeft(Vector[Theory]()) { (theories, _) =>
      val newExmpl = getNextBatch()
      val newTheory = generateChildNode(fromNode.theory, newExmpl, gl)
      // This is too fucking expensive, something must be done.
      val isNew = theories.forall(p => !p.thetaSubsumes(newTheory) && ! newTheory.thetaSubsumes(p))
      if (isNew) {
        // score it on this example, don't waste it
        scoreNode(newTheory, newExmpl, gl)
        theories :+ newTheory
      } else {
        theories
      }
    }

    logger.info(s"Generated ${candidateChildren.size} candidate revisions")

    if (candidateChildren.nonEmpty) {
      // Next we need to use a Hoeffding test to identify the best of these theories.

      val candidates = Vector(fromNode.theory) ++ candidateChildren

      var hoeffdingSucceeds = false

      // score all on the initial example, don't waste it
      candidates.foreach(p => scoreNode(p, exmpl, gl))

      /*
      while (!hoeffdingSucceeds) {

      }
      */

      // for now I'll do this stupid thing where you score them on a fixed number of interpretations
      // and get the best theory. There are several complications to implement an actual Hoeffding
      // test (you'll see them if you follow the code), which I need to address.
      logger.info("scoring candidates")
      for (1 <- 0 to scoreThemFor) {
        val e = getNextBatch()
        candidates.foreach(p => scoreNode(p, e, gl))
      }

      val sorted = candidates.sortBy(x => - x.fscore)
      val best = if (sorted.head.fscore > fromNode.theory.fscore) {
        // The depth is used in the id generation of the children nodes.
        val depth = fromNode.getDepth() + 1
        val id = s"$iterationCount-$depth-1"
        val newNode = InnerNode(id, sorted.head, fromNode)
        //val score = score1 - score2
        newNode.updateRewards(sorted.head.fscore)
        newNode.incrementVisits()
        fromNode.addChild(newNode)
        newNode.propagateReward(sorted.head.fscore)
        newNode
      } else {
        scoreAreReturnNode(fromNode, exmpl, gl)
      }
      best
    } else {
      scoreAreReturnNode(fromNode, exmpl, gl)
    }
  }

  def scoreAreReturnNode(node: TreeNode, exmpl: Example, gl: Globals) = {
    //val score = scoreNode(node.theory, exmpl, jep, gl)
    node.updateRewards(node.theory.fscore)
    node.incrementVisits()
    node.propagateReward(node.theory.fscore)
    node
  }

  def scoreNode(node: Theory, exmpl: Example, gl: Globals) = {
    evaluateTheory(node, exmpl, "", gl)
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
        lit.functor match {
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

  def getNextBatch() = {
    if (trainingData.nonEmpty) trainingData.next()
    else {
      trainingData = trainingDataFunction(trainingDataOptions)
      trainingData.next()
    }
  }



}

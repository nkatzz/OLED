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

package oled.weightlearn.parallel

import akka.actor.{Actor, ActorRef, Props}
import app.runutils.IOHandling.Source
import app.runutils.{Globals, RunningOptions}
import logic.Examples.Example
import logic.{Clause, Constant, Literal, Theory}
import oled.single_core.TheoryLearner
import org.slf4j.LoggerFactory
import utils.{ASP, Utils}
import oled.functions.SingleCoreOLEDFunctions._
import utils.Implicits._
import oled.functions.WeightLearningFunctions._
import oled.weightlearn.{Auxil, MAPInference}
import oled.weightlearn.parallel.IO.{FinishedBatch, MLNClauseHandingMasterInput, MLNClauseHandlingOutput, NodeDoneMessage, TheoryRequestMessage}

/* This class is used when learning weights while allowing for parallel clause evaluation.
 * The task requires collaboration with MLNClauseEvalMaster and uses blocking (we need to
 * wait until the evaluator class finishes its job before moving on to the next mini-batch).
 * For this reason, the functionality here differs significantly from the regular one of
 * oled.weightlearn.WeightedTheoryLearner (where all clauses are evaluated in one go).
 *
 * */

class WeightedTheoryLearner[T <: Source](inps: RunningOptions, trainingDataOptions: T,
                                         testingDataOptions: T, trainingDataFunction: T => Iterator[Example],
                                         testingDataFunction: T => Iterator[Example],
                                         targetClass: String) extends TheoryLearner(inps,
  trainingDataOptions, testingDataOptions, trainingDataFunction, testingDataFunction, targetClass) {

  import context.become

  private val startTime = System.nanoTime()
  private val logger = LoggerFactory.getLogger(self.path.name)
  private var repeatFor = inps.repeatFor
  private var data = Iterator[Example]()
  private var topTheory = Theory()

  private var workers: Vector[ActorRef] = Vector.empty[ActorRef]

  private val master: ActorRef = context.actorOf(
    Props( new MLNClauseEvalMaster(inps, initorterm) ), name = s"${this.initorterm}-master")

  override def receive = {
    case "go" =>
      workers = getWorkersPool
      start
  }

  def getTrainData = trainingDataFunction(trainingDataOptions)

  def getNextBatch = if (data.isEmpty) Example() else data.next()

  def start = {
    this.repeatFor -= 1
    data = getTrainData
    if (data.isEmpty) { logger.error(s"No data received.") ; System.exit(-1) }
    become(normalState)
    self ! getNextBatch
  }

  def normalState: Receive = {

    case exmpl: Example =>
      if (exmpl == Example()) {
        logger.info(s"Finished the data")
        if (repeatFor > 0) {
          self ! "start-over"
        } else if (repeatFor == 0) {
          val endTime = System.nanoTime()
          val totalTime = (endTime - startTime)/1000000000.0

          val theory = if (topTheory.clauses.nonEmpty) topTheory.clauses else inps.globals.state.getAllRules(inps.globals, "top")

          // used for printing out the avegare loss vector
          def avgLoss(in: Vector[Int]) = {
            in.foldLeft(0, 0, Vector.empty[Double]){ (x, y) =>
              val (count, prevSum, avgVector) = (x._1, x._2, x._3)
              val (newCount, newSum) = (count + 1, prevSum + y)
              (newCount, newSum, avgVector :+ newSum.toDouble/newCount )
            }
          }

          logger.info(s"\nTheory:\n${Theory(theory).showWithStats}\nTraining time: $totalTime")
          logger.info(s"Mistakes per batch:\n${inps.globals.state.perBatchError}")
          logger.info(s"Accumulated mistakes per batch:\n${inps.globals.state.perBatchError.scanLeft(0.0)(_ + _).tail}")
          logger.info(s"Average loss vector:\n${avgLoss(inps.globals.state.perBatchError)}")
          logger.info(s"Sending the theory to the parent actor")
          context.parent ! (topTheory, totalTime)
        } else {
          throw new RuntimeException("This should never have happened (repeatFor is now negative?)")
        }
      } else {
        become(processingState)
        self ! exmpl
      }

    case _: FinishedBatch => self ! getNextBatch

    case _: TheoryRequestMessage => sender ! topTheory

    case "start-over" =>
      logger.info(s"Starting a new training iteration (${this.repeatFor - 1} iterations remaining.)")
      start
  }

  def processingState: Receive = {

    case e: Example =>

      // All the work takes place here
      var newTopTheory = topTheory

      val useToPredict =
        if (newTopTheory.clauses.nonEmpty) {

          newTopTheory.clauses.toVector foreach
            (rule => if (rule.refinements.isEmpty) rule.generateCandidateRefs(inps.globals))

          newTopTheory.clauses.map { topClause =>
            val bestRef = topClause.refinements.sortBy(x => - x.mlnWeight).head
            if (topClause.mlnWeight > bestRef.mlnWeight) topClause else bestRef

          }
        } else newTopTheory.clauses


      //val error = predictSate(useToPredict, e, inps, initorterm, jep)
      //Globals.errorProb = Globals.errorProb :+ error

      val (_, startNewRulesTime) = Utils.time {
        val startNew =
        //if (this.inps.tryMoreRules && (this.targetClass == "terminated" ||this.targetClass == "initiated" )) true
        if (this.inps.tryMoreRules && targetClass == "terminated"){
          true
        } else {
          //newTopTheory.growNewRuleTest(e, jep, initorterm, inps.globals)
          Theory(useToPredict).growNewRuleTest(e, initorterm, inps.globals)
        }
        //*/
        if (startNew) {
          val newRules_ = if (inps.tryMoreRules) {
            // Don't use the current theory here to force the system to generate new rules
            generateNewRules(Theory(), e, initorterm, inps.globals)
          } else {
            //generateNewRules(topTheory, e, jep, initorterm, inps.globals)
            generateNewRules(Theory(useToPredict), e, initorterm, inps.globals)
          }
          // Just to be on the safe side...
          val newRules = newRules_.toVector.filter(x => x.head.functor == initorterm)
          if (newRules.nonEmpty) {
            logger.info(s"Generated ${newRules.length} new rules.")
            if (topTheory.clauses.nonEmpty) {
              //val largestWeight = topTheory.clauses.sortBy(x => -x.mlnWeight).head.mlnWeight
              //newRules.foreach(x => x.mlnWeight = largestWeight)
              //newRules.foreach(x => x.mlnWeight = 1.0)
            }
          }
          if (inps.compressNewRules) newTopTheory = topTheory.clauses ++ filterTriedRules(topTheory, newRules, logger)
          else newTopTheory = topTheory.clauses ++ newRules
        }
      }

      if (newTopTheory.clauses.nonEmpty) {

        val generate_refs_timed = Utils.time {
          newTopTheory.clauses.toVector foreach
            (rule => if (rule.refinements.isEmpty) rule.generateCandidateRefs(inps.globals))
        }

        val allClauses = newTopTheory.clauses.flatMap { x =>
          if (x.body.nonEmpty) List(x) ++ x.refinements
          else x.refinements
        }

        val enumClauses = (1 to allClauses.length).toList
        val uuidsToRuleIdsMap = (allClauses.map(_.uuid) zip enumClauses).toMap

        val input = new MLNClauseHandingMasterInput(allClauses.toVector, e, inps, initorterm, splitEvery = 3, workers)

        //val error = predictSate(useToPredict, e, inps, initorterm, jep)
        //Globals.errorProb = Globals.errorProb :+ error

        become( waitingState(newTopTheory, uuidsToRuleIdsMap) )
        master ! input

      } else {
        topTheory = newTopTheory
        become(normalState)
        self ! new FinishedBatch
      }

    case _ => throw new RuntimeException("This shouldn't have happened.") // just to be on the safe side...

  }

  def waitingState(newTopTheory: Theory, uuidsToRuleIdsMap: Map[String, Int]): Receive = {

    case result: MLNClauseHandlingOutput =>

      val exmplCount = result.totalExampleCount

      Utils.time {
        newTopTheory.clauses foreach { clause =>

          if (clause.body.nonEmpty) Auxil.updateClauseStats(clause,
            uuidsToRuleIdsMap, result.inferredTrue, result.actuallyTrue, result.incorrectlyTerminated,
            result.correctlyNotTerminated, result.clausesWithUpdatedWeights, this.initorterm)

          clause.refinements.foreach(ref =>
            Auxil.updateClauseStats(ref, uuidsToRuleIdsMap, result.inferredTrue,
              result.actuallyTrue, result.incorrectlyTerminated, result.correctlyNotTerminated,
              result.clausesWithUpdatedWeights, this.initorterm))

          clause.seenExmplsNum += exmplCount
          clause.refinements.toVector.foreach(y => y.seenExmplsNum += exmplCount)
          clause.supportSet.clauses.toVector.foreach(y => y.seenExmplsNum += exmplCount)
        }
      }

      val (expanded, expTimed) = Utils.time { expandRules(newTopTheory, inps, logger) }

      if (inps.onlinePruning) topTheory = pruneRules(expanded._1, inps, logger)
      else topTheory = expanded._1

      become(normalState)
      self ! new FinishedBatch

    case _ => throw new RuntimeException("This shouldn't have happened.") // just to be on the safe side...

  }

  def getWorkersPool = {
    ///*
    val  cores = Runtime.getRuntime.availableProcessors()
    val workerNames = (1 to cores).toList map (i => s"Worker-${this.initorterm}-$i")
    workerNames map { name => context.actorOf(Props( new MLNClauseEvalWorker ), name = name) } toVector
    //*/
    //Vector(context.actorOf(Props( new MLNClauseEvalWorker ), name = "worker"))
  }



  /* Perform MAP inference to detect whether a new rule should be added.
   * This is half-finished and is currently not used anywhere. */
  def predictSate(useToPredict: List[Clause], e: Example, inps: RunningOptions, targetClass: String) = {

    val clauses = useToPredict

    val ((groundNetwork, trueGroundingsMap, totalExmplCount, annotationMLN, incorrectlyTerminated, correctlyNotTerminated), groundingTime) = {
      val timed = Utils.time{ getGroundTheory(clauses.toVector, e, inps, targetClass) }
      (timed._1, timed._2)
    }

    // Perform inference
    val solver = new MAPInference
    val (inferredGroundNetwork, mapInferenceTime): (Vector[Literal], Double) = {
      val timed = Utils.time { solver.infer(groundNetwork, clauses.toVector) }
      (timed._1, timed._2)
    }

    val inferredTrue = inferredGroundNetwork.filter(x => x.mlnTruthValue)
    val actuallyTrue = annotationMLN

    val _inferredTrue = inferredTrue.map(x => x.tostring_mln).toSet
    val _actuallyTrue = actuallyTrue.map(x => x.tostring_mln).toSet

    val tps = _inferredTrue.intersect(_actuallyTrue).size
    val fps = _inferredTrue.diff(_actuallyTrue).size

    val error = Math.abs(_actuallyTrue.size - (tps+fps))

    error
    //Math.abs(_inferredTrue.size - _actuallyTrue.size)

    //(inferredTrue, actuallyTrue, incorrectlyTerminated, correctlyNotTerminated, clauses, totalExmplCount)

  }




}

package oled.weightlearn

import java.io.{BufferedWriter, File, FileWriter, PrintWriter}

import akka.actor.{ActorRef, Props}
import akka.event.Logging.Debug
import app.runutils.IOHandling.Source
import app.runutils.RunningOptions
import logic.Examples.Example
import logic.{Clause, Constant, Literal, Theory}
import oled.functions.SingleCoreOLEDFunctions.reScoreAndPrune
import oled.single_core.TheoryLearner
import org.slf4j.LoggerFactory
import utils.{ASP, Utils}
import oled.functions.SingleCoreOLEDFunctions._
import oled.functions.WeightLearningFunctions
import utils.Implicits._
import oled.functions.WeightLearningFunctions._
import oled.weightlearn.parallel.IO.{MLNClauseHandingMasterInput, MLNClauseHandlingOutput}
import oled.weightlearn.parallel.{MLNClauseEvalMaster, MLNClauseEvalWorker}
import akka.pattern.ask
import akka.util.Timeout


import scala.concurrent.{Await, ExecutionContext, Future}
import scala.concurrent.duration._


class WeightedTheoryLearner[T <: Source](inps: RunningOptions, trainingDataOptions: T,
                                         testingDataOptions: T, trainingDataFunction: T => Iterator[Example],
                                         testingDataFunction: T => Iterator[Example],
                                         targetClass: String) extends TheoryLearner(inps,
  trainingDataOptions, testingDataOptions, trainingDataFunction, testingDataFunction, targetClass) {

  import context.become

  private val logger = LoggerFactory.getLogger(self.path.name)

  def parallelClauseEvaluationAwait(test: String) : Receive = {
    case "test" => true
  }

  override def run: (Theory, Double) = {

    def runOnce(inTheory: Theory): Theory = {
      val trainingData = trainingDataFunction(trainingDataOptions)
      if (trainingData.isEmpty) {
        logger.error(s"DB ${inps.db} is empty.")
        System.exit(-1)
      }

      trainingData.foldLeft(inTheory){ (topTheory, newExample) =>
        println(newExample.time)
        val res = Utils.time {
          processExample_1(topTheory, newExample)
          //processExample(topTheory, newExample)
        }
        println(s"Process time: ${res._2}")
        res._1

      }
    }

    logger.info(s"Starting learning for $targetClass")


    //val allClauses = List( Clause.parse("initiatedAt(meeting(X0,X1),X2) :- happensAt(active(X0),X2),happensAt(active(X1),X2),close(X0,X1,25,X2)"),
    //  Clause.parse("initiatedAt(meeting(X0,X1),X2) :- close(X1,X0,24,X2)"))

    /*
    val allClauses = List( Clause.parse("initiatedAt(meeting(X0,X1),X2) :- close(X1,X0,24,X2),happensAt(walking(X0),X2),happensAt(inactive(X1),X2)"),
      Clause.parse("initiatedAt(meeting(X0,X1),X2) :- happensAt(active(X0),X2)," +
        "happensAt(active(X1),X2)"), Clause.parse("initiatedAt(meeting(X0,X1),X2) :- close(X0,X1,24,X2),happensAt(inactive(X0),X2),happensAt(walking(X1),X2)"))
    */
    /*
    val allClauses = List(Clause.parse("terminatedAt(meeting(X0,X1),X2) :- happensAt(walking(X0),X2),happensAt(walking(X1),X2)"),
      Clause.parse("terminatedAt(meeting(X0,X1),X2) :- not close(X1,X0,24,X2)"))
    */

    //val allClauses = List(Clause.parse("terminatedAt(meeting(X0,X1),X2) :- happensAt(walking(X0),X2),happensAt(walking(X1),X2)"))

    val allClauses = List( Clause.parse("terminatedAt(moving(X0,X1),X2) :- happensAt(active(X1),X2),happensAt(active(X0),X2)"),
      Clause.parse("terminatedAt(moving(X0,X1),X2) :- close(X1,X0,25,X2),happensAt(walking(X1),X2)"),
      Clause.parse("terminatedAt(moving(X0,X1),X2) :- close(X0,X1,25,X2),happensAt(walking(X0),X2)"),
      Clause.parse("terminatedAt(moving(X0,X1),X2) :- not orientationMove(X0,X1,X2)") )

    //val allClauses = List( Clause.parse("initiatedAt(meeting(X0,X1),X2) :- happensAt(active(X0),X2),happensAt(active(X1),X2),close(X0,X1,25,X2)") )
    //val allClauses = List( Clause.parse("terminatedAt(meeting(X0,X1),X2) :- happensAt(disappear(X1),X2)") )
    //val allClauses = List( Clause.parse("terminatedAt(meeting(X0,X1),X2) :- happensAt(active(X0),X2),happensAt(active(X1),X2),close(X0,X1,24,X2)") )
    val inTheory = Theory(allClauses)
    val _finalTheory = Utils.time{ (1 to inps.repeatFor).foldLeft(inTheory)( (t,_) =>  runOnce(t)) }

    //val _finalTheory = Utils.time{ (1 to inps.repeatFor).foldLeft(Theory())( (t,_) =>  runOnce(t)) }

    val (finalTheory,time) = (_finalTheory._1,_finalTheory._2)
    logger.info(s"\nTraining time for $targetClass: $time")

    /* We'll have to see if all that stuff with post-pruning will be ported in the MLN-based version*/
    val output = inps.withPostPruning match {
      case true =>
        val data = trainingDataFunction(trainingDataOptions)
        reScoreAndPrune(inps, data, finalTheory, initorterm, logger)
      case _ =>
        // no re-scoring
        //val pruned = finalTheory.clauses.filter(x => x.score > inps.pruneThreshold && x.seenExmplsNum > inps.minEvalOn )

        //val pruned = finalTheory.clauses.filter(x => x.mlnWeight > inps.mlnWeightThreshold && x.seenExmplsNum > inps.minEvalOn )

        val pruned = finalTheory.clauses.filter( x => x.mlnWeight > 0.0 && x.seenExmplsNum > 2000 )

        logger.info(s"\nTheory (non-pruned)\n${finalTheory.showWithStats}")

        val msgLength = "Learnt hypothesis (pruned | --mln-weight-at-least:  |  --eval-atleast-on:   examples".length
        val sep = "-" * (msgLength+15)

        logger.info(s"\n$sep\nLearnt hypothesis (pruned | --mln-weight-at-least: ${inps.mlnWeightThreshold} |" +
          s" --eval-atleast-on: ${inps.minEvalOn} examples):\n$sep${if(pruned.clauses.nonEmpty) "\n"+pruned.showWithStats else "Empty"}")
        Theory(pruned)
    }

    logger.debug(s"\n$targetClass theory found:\n${output.tostring}")
    (output,time)
  }





  override def processExample(topTheory: Theory, e: Example): Theory = {

    if (e.time == "6344") {
      val stop = "stop"
    }

    println("=============================")
    println(e.time)
    println("=============================")

    var newTopTheory = topTheory

    val (_, startNewRulesTime) = Utils.time{
      // WE NEED TO HANDLE THE startNew VARIABLE AS WELL...
      ///*
      val startNew =
      //if (this.inps.tryMoreRules && (this.targetClass == "terminated" ||this.targetClass == "initiated" )) true
      if (this.inps.tryMoreRules && this.targetClass == "terminated") true
      else newTopTheory.growNewRuleTest(e, initorterm, inps.globals)
      //*/

      if (startNew) {
        val newRules_ = if (this.inps.tryMoreRules) {
          // Don't use the current theory here to force the system to generate new rules
          generateNewRules(Theory(), e, this.initorterm, inps.globals)
        } else {
          generateNewRules(topTheory, e, this.initorterm, inps.globals)
        }
        // Just to be on the safe side...
        val newRules = newRules_.toVector.filter(x => x.head.functor == this.initorterm)

        if (newRules.nonEmpty) logger.info(s"Generated ${newRules.length} new rules.")

        if (this.inps.compressNewRules) {
          newTopTheory = topTheory.clauses ++ filterTriedRules(topTheory, newRules, logger)
        } else {
          newTopTheory = topTheory.clauses ++ newRules
        }
      }
    }

    if (newTopTheory.clauses.nonEmpty) {

      val generate_refs_timed = Utils.time {
        newTopTheory.clauses.toVector foreach (rule => if (rule.refinements.isEmpty) rule.generateCandidateRefs)
      }


      val x = predictSate(newTopTheory, e, inps, initorterm)


      //logger.info(s"Generate refinements time: ${generate_refs_timed._2}")
      // There's an issue with inference with empty-bodied rules (see the big comment at
      // ASP2MLN.parseRules for more details, so for now we do not give these rules to the MLN side).
      val allClauses = newTopTheory.clauses.flatMap { x =>
        if (x.body.nonEmpty) List(x) ++ x.refinements
        else x.refinements
      }

      val ((groundNetwork, trueGroundingsMap, totalExmplCount, annotationMLN,
      incorrectlyTerminated, correctlyNotTerminated), groundingTime) = {
        val timed = Utils.time{ getGroundTheory(allClauses.toVector, e, inps, this.initorterm) }
        (timed._1, timed._2)
      }

      val enumClauses = (1 to allClauses.length).toList
      val uuidsToRuleIdsMap = (allClauses.map(_.uuid) zip enumClauses).toMap

      /* I'll have to make this generic to work with termination too... */
      //val annotationMLN = Auxil.getAnnotation(e, enumClauses)

      // Update weights
      val trueGroundingsPerClause =
        // Is there a reason to have an if here?
        if (this.initorterm == "terminatedAt") {
          enumClauses map (clauseId => trueGroundingsMap(clauseId).sum)
        } else {
          enumClauses map (clauseId => trueGroundingsMap(clauseId).sum)
        }

      val (clausesWithUpdatedWeights, adagradTime) = {
        val timed = Utils.time{

          AdaGrad.adagrad(inps, groundNetwork, allClauses.toVector,
            trueGroundingsPerClause, annotationMLN, correctlyNotTerminated, incorrectlyTerminated, initorterm)

        }
        (timed._1, timed._2)
      }


      //val groundMRF = getMRFAsStringSet(mrf)

      /* DEBUG */
      //val groundNetwork1 = Test.getGroundNetwork_OLD_WAY(newTopTheory, e, this.initorterm, step, inps, jep)
      //val groundNetwork2 = groundNetwork.map(x => x.tostring_mln).toSet
      //assert(groundNetwork1 == groundNetwork2)
      /* DEBUG */

      // Perform inference
      val solver = new MAPInference
      val (newInferredGroundNetwork, mapInferenceTime): (Vector[Literal], Double) = {
        val timed = Utils.time { solver.infer(groundNetwork, clausesWithUpdatedWeights) }
        (timed._1, timed._2)
      }

      // Atoms that inferred as true:
      val inferredTrue = newInferredGroundNetwork.filter(x => x.mlnTruthValue)
      val actuallyTrue = annotationMLN //annotationMLN.map(x => x.tostring_mln).toSet

      val exmplCount = totalExmplCount //groundNetwork.length

      Utils.time {
        newTopTheory.clauses foreach { clause =>

          if (clause.body.nonEmpty) Auxil.updateClauseStats(clause,
            uuidsToRuleIdsMap, inferredTrue, actuallyTrue, incorrectlyTerminated,
            correctlyNotTerminated, clausesWithUpdatedWeights, this.initorterm)

          clause.refinements.foreach(ref =>
            Auxil.updateClauseStats(ref, uuidsToRuleIdsMap, inferredTrue,
              actuallyTrue, incorrectlyTerminated, correctlyNotTerminated,
              clausesWithUpdatedWeights, this.initorterm))

          clause.seenExmplsNum += exmplCount
          clause.refinements.toVector.foreach(y => y.seenExmplsNum += exmplCount)
          clause.supportSet.clauses.toVector.foreach(y => y.seenExmplsNum += exmplCount)
        }
      }

      val (expanded, expTimed) = Utils.time { expandRules(newTopTheory) }

      ///*
      logger.info(s"\nGround network time (clingo + parsing results):" +
        s" $groundingTime\nAdaGrad Time: $adagradTime\nMAP Inference time:" +
        s" $mapInferenceTime\nStart new rules (ASP) time: $startNewRulesTime\nExpand clauses time: $expTimed")
      //*/

      if (inps.onlinePruning) {
        pruneRules(expanded, inps, logger)
      } else {
        expanded
      }
    } else {
      newTopTheory
    }

  }






  def predictSate(topTheory: Theory, e: Example, inps: RunningOptions, targetClass: String) = {

    val clauses = topTheory.clauses.map { topClause =>
      val bestRef = topClause.refinements.sortBy(x => - x.mlnWeight).head
      if (topClause.mlnWeight > bestRef.mlnWeight) topClause else bestRef
    }

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

    val inferredTrue = inferredGroundNetwork.filter(x => x.mlnTruthValue).
      map(x => Literal(functor = x.functor, terms = x.terms.take(x.terms.length-1))).map(x => x.tostring_mln).toSet

    val actuallyTrue = annotationMLN.
      map(x => Literal(functor = x.functor, terms = x.terms.take(x.terms.length-1))).map(x => x.tostring_mln).toSet

    if (actuallyTrue.diff(inferredTrue).nonEmpty) {
      val stop = "stop"
    }

    (inferredTrue, actuallyTrue, incorrectlyTerminated, correctlyNotTerminated, clauses, totalExmplCount)

  }





   /* For dubugging, run with a single hard-coded rule*/


  ///*

  val debugPath = "/home/nkatz/tmp/debug/"

  var start = 0
  var end = 9

  def processExample_1(topTheory: Theory, e: Example): Theory = {

    val allClauses = topTheory.clauses

    /* DEBUG */
    /*====================================================================*/
    /*
    val functionMaps = List("Inactive_B = inactive(B)","Meeting_A_B = meeting(A,B)","Disappear_B = disappear(B)","Active_B = active(B)","Meeting_B_A = meeting(B,A)","Appear_B = appear(B)","Active_A = active(A)","Inactive_A = inactive(A)","Walking_B = walking(B)","Meeting_B_B = meeting(B,B)","Running_A = running(A)","Walking_A = walking(A)","Disappear_A = disappear(A)","Meeting_A_A = meeting(A,A)","Running_B = running(B)","Appear_A = appear(A)")

    val data_ = (e.annotation ++ e.narrative).map { x =>
      val a = Literal.parseWPB2(x)
      Literal.toMLNFlat(a)
    }

    val data = data_.map(x => x.tostring_mln)

    /*
    val fileName = {
      val sorted = data_.sortBy(x => x.terms(1).name.toInt)
      s"${sorted.head.terms(1).name}-${sorted.last.terms(1).name}.db"
    }
    */

    val fileName = s"$start-$end.db"
    start = end+1
    end = end+10

    val nextAtoms = ASP.solveMLNGrounding(this.inps, e, Vector[(Clause, Clause, Clause)](), this.initorterm).map{ x =>
      x.capitalize
    }

    val batchData = functionMaps.mkString("\n")+"\n"+data.mkString("\n")+"\n"+nextAtoms.mkString("\n")

    val file = new File(s"$debugPath$fileName")
    val bw = new BufferedWriter(new FileWriter(file))
    bw.write(batchData)
    bw.close()

    */
    /*====================================================================*/
    /* DEBUG */


    ///*
    println(s"MLN WEIGHT: ${allClauses.head.mlnWeight}")


    val enumClauses = (1 to allClauses.length).toList

    val uuidsToRuleIdsMap = (allClauses.map(_.uuid) zip enumClauses) toMap

    val ((groundNetwork, trueGroundingsMap, totalExmplCount, annotationMLN,
    incorrectlyTerminated, correctlyNotTerminated), groundingTime) = {
      val timed = Utils.time{ getGroundTheory(allClauses.toVector, e, inps, this.initorterm) }
      (timed._1, timed._2)
    }

    if (annotationMLN.nonEmpty) {
      val stop = "stop"
    }

    // Update weights
    val trueGroundingsPerClause = enumClauses map (clauseId => trueGroundingsMap(clauseId).sum)
    val (clausesWithUpdatedWeights, adagradTime) = {
      val timed = Utils.time{
        AdaGrad.adagrad(inps, groundNetwork, allClauses.toVector,
          trueGroundingsPerClause, annotationMLN, correctlyNotTerminated, incorrectlyTerminated, initorterm)
      }
      (timed._1, timed._2)
    }

    val solver = new MAPInference
    val (newInferredGroundNetwork, mapInferenceTime): (Vector[Literal], Double) = {
      val timed = Utils.time { solver.infer(groundNetwork, clausesWithUpdatedWeights) }
      (timed._1, timed._2)
    }

    // Atoms that inferred as true:
    val inferredTrue = newInferredGroundNetwork.filter(x => x.mlnTruthValue)
    val actuallyTrue = annotationMLN //annotationMLN.map(x => x.tostring_mln).toSet

    val exmplCount = totalExmplCount //groundNetwork.length


    Utils.time {
      topTheory.clauses foreach { clause =>

        if (clause.body.nonEmpty) Auxil.updateClauseStats(clause,
          uuidsToRuleIdsMap, inferredTrue, actuallyTrue, incorrectlyTerminated,
          correctlyNotTerminated, clausesWithUpdatedWeights, this.initorterm)

        clause.seenExmplsNum += exmplCount
        clause.supportSet.clauses.toVector.foreach(y => y.seenExmplsNum += exmplCount)
      }
    }
    //*/

    topTheory
  }







}

package oled.weightlearn.parallel

import app.runutils.RunningOptions
import logic.Examples.Example
import logic.{Literal, Theory}
import oled.weightlearn.{Auxil, MAPInference}
import utils.Utils
import oled.functions.WeightLearningFunctions.getGroundTheory

object Test extends App {


  println(Auxil.cnfToRules)

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

    val inferredTrue = inferredGroundNetwork.filter(x => x.mlnTruthValue)
    val actuallyTrue = annotationMLN

    (inferredTrue, actuallyTrue, incorrectlyTerminated, correctlyNotTerminated, clauses, totalExmplCount)

  }

}

package oled.weightlearn.parallel

import akka.actor.Actor
import com.typesafe.scalalogging.LazyLogging
import logic.Literal
import oled.functions.WeightLearningFunctions._
import oled.weightlearn.parallel.IO.{MLNClauseHandlingInput, MLNClauseHandlingOutput}
import oled.weightlearn.{AdaGrad, MAPInference}
import org.slf4j.LoggerFactory
import utils.Utils



class MLNClauseEvalWorker extends Actor {

  private val logger = LoggerFactory.getLogger(self.path.name)

  def receive = {
    case x: MLNClauseHandlingInput => sender ! process(x)
  }

  def process(x: MLNClauseHandlingInput) = {

    val ((groundNetwork, trueGroundingsMap, totalExmplCount, annotationMLN, incorrectlyTerminated, correctlyNotTerminated), groundingTime) = {
      val timed = Utils.time{ getGroundTheory(x.clauses, x.example, x.inps, x.targetClass, x.clauseIds.toList) }
      (timed._1, timed._2)
    }

    val trueGroundingsPerClause = x.clauseIds map (clauseId => trueGroundingsMap(clauseId).sum)

    val (clausesWithUpdatedWeights, adagradTime) = {
      val timed = Utils.time{
        AdaGrad.adagrad(x.inps, groundNetwork, x.clauses,
          trueGroundingsPerClause.toList, annotationMLN,
          correctlyNotTerminated, incorrectlyTerminated, x.targetClass, x.clauseIds.toList)
      }
      (timed._1, timed._2)
    }

    // Perform inference
    val solver = new MAPInference
    val (newInferredGroundNetwork, mapInferenceTime): (Vector[Literal], Double) = {
      val timed = Utils.time { solver.infer(groundNetwork, clausesWithUpdatedWeights, x.clauseIds.toList) }
      (timed._1, timed._2)
    }

    // Atoms that inferred as true:
    val inferredTrue = newInferredGroundNetwork.filter(x => x.mlnTruthValue)
    val actuallyTrue = annotationMLN

    new MLNClauseHandlingOutput(inferredTrue, actuallyTrue, incorrectlyTerminated,
      correctlyNotTerminated, clausesWithUpdatedWeights, totalExmplCount)
  }

}

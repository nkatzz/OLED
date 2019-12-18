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
      val bestRef = topClause.refinements.sortBy(x => -x.weight).head
      if (topClause.weight > bestRef.weight) topClause else bestRef
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

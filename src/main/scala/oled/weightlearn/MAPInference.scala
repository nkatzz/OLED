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

package oled.weightlearn

import app.runutils.Debug
import com.typesafe.scalalogging.LazyLogging
import logic.{Clause, Literal}
import optimus.algebra.AlgebraOps._
import optimus.algebra.Expression
import optimus.optimization._
import optimus.optimization.enums.{PreSolve, SolverLib}
import optimus.optimization.model.MPFloatVar

class MAPInference extends LazyLogging {

  implicit val problem = MPModel(SolverLib.LpSolve)

  /* Non-empty clauseIds are passed when rules are evaluated in parallel.
   * See also MLNClauseHandlingWorker:
   *
   * AdaGrad.adagrad(groundNetwork, x.clauses, trueGroundingsPerClause.toList, x.clauseIds.toList)
   *
   * and
   *
   * solver.infer(groundNetwork, clausesWithUpdatedWeights, x.clauseIds.toList)
   * */

  def infer(groundNetwork: Vector[Literal],
            liftedClauses: Vector[Clause], clauseIds: List[Int] = Nil): Vector[Literal] = {

    val enumClauses = clauseIds match {
      case Nil => (1 to liftedClauses.length).toList
      case _ => clauseIds
    }

    val idsToRuleIdsMap = (enumClauses zip liftedClauses).toMap

    val sTranslation = System.currentTimeMillis()

    var literalLPVars = Map.empty[Int, MPFloatVar]

    var expressions: List[Expression] = Nil

    groundNetwork.zipWithIndex.foreach { case (_, idx) =>
      literalLPVars += idx -> MPFloatVar(s"y$idx", 0, 1)
    }

    groundNetwork.zipWithIndex.foreach { case (lit, idx) =>

      // Literal weight:
      val weight = idsToRuleIdsMap(lit.derivedFrom).weight
      val floatVar = literalLPVars(idx)

      if (!lit.isNAF && weight != 0) expressions ::= weight * floatVar
      if (lit.isNAF) add((1 - floatVar) >:= 1)
    }

    val eTranslation = System.currentTimeMillis()
    //logger.info("Translation time: " + (eTranslation - sTranslation))

    // Step 4: Optimize function subject to the constraints introduced

    val solveTimed = utils.Utils.time{
      maximize(sum(expressions))
      start(PreSolve.CONSERVATIVE)
      release()
    }

    //logger.info("Solver time: " + solveTimed._2)

    Debug.totalILPSolverTime += solveTimed._2

    var nonIntegralSolutionsCounter = 0
    var fractionalSolutions = Vector.empty[Int]

    for((id, lpVar) <- literalLPVars) {
      val value = lpVar.value.getOrElse {
        logger.error(s"There is no solution for variable '${lpVar.symbol}'")
        sys.exit()
      }

      val normalisedValue = if (value > 0.99) 1.0 else value

      if (normalisedValue != 0.0 && normalisedValue != 1.0) {
        nonIntegralSolutionsCounter += 1
        fractionalSolutions +:= id
      }
      else {
        val currentAtom = groundNetwork(id)
        currentAtom.mlnTruthValue = if (normalisedValue == 0) false else true
      }
    }

    val sRoundUp = System.currentTimeMillis()

    if (nonIntegralSolutionsCounter > 0) {
      for (i <- fractionalSolutions.indices) {
        val id = fractionalSolutions(i)
        val currentAtom = groundNetwork(id)
        val weight = idsToRuleIdsMap(currentAtom.derivedFrom).weight

        if (currentAtom.mlnTruthValue && !currentAtom.isNAF && weight >= 0)
          currentAtom.mlnTruthValue = true
        else if (currentAtom.mlnTruthValue && !currentAtom.isNAF && weight < 0)
          currentAtom.mlnTruthValue = false
        else if (!currentAtom.mlnTruthValue && !currentAtom.isNAF && weight >= 0)
          currentAtom.mlnTruthValue = false
        else if (!currentAtom.mlnTruthValue && !currentAtom.isNAF && weight < 0)
          currentAtom.mlnTruthValue = true
        else if (currentAtom.isNAF) currentAtom.mlnTruthValue = false

   /*     else if (currentAtom.mlnTruthValue && currentAtom.isNAF && weight >= 0)
          currentAtom.mlnTruthValue = false
        else if (currentAtom.mlnTruthValue && currentAtom.isNAF && weight < 0)
          currentAtom.mlnTruthValue = true
        else if (!currentAtom.mlnTruthValue && !currentAtom.isNAF && weight >= 0)
          currentAtom.mlnTruthValue = false
        else if (!currentAtom.mlnTruthValue && !currentAtom.isNAF && weight < 0)
          currentAtom.mlnTruthValue = true
        else if (!currentAtom.mlnTruthValue && currentAtom.isNAF && weight >= 0)
          currentAtom.mlnTruthValue = true
        else if (!currentAtom.mlnTruthValue && currentAtom.isNAF && weight < 0)
          currentAtom.mlnTruthValue = false*/
      }
    }

    val eRoundUp = System.currentTimeMillis()
    //logger.info("Roundup time: " + (eRoundUp - sRoundUp))

    groundNetwork
  }
}

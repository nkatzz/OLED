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

import app.runutils.RunningOptions
import com.typesafe.scalalogging.LazyLogging
import logic.{Clause, Literal}


object AdaGrad extends LazyLogging {

  /* Non-empty clauseIds are passed when rules are evaluated in parallel.
   * See also MLNClauseHandlingWorker:
   *
   * AdaGrad.adagrad(groundNetwork, x.clauses, trueGroundingsPerClause.toList, x.clauseIds.toList)
   *
   * and
   *
   * solver.infer(groundNetwork, clausesWithUpdatedWeights, x.clauseIds.toList)
   * */
  def adagrad(inps: RunningOptions,
              groundNetwork: Vector[Literal],
              liftedClauses: Vector[Clause],
              trueGroundingsPerClause: List[Int],
              annotation: Vector[Literal],
              correctlyNotTerminated: Vector[Literal],
              incorrectlyTerminated: Vector[Literal],
              targetClass: String,
              clauseIds: List[Int] = Nil): Vector[Clause] = {

    val lambda: Double = inps.adaRegularization //0.001 // 0.01 default
    val eta: Double = inps.adaLearnRate//1.0 // default
    val delta: Double = inps.adaGradDelta//1.0

    val enumClauses = clauseIds match {
      case Nil => (1 to liftedClauses.length).toList
      case _ => clauseIds
    }

    val solver = new MAPInference
    val inferredGroundNetwork = solver.infer(groundNetwork, liftedClauses, clauseIds)

    val trueCounts = trueGroundingsPerClause

    if (inps.adaLossFunction == "default") {

      val inferredCounts = enumClauses map { clauseId =>
        inferredGroundNetwork.filter(p => p.derivedFrom == clauseId) count { x =>
          // we don't want the true negative counts...
          x.mlnTruthValue && !x.isNAF //|| (!x.mlnTruthValue && x.isNAF)
        }
      }

      //logger.info(s"True/Inferred counts:\n$trueCounts\n$inferredCounts")

      liftedClauses.zipWithIndex.foreach { case (c, idx) =>

        val currentSubgradient = inferredCounts(idx) - trueCounts(idx)

        c.subGradient += currentSubgradient * currentSubgradient

        val coefficient = eta / (delta + math.sqrt(c.subGradient))
        val value = c.weight - coefficient * currentSubgradient
        val difference = math.abs(value) - (lambda * coefficient)

        if (difference > 0) c.weight = if (value >= 0) difference else -difference
        else c.weight = 0.0
      }

    } else { // custom loss function

      val lossVector = getCustomLoss(enumClauses, inferredGroundNetwork, annotation,
        correctlyNotTerminated, incorrectlyTerminated, targetClass)

      liftedClauses.zipWithIndex.foreach { case (c, idx) =>

        // This is exactly the same with the default loss function
        // (as above) for the initiation case.
        val currentSubgradient =
          if (targetClass == "terminatedAt") {
            val relatedTps_term = correctlyNotTerminated.filter(p => p.derivedFrom == idx)
            lossVector(idx) - relatedTps_term.size
          } else lossVector(idx) - trueCounts(idx)

        c.subGradient += currentSubgradient * currentSubgradient

        val coefficient = eta / (delta + math.sqrt(c.subGradient))
        val value = c.weight - coefficient * currentSubgradient
        val difference = math.abs(value) - (lambda * coefficient)

        if (difference > 0) c.weight = if (value >= 0) difference else -difference
        else c.weight = 0.0
      }

    }

    liftedClauses
  }


  /* It seems that the only reason to use this is for weighting tps, fps, fns.
   * A different function for termination is not needed. The way we've designed it,
    * fps in termination (instances where a termination clause erroneousely fires)
    * correspond to fns in termination for OLED. And since we counts tps and fps here, we're done. */
  def getCustomLoss(enumClauses: List[Int], inferredGroundNetwork: Vector[Literal], annotation: Vector[Literal],
                    correctlyNotTerminated: Vector[Literal], incorrectlyTerminated: Vector[Literal],
                    targetClass: String) = {

    /*
    if (targetClass == "terminatedAt") {

      val tps_fns_per_clause = getCountsTerminated(enumClauses, inferredGroundNetwork,
        correctlyNotTerminated, incorrectlyTerminated)

      tps_fns_per_clause map {x =>
        val (tps, fns) = (x._1, x._2)
        //val recall = if (tps == 0) 0.0 else tps.toDouble/(tps.toDouble+fns.toDouble)
        //recall
        (tps+fns).toDouble
      } // tps - fns

    } else { // this works for general concepts to (not necessarily initiatedAt)

      val tps_fps_fns_per_clause = getCountsInitiated(enumClauses, annotation, inferredGroundNetwork)
      tps_fps_fns_per_clause map {x =>
        val (tps, fps) = (x._1, x._2)
        //val precision = if (tps == 0) 0.0 else tps.toDouble/(tps.toDouble+fps.toDouble)
        //precision
        (tps+fps).toDouble
      } //tps - fps

    }
    */
    val tps_fps_fns_per_clause = getCounts(enumClauses, annotation, inferredGroundNetwork)
    tps_fps_fns_per_clause map {x =>
      val (tps, fps) = (x._1, x._2)
      //val precision = if (tps == 0) 0.0 else tps.toDouble/(tps.toDouble+fps.toDouble)
      //precision
      (tps+fps).toDouble
    } //tps - fps

  }


  /*
  def getCountsTerminated(enumClauses: List[Int], inferredGroundNetwork: Vector[Literal],
                          correctlyNotTerminated: Vector[Literal],
                          incorrectlyTerminated: Vector[Literal]) = {

    def isTrue(inferredAtom: Literal) = { inferredAtom.mlnTruthValue && !inferredAtom.isNAF }

    def isTPAtom(relevantCorrectlyNotTerminated: Set[String], inferredAtom: Literal) = {
      !isTrue(inferredAtom) && relevantCorrectlyNotTerminated.contains(inferredAtom.tostring_mln)
    }

    def isFNAtom(relevantIncorrectlyTerminated: Set[String], inferredAtom: Literal) = {
      isTrue(inferredAtom) && relevantIncorrectlyTerminated.contains(inferredAtom.tostring_mln)
    }

    val tps_fns_per_clause = enumClauses map { clauseId =>

      val relevantCorrectlyNotTerminated = correctlyNotTerminated.
        filter(p => p.derivedFrom == clauseId).map(x => x.tostring_mln).toSet

      val relevantIncorrectlyTerminated = incorrectlyTerminated.
        filter(p => p.derivedFrom == clauseId).map(x => x.tostring_mln).toSet

      inferredGroundNetwork.filter(p => p.derivedFrom == clauseId).foldLeft(0,0){ (counts, inferredAtom) =>

        if (isTPAtom(relevantCorrectlyNotTerminated, inferredAtom)) (counts._1 + 1, counts._2)

        else if (isFNAtom(relevantIncorrectlyTerminated , inferredAtom)) (counts._1, counts._2 + 1)
        else (counts._1, counts._2) // we don't count anything else.

      }
    }
    println(s"(tps, fns): $tps_fns_per_clause")
    tps_fns_per_clause
  }
  */

  def getCounts(enumClauses: List[Int], annotation: Vector[Literal],
                         inferredGroundNetwork: Vector[Literal]) = {

    def isTrue(inferredAtom: Literal) = { inferredAtom.mlnTruthValue && !inferredAtom.isNAF }

    def isTPAtom(annotation: Set[String], inferredAtom: Literal) = {
      isTrue(inferredAtom) && annotation.contains(inferredAtom.tostringMLN)
    }

    def isFPAtom(annotation: Set[String], inferredAtom: Literal) = {
      isTrue(inferredAtom) && !annotation.contains(inferredAtom.tostringMLN)
    }

    def isFNAtom(annotation: Set[String], inferredAtom: Literal) = {
      !isTrue(inferredAtom) && annotation.contains(inferredAtom.tostringMLN)
    }

    val tps_fps_fns_per_clause = enumClauses map { clauseId =>

      val relevantAnnotationAtoms = annotation.filter(p => p.derivedFrom == clauseId).map(x => x.tostringMLN).toSet

      inferredGroundNetwork.filter(p => p.derivedFrom == clauseId).foldLeft(0,0,0){ (counts, inferredAtom) =>

        if (isTPAtom(relevantAnnotationAtoms, inferredAtom)) {
          (counts._1 + 1, counts._2, counts._3)
        } else if (isFPAtom(relevantAnnotationAtoms, inferredAtom)) {
           (counts._1, counts._2 + 1, counts._3)
        } else if (isFNAtom(relevantAnnotationAtoms, inferredAtom)) {
           (counts._1, counts._2, counts._3 + 1)
        } else {
          (counts._1, counts._2, counts._3) // we don't count true negatives.
        }

      }
    }
    println(s"(tps, fps, fns): $tps_fps_fns_per_clause")
    tps_fps_fns_per_clause
  }

}

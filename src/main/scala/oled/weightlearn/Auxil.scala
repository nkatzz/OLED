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

import logic.Examples.Example
import logic.{Clause, Constant, Literal}

object Auxil {

  def updateClauseStats(clause: Clause, uuidsToRuleIdsMap: Map[String, Int],
                        inferredTrue: Vector[Literal], _actuallyTrue: Vector[Literal],
                        incorrectlyTerminated: Vector[Literal], correctlyNotTerminated: Vector[Literal],
                        clausesWithUpdatedWeights: scala.Vector[Clause], targetClass: String) = {

    if (targetClass == "initiatedAt" || targetClass == "terminatedAt") {
      val clauseId = uuidsToRuleIdsMap(clause.uuid)
      val inferredTrueByThisClause = inferredTrue.filter(p => p.derivedFrom == clauseId).map(x => x.tostringMLN).toSet
      val actuallyTrue = _actuallyTrue.filter(p => p.derivedFrom == clauseId).map(x => x.tostringMLN).toSet

      val tps =
        if (targetClass == "terminatedAt") {
          val tpsCrisp = correctlyNotTerminated.filter(p => p.derivedFrom == clauseId).map(x => x.tostringMLN).toSet
          tpsCrisp.diff(inferredTrueByThisClause).size
        } else {
          inferredTrueByThisClause.intersect(actuallyTrue).size
        }

      val fps = inferredTrueByThisClause.diff(actuallyTrue).size

      val fns =
        if (targetClass == "terminatedAt") {
          val fnsCrisp = incorrectlyTerminated.filter(p => p.derivedFrom == clauseId).map(x => x.tostringMLN).toSet
          inferredTrueByThisClause.intersect(fnsCrisp).size
        } else {
          actuallyTrue.diff(inferredTrueByThisClause).size
        }

      clause.tps += tps
      clause.fps += fps
      clause.fns += fns

      val updtWeightCl = clausesWithUpdatedWeights.find(c => c.uuid == clause.uuid).
        getOrElse(throw new RuntimeException(s"Cannot find clause ${clause.uuid} in the updated weights clause vector returned from AdaGrad."))

      clause.weight = updtWeightCl.weight
      clause.subGradient = updtWeightCl.subGradient

      /*
      logger.info(s"\nClause:\n${clause.tostring}\ntps: ${clause.tps} | fps: ${clause.fps} |" +
        s" fns: ${clause.fns} | seen: ${clause.seenExmplsNum} | mln-weight: ${clause.mlnWeight}" +
        s" | sungradient: ${clause.subGradient}")
      */
    } else { // for termination conditions???

    }


  }

  def debug(groundNetwork: Array[Literal]) = {
    groundNetwork.sortBy(x => (x.terms(2).name.split("_")(1).toInt,
      x.terms(1).name.toInt, x.terms.head.name.split("_")(1), x.terms.head.name.split("_")(1)) )
      .map(z => z.tostringMLN).mkString("\n")
  }

  def getAnnotation(e: Example, enumClauses: List[Int]) = {
    e.annotation.flatMap { x =>
      val p = Literal.parseWPB2(x)

      val functor = "initiatedAt"
      val terms = p.terms.take(p.terms.length-1)
      val time = p.terms.last.name.toInt - 1


      val p1 = enumClauses map { clauseId =>
        val a = Literal(predSymbol = functor, terms = terms ++ List(Constant(time.toString), Constant(s"ruleId_$clauseId")) )
        Literal.toMLNFlat(a)
      }

      p1
    }
  }


  /* Converts a set of CNF clauses learned by OSLa to rules. For example:
   *
   * 0.410215389776 HoldsAt(meet(x4, x1),t1) v !Happens(inactive(x1),t0) v !Happens(walking(x4),t0) v !Next(t1,t0) v !Close(x1,x4,24,t0)
   *
   * turns into
   *
   * 0.410215389776 InitiatedAt(meeting(x4,x1),t0) :- HappensAt(inactive(x1),t0) ^ HappensAt(walking(x4),t0) ^ Close(x1,x4,24,t0)
   *
   * */
  def cnfToRules = {

    def cnfClauseToRule(clause: String) = {
      val noOr = clause.replaceAll(", ", ",").replaceAll(" v ", " ").split(" ").map(x => x.trim).filter(x => x!= "")
      val weight = noOr.take(1).head
      val atoms = noOr.drop(1)
      val (head, body) = atoms.foldLeft(Vector.empty[String], Vector.empty[String]) { (accum, atom) =>
        if (atom.contains("HoldsAt")) (accum._1 :+ atom, accum._2) else (accum._1, accum._2 :+ atom)
      }

      val _head =
        if (head.head.startsWith("!")) {
          head.head.replaceAll("!", "").replaceAll("HoldsAt", "TerminatedAt").replaceAll("t1", "t0")
        } else {
          head.head.replaceAll("HoldsAt", "InitiatedAt").replaceAll("t1", "t0")
        }

      val _body = body.filter(x => !x.contains("Next")).map { x =>
        if (x.startsWith("!")) x.replaceAll("!", "")
        else "!"+x
      }.mkString(" ^ ")

      s"$weight ${_head} :- ${_body}".
        replaceAll("meet", "meeting").replaceAll("enter", "appear").
        replaceAll("exit", "disappear").replaceAll("Happens", "HappensAt")
    }

    scala.io.Source.fromFile("/home/nkatz/dev/learned.mln").

      getLines.filter(x => x != "").toVector.map(x => cnfClauseToRule(x)).mkString("\n")


  }


}

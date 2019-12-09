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

package logic

import app.runutils.Globals
import logic.Examples.Example
import utils.{ASP, Utils}
import xhail.Xhail

import scala.collection.mutable.ListBuffer


/**
  * Created by nkatz on 9/13/16.
  */

object LogicUtils {

  /*
  * Simplifies a rule by removing redundant comparison predicates from its body.
  * */
  def simplifyRule(c: Clause, gl: Globals) = {

    val (nonComparisonPreds, comparisonPreds) = c.body.foldLeft(Set[Literal](), Set[Literal]()) { (accum, lit) =>
      if (gl.comparisonPredicates.contains(lit.modeAtom)) (accum._1, accum._2 + lit) else (accum._1 + lit, accum._2)
    }

    val grouped = comparisonPreds.groupBy(x => x.modeAtom)

    val simplified = grouped.map{ case (modeAtom, literals) =>
      if (modeAtom.compRelation == "lessThan") {
        literals.toList.minBy(_.getComparisonTerm.name.toInt)
      } else if (modeAtom.compRelation == "greaterThan") {
        literals.toList.maxBy(_.getComparisonTerm.name.toInt)
      } else {
        throw new RuntimeException(s"Don't know what to do with this comparison relation: ${modeAtom.compRelation}")
      }
    }.toSet

    val newTerms = nonComparisonPreds.toList ++ simplified.toList

    val cc = Clause(head = c.head, body = newTerms, supportSet = c.supportSet, uuid = c.uuid)
    // Just to be on the safe side...
    cc.parentClause = c.parentClause
    cc.countsPerNode = c.countsPerNode
    cc.weight = c.weight
    cc.subGradient = c.subGradient
    cc.w_pos = c.w_pos
    cc.totalTPs = c.totalTPs
    cc.totalFPs = c.totalFPs
    cc.totalFNs = c.totalFNs
    cc.totalSeenExmpls = c.totalSeenExmpls
    cc.tps = c.tps
    cc.fps = c.fps
    cc.fns = c.fns
    cc.refinements = c.refinements
    cc.seenExmplsNum = c.seenExmplsNum
    cc.previousMeanDiffCount = c.previousMeanDiffCount
    cc.previousMeanScoreCount = c.previousMeanScoreCount
    cc.previousMeanDiff = c.previousMeanDiff
    cc.previousScore = c.previousScore

    cc

  }

  ///*
  def compressTheory(kernel: List[Clause]): List[Clause] = {
    val compressed = new ListBuffer[Clause]
    val included = (c: Clause) => compressed.toList.exists(x => x.thetaSubsumes(c) && c.thetaSubsumes(x))
    for (c <- kernel) {
      if (!included(c)) compressed += c
    }
    compressed.toList
  }

  def compressTheory_RemoveSubsumers(kernel: List[Clause]): List[Clause] = {
    val compressed = new ListBuffer[Clause]
    val included = (c: Clause) => compressed.toList.exists(x => c.thetaSubsumes(x))
    for (c <- kernel) {
      if (!included(c)) compressed += c
    }
    compressed.toList
  }


  //*/

  /*
  def compressTheory(kernel: List[Clause]): List[Clause] = {
    val compressed = new ListBuffer[Clause]
    val included = (c: Clause) => compressed.toList.exists(x => c.thetaSubsumes(x))
    for (c <- kernel) {
      if (!included(c)) compressed += c
    }
    compressed.toList
  }
  */

  def generateKernel(examples: Map[String,List[String]], fromWeakExmpl: Boolean = false,
                     learningTerminatedOnly: Boolean=false, bkFile: String, globals: Globals) = {

    val infile = Utils.getTempFile("example", ".lp")
    val f = (x: String) => if (x.endsWith(".")) x else s"$x."
    val interpretation = examples("annotation").map(x => s"${f(x)}") ++ examples("narrative").map(x => s"${f(x)}")
    Utils.writeToFile(infile, "overwrite") { p => interpretation.foreach(p.println) }
    var (kernel, varKernel) =
      Xhail.runXhail(fromFile = infile.getAbsolutePath, kernelSetOnly = true,
        fromWeakExmpl = fromWeakExmpl, learningTerminatedAtOnly=learningTerminatedOnly, bkFile=bkFile, globals=globals)
    if (fromWeakExmpl) {
      varKernel = varKernel.map (x => Clause.updateField(x, fromWeakExample = true))
    }
    infile.delete()
    (kernel,varKernel)
  }

  /*The only difference is that the examples are provided with a file. I have to
  * fix this, it's stupid to duplicate code like that.*/
  def generateKernel2(examplesFile: java.io.File, bkFile: String, globals: Globals) = {
    val (kernel, varKernel) =
      Xhail.runXhail(fromFile = examplesFile.getAbsolutePath, kernelSetOnly = true, bkFile=bkFile, globals=globals)
    (kernel,varKernel)
  }


  def isSAT(theory: Theory, example: Example, globals: Globals, F: (Theory, Example, Globals) => String): Boolean = {
    val f = F(theory, example, globals)
    val out = ASP.solve(Globals.CHECKSAT, Map(), new java.io.File(f), example.toMapASP)
    if (out != Nil && out.head == AnswerSet.UNSAT) false else true
  }

  def updateSupport(theory: Theory, kernelSet: Theory, fromWeakExample: Boolean = false) = {

    // This is used to avoid adding redundant rules in the
    // support set. A rule is redundant if it subsumes
    // by a rule already present in the support set
    val isRedundant = (ss: Clause, c: Clause) =>
      c.supportSet.clauses exists (x => ss.thetaSubsumes(x))

    for (c <- theory.clauses;
         ks <- kernelSet.clauses
         if !isRedundant(ks, c) && c.thetaSubsumes(ks)) {
      val markedKS = Clause.updateField(ks, fromWeakExample = fromWeakExample)
      c.supportSet = Theory(c.supportSet.clauses :+ markedKS)
    }

    // This is used in order to avoid maintaining redundant
    // rules in the support set. In this context, a support set
    // rule is redundant if it subsumes some other rule
    // in the support set. This can happen in cases where e.g.
    // p :- q,r was added to the support set early on and
    // later on p :- q,r,s was added. In this case the first
    // rule is redundant and should be removed. This redundancy
    // checking should be done whenever the support set
    // changes with the addition of a rule.

    theory.clauses foreach (x => x.compressSupport)
  }


}

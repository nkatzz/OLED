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

import lomrf.logic._
import lomrf.logic.parser.KBParser
import lomrf.mln.grounding.MRFBuilder
import lomrf.mln.inference.{ILP, Solver}
import lomrf.mln.learning.weight.WLearning
import lomrf.mln.model.{EvidenceBuilder, KB, MLN, MLNSchema, PredicateSpace}
import lomrf.mln.model._
import lomrf.mln.model.mrf.MRF

import scala.util.{Failure, Success}

object LoMRFTest extends App {

  val sigma: Double = 1.0
  val lambda: Double = 0.01
  val eta: Double = 1.0
  val delta: Double = 1.0

  // This is the way to read predicate/functions schemata from files
  val (kb, constants) = KB.fromFile("/home/nkatz/dev/OLED/iled/datasets/CaviarWeightLearn/MLN/move_HI.mln")

  //println(kb.functionSchema)
  //println(kb.predicateSchema)

  val t = kb.predicateSchema.map { case (a, seq) => a -> seq.toVector }
  private val parser = new KBParser(t, kb.functionSchema)

  /*
  val rules = List(
    "1 InitiatedAt(move(id1, id2), t) :- HappensAt(walking(id1), t)",
    "0.6 InitiatedAt(move(id1, id2), t) :- HappensAt(walking(id2), t)",
    "1 TerminatedAt(move(id1, id2), t) :- HappensAt(active(id1), t)"
  )
  */

  val rules = List(
    "1 InitiatedAt(meeting(id1, id2), t) :- HappensAt(walking(id1), t)"
  )

  val definiteClauses = rules.map(parser.parseDefiniteClause)

  // Query predicates
  val queryPredicates = Set[AtomSignature](AtomSignature("InitiatedAt", 2))

  /*------------------------------------------------------------------------------------------------------*/
  /* This is what I need to call to read evidence from file!!!! */
  //val nnn = Evidence.fromFiles(kb,constants,queryPredicates,Vector(new File("some file with the data")),false,true)
  /*------------------------------------------------------------------------------------------------------*/

  val constantsDomain = Map(
    "time" -> ConstantsSet((0 to 1).map(_.toString)),
    "fluent" -> ConstantsSet("Meeting_A_B"),
    "event" -> ConstantsSet("Walking_A", "Walking_B"),
    "person" -> ConstantsSet("A", "B"),
    "dist" -> ConstantsSet("45")
  )

  val builder = EvidenceBuilder(kb.predicateSchema, kb.functionSchema, queryPredicates, Set.empty, constantsDomain)

  // Append evidence atoms for Happens predicate
  builder.evidence += EvidenceAtom.asTrue("HappensAt", Vector[Constant](Constant("Walking_A"), Constant(0.toString)))
  builder.evidence += EvidenceAtom.asTrue("Next", Vector[Constant](Constant(1.toString), Constant(0.toString)))

  builder.functions += FunctionMapping("Walking_A", "walking", Vector(Constant("A")))
  builder.functions += FunctionMapping("Walking_B", "walking", Vector(Constant("B")))
  builder.functions += FunctionMapping("Active_A", "active", Vector(Constant("B")))
  builder.functions += FunctionMapping("Active_B", "active", Vector(Constant("B")))
  builder.functions += FunctionMapping("Meeting_A_B", "meeting", Vector(Constant("A"), Constant("B")))

  val evidence = builder.result()

  val completed = PredicateCompletion(Set.empty, definiteClauses.toSet, PredicateCompletionMode.Decomposed)(kb.predicateSchema, kb.functionSchema, evidence.constants)

  val cnf = NormalForm.compileCNF(completed)(evidence.constants)

  val mlnSchema = MLNSchema(kb.predicateSchema, kb.functionSchema, Map.empty, Map.empty)
  val predicateSpace = PredicateSpace(mlnSchema, queryPredicates, evidence.constants)
  val mln = MLN(mlnSchema, evidence, predicateSpace, cnf.toVector)


  // ------- Create annotation
  val annotationSchema = Map(AtomSignature("InitiatedAt", 2) -> Vector("fluent", "time"))
  val annotationBuilder = EvidenceBuilder(annotationSchema, queryPredicates, Set.empty, constantsDomain)

  // ANNOTATION
  annotationBuilder.evidence +=
    EvidenceAtom.asTrue("InitiatedAt", Vector[Constant](Constant("Meeting_A_B"), Constant(0.toString)))
  annotationBuilder.evidence +=
    EvidenceAtom.asFalse("InitiatedAt", Vector[Constant](Constant("Meeting_A_B"), Constant(1.toString)))

  val annotationDB = annotationBuilder.result().db

  // ---------------------------------------------


  /*val weights = Array.fill[Double](numberOfClauses)(0.0)
  val sumSquareGradients = Array.fill[Int](numberOfClauses)(0)

  val mrfBuilder = new MRFBuilder(mln, createDependencyMap = true)
  val mrf = mrfBuilder.buildNetwork

  WLearning.setAnnotatedState(mrf, annotationDB)
  val trueCounts = WLearning.countGroundings(mrf, numberOfClauses)
  println("True Counts: [" + trueCounts.deep.mkString(", ") + "]")

  // Perform inference for the current weight vector and count true groundings
  val result = WLearning.infer(mrf, annotationDB, weights)
  val solver = new ILP(mrf, annotationDB = annotationDB, lossAugmented = false, ilpSolver = Solver.LPSOLVE)
  val result = solver.infer()*/

  import lomrf.mln.model.AtomIdentityFunctionOps._
  def atoms2TXT(mrf: MRF) = {
    val iterator = mrf.atoms.iterator()

    var result = Set.empty[String]
    while (iterator.hasNext) {
      iterator.advance()
      val atomID = iterator.key()

      val groundAtom = iterator.value()
      val state = if (groundAtom.getState) 1 else 0

      atomID.decodeAtom(mrf.mln) match {
        case Success(txtAtom) =>
          result += s"$txtAtom $state"
        case Failure(_) =>
          sys.error(s"failed to decode id: $atomID")
          sys.exit(1)
      }
    }
    result
  }

  val clausesWithUpdatedWeights = adagrad(mln, annotationDB)



  def adagrad(mln: MLN, annotationDB: EvidenceDB): Vector[Clause] = {

    val numberOfClauses = mln.clauses.length

    val mrfBuilder = new MRFBuilder(mln, createDependencyMap = true)
    val mrf = mrfBuilder.buildNetwork

    WLearning.setAnnotatedState(mrf, annotationDB)
    val trueCounts = WLearning.countGroundings(mrf, numberOfClauses)
    //println("True Counts: [" + trueCounts.deep.mkString(", ") + "]")

    // Perform inference for the current weight vector and count true groundings
    val solver = new ILP(mrf, annotationDB = annotationDB, lossAugmented = false, ilpSolver = Solver.LPSOLVE)
    solver.infer()

    val inferredCounts = WLearning.countGroundings(mrf, numberOfClauses)
    //println("Inferred Counts: [" + inferredCounts.deep.mkString(", ") + "]")

    mln.clauses.zipWithIndex.foreach { case (c, idx) =>
      val currentSubgradient = if (c.isHard) 0 else inferredCounts(idx) - trueCounts(idx)

      c.subgradient += currentSubgradient * currentSubgradient

      val coefficient = eta / (this.delta + math.sqrt(c.subgradient))
      val value = c.weight - coefficient * currentSubgradient
      val difference = math.abs(value) - (lambda * coefficient)

      if (difference > 0) c.weight = if (value >= 0) difference else -difference
      else c.weight = 0.0
    }
    mln.clauses
  }

  //////
  //
  //  val subgradients = Array.fill[Int](numberOfClauses)(0)
  //  for (clauseIdx <- 0 until numberOfClauses) {
  //    if (!mrf.mln.clauses(clauseIdx).isHard)
  //      subgradients(clauseIdx) = inferredCounts(clauseIdx) - trueCounts(clauseIdx)
  //  }
  //
  //  var clauseIdx = 0
  //  while (clauseIdx < numberOfClauses) {
  //
  //    sumSquareGradients(clauseIdx) += subgradients(clauseIdx) * subgradients(clauseIdx)
  //
  //    val coefficient = eta / (this.delta + math.sqrt(sumSquareGradients(clauseIdx)))
  //    val value = weights(clauseIdx) - coefficient * subgradients(clauseIdx)
  //    val difference = math.abs(value) - (lambda * coefficient)
  //
  //    if (difference > 0)
  //      weights(clauseIdx) = if (value >= 0) difference else -difference
  //    else weights(clauseIdx) = 0.0
  //
  //    clauseIdx += 1
  //  }

  mln.clauses.foreach { x =>
    println(x.toText(), x.weight)
  }




}

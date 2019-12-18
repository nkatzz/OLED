package woled

import java.io.File

import logic.{Clause, Literal}
import lomrf.logic.compile.{PredicateCompletion, PredicateCompletionMode}
import lomrf.logic.parser.KBParser
import lomrf.logic.{AtomSignature, Constant, EvidenceAtom, FunctionMapping}
import lomrf.mln.grounding.MRFBuilder
import lomrf.mln.inference.ILP
import lomrf.mln.learning.structure.ClauseConstructor
import lomrf.mln.model.AtomIdentityFunctionOps._
import lomrf.mln.model.{Evidence, EvidenceBuilder, KB, MLN}

import scala.io.Source

object MAPCorrect extends App {

  val queryAtoms = Set(
    AtomSignature("HoldsAt", 2),
    AtomSignature("InitiatedAt", 2),
    AtomSignature("TerminatedAt", 2)
  )

  //val mlnBKFile = "/home/nkatz/dev/BKExamples/BK-various-taks/WeightLearning/Caviar/fragment/meeting/MLN/MAPInferenceBK.mln"
  val mlnBKFile = "/home/nkatz/dev/WOLED-DEBUG/bk-and-rules"
  val (kb, constants) = KB.fromFile(mlnBKFile)
  val formulas = kb.formulas

  val parser = new KBParser(kb.predicateSchema.map { case (x, y) => x -> y.toVector }, kb.functionSchema)

  //val evidenceFile = new File("/home/nkatz/dev/BKExamples/BK-various-taks/WeightLearning/Caviar/fragment/meeting/MLN/23-Meet_Crowd.id0_id2.db")
  val evidenceFile = new File("/home/nkatz/dev/WOLED-DEBUG/evidence")

  val evidence = Evidence.fromFiles(kb, constants, queryAtoms, Seq(evidenceFile), false, false)

  //========================================================================================
  //val b = EvidenceBuilder(kb.predicateSchema, queryAtoms, Set.empty, constants)
  ///
  //b.functions += new FunctionMapping("A", "foo", Vector("B", "C"))
  //b.evidence += EvidenceAtom.asTrue("HappensAt", Vector(Constant("A")))
  ///
  //val e = b.result()
  //========================================================================================

  val source = Source.fromFile("/home/nkatz/dev/BKExamples/BK-various-taks/WeightLearning/Caviar/fragment/meeting/ASP/asp-rules-test")
  val list = source.getLines
  val rulesList = list.map(x => Clause.parse(x)).toList
  source.close

  val testRule = rulesList.head
  val head = testRule.head.asLiteral
  val body = testRule.body

  val ruleLiteralstoMLN = (head :: body).map(Literal.toMLNClauseLiteral)

  val result = infer(rulesList)

  val trueHoldsInit = result.filter{ case (k, v) => (k.startsWith("Init") || k.startsWith("Holds")) && v }

  println(s"Initiation & Holds atoms inferred as true:\n${trueHoldsInit.mkString("\n")}")

  def infer(rules: List[Clause]): Map[String, Boolean] = {

    /*val definiteClauses = rules.map { rule =>
      val head = Literal.toMLNClauseLiteral(rule.head.asLiteral).tostring_mln
      val body = rule.body.map(Literal.toMLNClauseLiteral(_).tostring_mln).mkString(" ^ ")
      parser.parseDefiniteClause(s"1 $head :- $body")
    }*/

    val definiteClauses = kb.definiteClauses

    definiteClauses.map(_.toText).foreach(println)

    val resultedFormulas = PredicateCompletion(formulas, definiteClauses.toSet, PredicateCompletionMode.Decomposed)(kb.predicateSchema, kb.functionSchema, constants)

    //val cnf = NormalForm.compileCNF(resultedFormulas)(constants).toVector

    println("Coverting to CNF...")

    val cnf = ClauseConstructor.makeCNF(resultedFormulas)(constants).toVector

    cnf.filter(x => !x.isHard).map(x => x.toText()).foreach(println)

    val mln = MLN(kb.schema, evidence, queryAtoms, cnf)
    val builder = new MRFBuilder(mln, createDependencyMap = true)
    val mrf = builder.buildNetwork

    val solver = ILP(mrf)
    solver.infer

    var result = Map.empty[String, Boolean]
    val queryStartID = mln.space.queryStartID
    val queryEndID = mln.space.queryEndID
    val iterator = mrf.atoms.iterator
    while (iterator.hasNext) {
      iterator.advance()
      val atomID = iterator.key
      if (atomID >= queryStartID && atomID <= queryEndID) {
        val groundAtom = iterator.value
        val state = if (groundAtom.getState) true else false
        result += atomID.decodeAtom(mln).get -> state
      }
    }

    result
  }
}

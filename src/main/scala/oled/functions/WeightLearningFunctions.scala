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

package oled.functions

import app.runutils.{Globals, RunningOptions}
import logic.Examples.Example
import logic._
import lomrf.logic.AtomSignature
import lomrf.mln.model.mrf.MRF
import lomrf.mln.model.{AtomIdentityFunctionOps, EvidenceBuilder, EvidenceDB, KB, MLN}
import oled.weightlearn.ASP2MLN
import oled.weightlearn.ASP2MLN.{adagrad1, adagrad2, cda, constructMLN}
import utils.{ASP, Utils}



object WeightLearningFunctions {


  class MLNInfoContainer(val mln: MLN, val labelAtoms: Set[String],
                         val fluentsMap: Map[String, String], val clauseIds: List[Int],
                         val annotationDB: EvidenceDB, val exmplCount: Int)

  /*
  *
  * types example: List("time", "person", "dist", "event", "fluent")
  * annotationTemplate example: "InitiatedAt(fluent,time)"
  *
  * This method returns an MLN the evidenceDB and the examples count from the current batch to use in the Hoeffding test.
  *
  * */
  def buildMLN(types: List[String], annotationTemplate: String,
               mlnClauses: Iterable[String], exmpl: Example,
               clauseIds: List[Int], inps: RunningOptions): MLNInfoContainer = {

    val aspBKPath = s"${inps.entryPath}/ASP/get-predicates.lp"
    val mlnBKPath = s"${inps.entryPath}/MLN/move_HI.mln"

    val parsedTargetPred = Literal.parse(annotationTemplate.toCharArray.take(1)(0).toString.toLowerCase() + annotationTemplate.drop(1))
    val targetPredASP = parsedTargetPred.predSymbol
    val targetPredMLN = parsedTargetPred.predSymbol.capitalize

    def getDomain() = {
      val rulsIdPreds = clauseIds map (i => s"ruleId(ruleId_$i).")
      val all = (exmpl.annotationASP ++ exmpl.narrativeASP ++ rulsIdPreds ++ List(s"""#include "$aspBKPath".""")) .mkString("\n")
      val f = Utils.getTempFile("domain", ".lp")
      Utils.writeLine(all, f.getCanonicalPath, "overwrite")
      ASP.solve(task=Globals.INFERENCE, aspInputFile=f)
    }

    val (exmplCountAtom, atoms) = getDomain().head.atoms.partition(x => x.contains("exmplCount"))

    if (exmplCountAtom.length > 1) throw new RuntimeException("More than one exmplCount/1 atoms")
    if (exmplCountAtom.isEmpty) throw new RuntimeException("No exmplCount/1 atoms")

    val (kb, constants) = KB.fromFile(mlnBKPath)

    val queryPredicates = Set[AtomSignature](AtomSignature(targetPredMLN, parsedTargetPred.arity))

    val t0 = System.nanoTime()
    val (fluentsMap, eventsMap, aspLabelAtoms, aspEventAtoms, allOtherAtoms) = ASP2MLN.getAllASPAtoms(atoms, inps.targetHLE, targetPredASP)
    val t1 = System.nanoTime()
    println(s"GetAllASPAtoms time: ${(t1-t0)/1000000000.0}")

    // First, gather all type predicates to form the constantsDomain.
    val parsedDomainAtoms = allOtherAtoms.map(x => Literal.parse(x))

    //split type predicates from simple domain atoms:
    val (typeAtoms, domainAtoms) = parsedDomainAtoms.foldLeft(Vector[Literal](), Vector[Literal]()) { (accum, lit) =>
      if (types.contains(lit.predSymbol)) (accum._1 :+ lit, accum._2) else (accum._1, accum._2 :+ lit)
    }

    val constantsDomain = ASP2MLN.getConstantsDomain(typeAtoms, fluentsMap, eventsMap)

    val evidenceBuilder = EvidenceBuilder(kb.predicateSchema, kb.functionSchema, queryPredicates, Set.empty, constantsDomain)
    val evidence_ = Utils.time{ ASP2MLN.buildEvidence(aspEventAtoms, aspLabelAtoms, domainAtoms, eventsMap, fluentsMap, evidenceBuilder) }
    val evidence = evidence_._1
    println(s"Evidence building time: ${evidence_._2}")

    val annotationSchema = ASP2MLN.getAnnotationSchema(annotationTemplate)
    val annotationBuilder = EvidenceBuilder(annotationSchema, queryPredicates, Set.empty, constantsDomain)
    val annotationDB_ = Utils.time{ ASP2MLN.buildAnnotation(aspLabelAtoms, fluentsMap, annotationBuilder) }
    val annotationDB = annotationDB_._1
    println(s"Annotation building time: ${annotationDB_._2}")

    val mln_ = Utils.time{ ASP2MLN.constructMLN(kb, evidence, mlnClauses, queryPredicates) }
    val mln = mln_._1
    println(s"Construct MLN time: ${mln_._2}")

    val exmplCount = exmplCountAtom.head.split("exmplCount\\(")(1).split("\\)")(0).toInt

    val out = new MLNInfoContainer(mln, aspLabelAtoms.toSet[String], fluentsMap, clauseIds, annotationDB, exmplCount)

    out

  }


  def getGroundTheory(clauses: Vector[Clause], e: Example,
                      inps: RunningOptions, targetClass: String, clauseIds: List[Int] = Nil) = {

    /*
    def proxyAtom(lit: PosLiteral) = {
      if (lit.functor == "terminatedAt") Literal(functor = "terminatedAt1", terms = lit.terms)
      else lit
    }

    def proxyFunctor(pred: String) = if (pred == "terminatedAt") "terminatedAt1" else pred
    */

    val enum = if (clauseIds.isEmpty) clauses zip (1 to clauses.length) else clauses zip clauseIds

    val p = enum map { x =>
      val (clause, index) = (x._1, x._2)
      val typeAtoms = clause.toLiteralList.flatMap(x => x.getTypePredicates(inps.globals)).distinct.map(x => Literal.parse(x))
      val markedHead = Literal(predSymbol = clause.head.functor, terms = clause.head.terms :+ Constant(s"ruleId_$index"))
      val markedClause = Clause(head = markedHead, body = clause.body ++ typeAtoms)

      /*
      // We don't need the body terms
      val unsatClauseHead = Literal(functor = "false_clause", terms = List(Constant(s"ruleId_$index"), proxyAtom(clause.head)) )
      // the body is the negation of the head
      val unsatClauseBody = Literal(functor = proxyFunctor(clause.head.functor), terms = clause.head.terms :+ Constant(s"ruleId_$index"), isNAF = true)
      val unsatClause = Clause(head = unsatClauseHead.asPosLiteral, body = List(unsatClauseBody)++ typeAtoms)

      // We also need the satisfied clauses
      val satClauseHead = Literal(functor = "true_clause", terms = List(Constant(s"ruleId_$index"), proxyAtom(clause.head)) )
      // the body is the negation of the head
      val satClauseBody = Literal(functor = proxyFunctor(clause.head.functor), terms = clause.head.terms :+ Constant(s"ruleId_$index"))
      val satClause = Clause(head = satClauseHead.asPosLiteral, body = List(satClauseBody)++ typeAtoms)
      */

      // We don't need the body terms
      val unsatClauseHead = Literal(predSymbol = "false_clause", terms = List(Constant(s"ruleId_$index"), clause.head) )
      // the body is the negation of the head
      val unsatClauseBody = Literal(predSymbol = clause.head.functor, terms = clause.head.terms :+ Constant(s"ruleId_$index"), isNAF = true)
      val unsatClause = Clause(head = unsatClauseHead.asPosLiteral, body = List(unsatClauseBody)++ typeAtoms)

      // We also need the satisfied clauses
      val satClauseHead = Literal(predSymbol = "true_clause", terms = List(Constant(s"ruleId_$index"), clause.head) )
      // the body is the negation of the head
      val satClauseBody = Literal(predSymbol = clause.head.functor, terms = clause.head.terms :+ Constant(s"ruleId_$index"))
      val satClause = Clause(head = satClauseHead.asPosLiteral, body = List(satClauseBody)++ typeAtoms)


      (markedClause, unsatClause, satClause)
    }

    val atoms_timed = Utils.time(ASP.solveMLNGrounding(inps, e, p, targetClass))
    val atoms = atoms_timed._1

    /* post-process */

    //val (trueGroundingsAtoms, trueFalseGrndClauseAtoms) = atoms.partition(x => x.startsWith("true_groundings"))

    val (trueGroundingsAtoms, totalExmplCount_, trueFalseGrndClauseAtoms, annotation_, fnsTerminated_, tpsTerminated_) =
      atoms.foldLeft(Vector[String](), Vector[String](), Vector[String](),
        Vector[String](), Vector[String](), Vector[String]()) { (x, atom) =>
      if (atom.startsWith("true_groundings")) (x._1 :+ atom, x._2, x._3, x._4, x._5, x._6)
      else if (atom.startsWith("exmplCount")) (x._1, x._2 :+ atom, x._3, x._4, x._5, x._6)
      else if (atom.startsWith("false_clause") || atom.startsWith("true_clause"))  (x._1, x._2, x._3 :+ atom, x._4, x._5,x._6)
      else if (atom.startsWith("annotation"))  (x._1, x._2, x._3, x._4 :+ atom, x._5,x._6)
      else if (atom.startsWith("fns_terminated"))  (x._1, x._2, x._3, x._4, x._5 :+ atom, x._6)
      else if (atom.startsWith("tps_terminated"))  (x._1, x._2, x._3, x._4, x._5, x._6 :+ atom)
      else throw new RuntimeException(s"Unexpected atom $atom")
    }


    if (totalExmplCount_.length !=1)
      throw new RuntimeException(s"Problem with the total example count (mun of target predicate groundings): ${totalExmplCount_.mkString(" ")}")

    val totalExmplCount = totalExmplCount_.head.split("\\(")(1).split("\\)")(0).toInt

    val trueGroundingsPerClause = {
      val pairs = trueGroundingsAtoms map { atom =>
        val s = atom.split(",")
        val id = s(0).split("\\(")(1).split("_")(1).toInt
        val count = s(1).split("\\)")(0).toInt
        (id, count)
      }
      pairs.toList.groupBy(x => x._1).map{ case (k, v) => (k, v.map(_._2)) }
    }

    val annotation = annotation_.map{ x =>
      val a = Literal.parseWPB2(x).terms.head.asInstanceOf[Literal]
      val derivedFromClause = a.terms.last.name.split("_")(1).toInt
      val b = Literal(predSymbol = a.predSymbol, terms = a.terms, derivedFrom = derivedFromClause)
      Literal.toMLNFlat(b)
    }

    /* This is the same as annotation, factor it out in a function! */
    val incorrectlyTerminated = fnsTerminated_.map { x =>
      val a = Literal.parseWPB2(x).terms.head.asInstanceOf[Literal]
      val derivedFromClause = a.terms.last.name.split("_")(1).toInt
      val b = Literal(predSymbol = a.predSymbol, terms = a.terms, derivedFrom = derivedFromClause)
      Literal.toMLNFlat(b)
    }

    val correctlyNotTerminated = tpsTerminated_.map { x =>
      val a = Literal.parseWPB2(x).terms.head.asInstanceOf[Literal]
      val derivedFromClause = a.terms.last.name.split("_")(1).toInt
      val b = Literal(predSymbol = a.predSymbol, terms = a.terms, derivedFrom = derivedFromClause)
      Literal.toMLNFlat(b)
    }

    // There are two rules that produce the true groundings counts, see the ground-initiated.lp bk file.
    // However the constraint below is problematic. There are cases where both rules that produce
    // groundings for a rule have the same number of groundings (e.g. they both return zero
    // groundings, so we have something like true_groundings(ruleId_5, 0), true_groundings(ruleId_5, 0)
    // which collapse to the same atom). For instance, consider a batch with no annotation (so
    // actually_initiated is never true) and a rule that fires almost always (e.g. a rule with a
    // "not happensAt(disappear) in the body"). Then no true groundings of this rule are produced.
    /*
    if (trueGroundingsPerClause.values.exists(_.length != 2))
      throw new RuntimeException("There is a potential problem with the true counts per clause returned by Clingo!")
    */
    val groundNetwork = trueFalseGrndClauseAtoms map { x =>
      //val p = Literal.parse(x)
      val p = Literal.parseWPB2(x) // much faster than parser combinators.

      if (p.predSymbol != "false_clause" && p.predSymbol != "true_clause") throw new RuntimeException(s"Don't know what to do with this atom: $x")
      val clauseId = p.terms.head.name.split("_")(1).toInt
      val clauseHead = p.terms(1).asInstanceOf[Literal]

      if (p.predSymbol == "true_clause") {
        val l = Literal(predSymbol = clauseHead.predSymbol, terms = clauseHead.terms :+ p.terms.head, derivedFrom = clauseId)
        Literal.toMLNFlat(l)
      }
      else {
        val l = Literal(predSymbol = clauseHead.predSymbol, terms = clauseHead.terms :+ p.terms.head, isNAF = true, derivedFrom = clauseId)
        Literal.toMLNFlat(l)
      }

    }

    (groundNetwork, trueGroundingsPerClause, totalExmplCount, annotation, incorrectlyTerminated, correctlyNotTerminated)

  }



  def getMRFAsStringSet(mrf: MRF): scala.collection.mutable.Set[String] = {

    import lomrf.mln.model.AtomIdentityFunction._

    val constraintIterator = mrf.constraints.iterator()
    var groundClausesSet = scala.collection.mutable.Set.empty[String]

    while (constraintIterator.hasNext) {
      constraintIterator.advance()
      val currentConstraint = constraintIterator.value()


      groundClausesSet += currentConstraint.literals.map { lit => decodeLiteral(lit)(mrf.mln).getOrElse(throw new RuntimeException(s"Fuck this shit"))
      }.mkString(" v ")
    }
    groundClausesSet
  }


  def getStatistics(mlnInfo: MLNInfoContainer, inferredAtoms: Set[String]) = {

    val aspLabelAtoms = mlnInfo.labelAtoms
    val fluentsMap = mlnInfo.fluentsMap
    val mln = mlnInfo.mln
    val clauseIds = mlnInfo.clauseIds

    // convert each atom to MLN format to make string comparisons with the label atoms.
    val labelAtoms = aspLabelAtoms.filter(!_.contains("false_")).map{ atom =>

      // A label atom is of the form initiatedAt(meeting(b,a), 4231, ruleId_5)
      // so we need to replace the function term with the corresponding constant and
      // capitalize everything else to get the MLN format. This is generally applicable,
      // since a label atom is an initiatedAt/2 (or terminatedAt/2) atom whose first term
      // is an compound (a fluent function term), its second term is time and its third term
      // is the ruleId term.
      val parsed = Literal.parse(atom)
      val fluentSignatureAtom = fluentsMap.keySet.find(x => atom.contains(x)).getOrElse(
        throw new RuntimeException(s"Cannot find mapping for atom $atom in events map: $fluentsMap"))
      val mlnFluentConstant = fluentsMap(fluentSignatureAtom)
      s"${parsed.predSymbol.capitalize}($mlnFluentConstant,${parsed.terms(1).name.capitalize},${parsed.terms(2).name.capitalize})"
    }

    val stats = getClauseStatistics(clauseIds, mln, inferredAtoms, labelAtoms)

    stats
  }

  def performInference(mrf: MRF): Set[String] = {
    // Get inferred atoms from the MRF, but remove all the false ones (not needed).
    // An inferred atom (inferred as true) is of the form
    // InitiatedAt(Meeting_A_B,4251,RuleId_7) 1
    val inferredAtoms = ASP2MLN.atoms2TXT(mrf) map ( x => x.split(" ") ) filter ( x => x(1) == "1") map (x => x(0))
    inferredAtoms
  }

  def learnWeights(mln: MLN, annotationDB: EvidenceDB, step: Int, postLearning: Boolean = false) = {

    //println("Before")
    //mln.clauses.foreach { x => println(x.toText(), x.weight) }

    //val mrf = adagrad2(mln, annotationDB, "recall", postLearning)


    //val mrf = cda(mln, annotationDB, step, postLearning)

    val mrf = adagrad1(mln, annotationDB, postLearning)

    //println("After")
    //mln.clauses.foreach { x => println(x.toText(), x.weight) }

    mrf
  }


  def getClauseStatistics(clauseIds: List[Int], mln: MLN, inferredAtoms: Set[String], labelAtoms: Set[String]) = {

    val isNumber = (s: String) => {
      try {
        s.toDouble
        true
      } catch {
        case _ : NumberFormatException => false
      }
    }

    val hasWeight = (c: String) => {
      val split = c.split(" ")(0)
      isNumber(split)
    }

    val out = clauseIds map { id =>
      val actualIdTerm = s"RuleId_$id)" // the end ")" is used so that e.g. "RuleId_1" is not confused with "RuleId_14"

      if (actualIdTerm == "RuleId_1)") {
        val stop = "stop"
      }

      val e = mln.clauses.filter{ clause =>
        val t = clause.toText()
        t.contains(actualIdTerm) && hasWeight(t)
      }

      //mln.clauses.map(_.toText()) foreach println

      if (e.length > 1) throw new RuntimeException(s"Found more than one weighted clauses for ruleId: $actualIdTerm")
      if (e.length < 1) throw new RuntimeException(s"Found no weighted clauses for ruleId: $actualIdTerm")

      val covers = inferredAtoms.filter(x => x.contains(actualIdTerm))
      val shouldCover = labelAtoms.filter(x => x.contains(actualIdTerm))

      val TPs_set = covers.intersect(shouldCover)
      val FPs_set = covers.diff(shouldCover)
      val FNs_set = shouldCover.diff(covers)

      val TPs = TPs_set.size
      val FPs = FPs_set.size
      val FNs = FNs_set.diff(covers).size

      ( id, (e.head.weight, e.head.subgradient, TPs, FPs, FNs) )
    }
    out.toMap
  }






}

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

package woled

import java.text.DecimalFormat

import app.runutils.{Globals, RunningOptions}
import logic.Examples.Example
import logic.{Clause, Constant, Literal, LogicUtils, Theory, Variable}
import lomrf.logic.compile.{NormalForm, PredicateCompletion, PredicateCompletionMode}
import lomrf.logic.{AtomSignature, Constant, EvidenceAtom, FunctionMapping}
import lomrf.logic.parser.KBParser
import lomrf.mln.grounding.MRFBuilder
import lomrf.mln.inference.ILP
import lomrf.mln.learning.structure.ClauseConstructor
import lomrf.mln.model.{AtomIdentityFunction, KB, MLN}
import utils.{ASP, Utils}
import lomrf.mln.model.AtomIdentityFunctionOps._
import lomrf.mln.model.builders.{ConstantsDomainBuilder, EvidenceBuilder}

import scala.collection.mutable
import scala.io.Source

object WoledUtils {

  def evalOnTestSet(testData: Iterator[Example], rules: List[Clause], inps: RunningOptions) = {

    println("Evaluating on the test set...")

    var totalTPs = 0
    var totalFPs = 0
    var totalFNs = 0

    testData foreach { batch =>
      val program = {
        val nar = batch.narrative.map(_ + ".").mkString("\n")
        val include = s"""#include "${inps.globals.BK_WHOLE_EC}"."""
        val show = inps.globals.bodyAtomSignatures.map(x => s"#show ${x.tostring}.").mkString("\n")
        Vector(nar, include, show)
      }
      // This transforms the actual data into an MLN-compatible form.
      val f = woled.Utils.dumpToFile(program)
      val t = ASP.solve(task         = Globals.INFERENCE, aspInputFile = f)
      val answer = t.head.atoms
      val e = new Example(annot = batch.annotation, nar = answer, _time = batch.time)

      val inferredState = WoledUtils.getInferredState(Theory.compressTheory(rules), e, Vector.empty[Literal], "MAP", inps)
      val inferredTrue = inferredState.filter(x => x._2 && x._1.startsWith("holdsAt")).keySet
      val actuallyTrue = e.annotation.toSet
      val tps = inferredTrue.intersect(actuallyTrue).size
      val fps = inferredTrue.diff(actuallyTrue).size
      val fns = actuallyTrue.diff(inferredTrue).size
      println(s"TPs, FPs, FNs: $tps, $fps, $fns")
      totalTPs += tps
      totalFPs += fps
      totalFNs += fns
    }

    val precision = totalTPs.toDouble / (totalTPs + totalFPs)
    val recall = totalTPs.toDouble / (totalTPs + totalFNs)
    val f1 = 2 * (precision * recall) / (precision + recall)
    println(s"F1-score on test set: $f1")
  }

  def getInferredState(rules: List[Clause], e: Example, inertiaAtoms: Vector[Literal], mode: String, inps: RunningOptions) = {

    // Other inference modes will be added here, e.g. WM (experts) and crisp (OLED)
    if (mode == "MAP") {
      MAPInference(rules, e, inertiaAtoms, inps)
    } else {
      Map[String, Boolean]()
    }

  }

  def format(x: Double) = {
    val defaultNumFormat = new DecimalFormat("0.############")
    defaultNumFormat.format(x)
  }

  def MAPInference(rules: List[Clause], e: Example, inertiaAtoms: Vector[Literal], inps: RunningOptions) = {

    val queryAtoms = Set(
      AtomSignature("HoldsAt", 2),
      AtomSignature("InitiatedAt", 2),
      AtomSignature("TerminatedAt", 2)
    )

    /* Get BK etc */
    val mlnBKFile = s"${inps.entryPath}/MAPInferenceBK.mln"

    val (kb, constants) = KB.fromFile(mlnBKFile)
    val formulas = kb.formulas

    val parser = new KBParser(kb.predicateSchema.map { case (x, y) => x -> y.toVector }, kb.functionSchema)

    /* Input definitive clauses, whose structure is learnt over time */
    val definiteClauses = rules.map { rule =>
      val head = Literal.toMLNClauseLiteral(rule.head.asLiteral).tostringMLN
      val body = rule.body.map(Literal.toMLNClauseLiteral(_).tostringMLN).mkString(" ^ ")
      parser.parseDefiniteClause(s"${format(rule.weight)} $head :- $body")
    }

    /* Read the definite clauses from the BK file. FOR DEBUGGING */
    //val definiteClauses = kb.definiteClauses

    val ee = new Example(annot = e.annotation, nar = e.narrative ++ inertiaAtoms.map(x => x.tostring), _time = e.time)

    val (functionMappings, mlnEvidenceAtoms, mlmConstsToAspAtomsMap) = getFunctionMappings(ee, inps.globals.BK_WHOLE_EC)

    // Adding constants
    val const = ConstantsDomainBuilder.from(constants)

    for ((_, (returnValue, symbol)) <- functionMappings) {
      val splitFunction = symbol.split(",").toVector
      val functionSymbol = splitFunction.head
      val args = splitFunction.tail
      val (returnValueDomain, argDomains) = kb.functionSchema(AtomSignature(functionSymbol, args.length))
      const += returnValueDomain -> returnValue
      argDomains.zip(args).foreach { case (domain, value) => const += domain -> value }
    }

    for (atom <- mlnEvidenceAtoms) {
      val args = atom.terms.map(x => lomrf.logic.Constant(x.tostring)).toVector
      val domains = kb.predicateSchema(AtomSignature(atom.predSymbol, args.length))
      domains.zip(args).foreach { case (domain, value) => const += domain -> value.symbol }
    }

    /* For CAVIAR fragment */
    /*const += "dist" -> "34"
    const += "dist" -> "30"
    const += "dist" -> "25"
    const += "dist" -> "24"
    const += "event" -> "Inactive_A"
    const += "event" -> "Inactive_B"
    const += "event" -> "Walking_A"
    const += "event" -> "Walking_Β"
    const += "event" -> "Active_A"
    const += "event" -> "Active_Β"
    const += "event" -> "Disappear_A"
    const += "event" -> "Disappear_Β"
    const += "event" -> "Appear_A"
    const += "event" -> "Appear_Β"
    const += "event" -> "Abrupt_A"
    const += "event" -> "Abrupt_Β"
    const += "event" -> "Running_A"
    const += "event" -> "Running_A"*/

    /* For the entire CAVIAR */
    /*const += "event" -> "Inactive_Id0"
    const += "event" -> "Inactive_Id4"
    const += "event" -> "Inactive_Id1"
    const += "event" -> "Inactive_Id5"
    const += "event" -> "Inactive_Id9"
    const += "event" -> "Inactive_Id2"
    const += "event" -> "Inactive_Id8"
    const += "event" -> "Inactive_Id6"
    const += "event" -> "Inactive_Id3"
    const += "event" -> "Inactive_Id7"
    const += "event" -> "Active_Id6"
    const += "event" -> "Active_Id3"
    const += "event" -> "Active_Id7"
    const += "event" -> "Active_Id4"
    const += "event" -> "Active_Id5"
    const += "event" -> "Active_Id0"
    const += "event" -> "Active_Id1"
    const += "event" -> "Active_Id9"
    const += "event" -> "Active_Id8"
    const += "event" -> "Active_Id2"
    const += "event" -> "Walking_Id4"
    const += "event" -> "Walking_Id5"
    const += "event" -> "Walking_Id9"
    const += "event" -> "Walking_Id6"
    const += "event" -> "Walking_Id7"
    const += "event" -> "Walking_Id2"
    const += "event" -> "Walking_Id1"
    const += "event" -> "Walking_Id3"
    const += "event" -> "Walking_Id8"
    const += "event" -> "Walking_Id0"
    const += "event" -> "Abrupt_Id6"
    const += "event" -> "Abrupt_Id5"
    const += "event" -> "Abrupt_Id0"
    const += "event" -> "Abrupt_Id9"
    const += "event" -> "Abrupt_Id4"
    const += "event" -> "Abrupt_Id1"
    const += "event" -> "Abrupt_Id8"
    const += "event" -> "Abrupt_Id2"
    const += "event" -> "Abrupt_Id3"
    const += "event" -> "Abrupt_Id7"
    const += "event" -> "Running_Id7"
    const += "event" -> "Running_Id0"
    const += "event" -> "Running_Id4"
    const += "event" -> "Running_Id6"
    const += "event" -> "Running_Id1"
    const += "event" -> "Running_Id5"
    const += "event" -> "Running_Id9"
    const += "event" -> "Running_Id2"
    const += "event" -> "Running_Id8"
    const += "event" -> "Running_Id3"
    const += "event" -> "Appear_Id0"
    const += "event" -> "Appear_Id4"
    const += "event" -> "Appear_Id3"
    const += "event" -> "Appear_Id6"
    const += "event" -> "Appear_Id8"
    const += "event" -> "Appear_Id2"
    const += "event" -> "Appear_Id9"
    const += "event" -> "Appear_Id7"
    const += "event" -> "Appear_Id1"
    const += "event" -> "Appear_Id5"
    const += "event" -> "Disappear_Id1"
    const += "event" -> "Disappear_Id5"
    const += "event" -> "Disappear_Id9"
    const += "event" -> "Disappear_Id4"
    const += "event" -> "Disappear_Id0"
    const += "event" -> "Disappear_Id8"
    const += "event" -> "Disappear_Id7"
    const += "event" -> "Disappear_Id3"
    const += "event" -> "Disappear_Id6"
    const += "event" -> "Disappear_Id2"*/

    /*val domains = const.result()
    domains.foreach { case (name, set) =>
      val constants = set.iterator
      println(s"$name: [${constants.mkString(",")}]")
    }*/

    val evidenceBuilder = EvidenceBuilder(kb.predicateSchema, kb.functionSchema, queryAtoms, Set.empty, const.result())
    //val evidenceBuilder = EvidenceBuilder(kb.predicateSchema, kb.functionSchema, queryAtoms, Set.empty, domains)

    for (entry <- functionMappings) {
      val functionReturnConstant = entry._2._1
      val functionStr = entry._2._2
      val splitFunction = functionStr.split(",").toList
      val functionSymbol = splitFunction.head
      val functionArgs = splitFunction.tail.toVector
      evidenceBuilder.functions += new FunctionMapping(functionReturnConstant, functionSymbol, functionArgs)
    }

    /* For now we need these hard-coded. It's a LoMRF limitation not being able to have them dynamically*/
    // This is for CAVIAR fragment
    /*evidenceBuilder.functions += new FunctionMapping("Inactive_A", "inactive", Vector("A"))
    evidenceBuilder.functions += new FunctionMapping("Inactive_B", "inactive", Vector("B"))
    evidenceBuilder.functions += new FunctionMapping("Walking_A", "walking", Vector("A"))
    evidenceBuilder.functions += new FunctionMapping("Walking_B", "walking", Vector("B"))
    evidenceBuilder.functions += new FunctionMapping("Active_A", "active", Vector("A"))
    evidenceBuilder.functions += new FunctionMapping("Active_B", "active", Vector("B"))
    evidenceBuilder.functions += new FunctionMapping("Disappear_A", "disappear", Vector("A"))
    evidenceBuilder.functions += new FunctionMapping("Disappear_B", "disappear", Vector("B"))
    evidenceBuilder.functions += new FunctionMapping("Appear_A", "appear", Vector("A"))
    evidenceBuilder.functions += new FunctionMapping("Appear_B", "appear", Vector("B"))
    evidenceBuilder.functions += new FunctionMapping("Abrupt_A", "abrupt", Vector("A"))
    evidenceBuilder.functions += new FunctionMapping("Abrupt_B", "abrupt", Vector("B"))
    evidenceBuilder.functions += new FunctionMapping("Running_A", "running", Vector("A"))
    evidenceBuilder.functions += new FunctionMapping("Running_B", "running", Vector("B"))*/

    // This is for the entire CAVIAR

    evidenceBuilder.functions += new FunctionMapping("ADSANdlj", "active", Vector("ref"))
    evidenceBuilder.functions += new FunctionMapping("ADSANdlj", "appear", Vector("ref"))
    evidenceBuilder.functions += new FunctionMapping("ADSANdlj", "inactive", Vector("ref"))
    evidenceBuilder.functions += new FunctionMapping("ADSANdlj", "walking", Vector("ref"))
    evidenceBuilder.functions += new FunctionMapping("ADSANdlj", "abrupt", Vector("ref"))
    evidenceBuilder.functions += new FunctionMapping("ADSANdlj", "running", Vector("ref"))
    evidenceBuilder.functions += new FunctionMapping("ADSANdlj", "disappear", Vector("ref"))

    /*evidenceBuilder.functions += new FunctionMapping("Inactive_Id0", "inactive", Vector("Id0"))
    evidenceBuilder.functions += new FunctionMapping("Inactive_Id4", "inactive", Vector("Id4"))
    evidenceBuilder.functions += new FunctionMapping("Inactive_Id1", "inactive", Vector("Id1"))
    evidenceBuilder.functions += new FunctionMapping("Inactive_Id5", "inactive", Vector("Id5"))
    evidenceBuilder.functions += new FunctionMapping("Inactive_Id9", "inactive", Vector("Id9"))
    evidenceBuilder.functions += new FunctionMapping("Inactive_Id2", "inactive", Vector("Id2"))
    evidenceBuilder.functions += new FunctionMapping("Inactive_Id8", "inactive", Vector("Id8"))
    evidenceBuilder.functions += new FunctionMapping("Inactive_Id6", "inactive", Vector("Id6"))
    evidenceBuilder.functions += new FunctionMapping("Inactive_Id3", "inactive", Vector("Id3"))
    evidenceBuilder.functions += new FunctionMapping("Inactive_Id7", "inactive", Vector("Id7"))
    evidenceBuilder.functions += new FunctionMapping("Active_Id6", "active", Vector("Id6"))
    evidenceBuilder.functions += new FunctionMapping("Active_Id3", "active", Vector("Id3"))
    evidenceBuilder.functions += new FunctionMapping("Active_Id7", "active", Vector("Id7"))
    evidenceBuilder.functions += new FunctionMapping("Active_Id4", "active", Vector("Id4"))
    evidenceBuilder.functions += new FunctionMapping("Active_Id5", "active", Vector("Id5"))
    evidenceBuilder.functions += new FunctionMapping("Active_Id0", "active", Vector("Id0"))
    evidenceBuilder.functions += new FunctionMapping("Active_Id1", "active", Vector("Id1"))
    evidenceBuilder.functions += new FunctionMapping("Active_Id9", "active", Vector("Id9"))
    evidenceBuilder.functions += new FunctionMapping("Active_Id8", "active", Vector("Id8"))
    evidenceBuilder.functions += new FunctionMapping("Active_Id2", "active", Vector("Id2"))
    evidenceBuilder.functions += new FunctionMapping("Walking_Id4", "walking", Vector("Id4"))
    evidenceBuilder.functions += new FunctionMapping("Walking_Id5", "walking", Vector("Id5"))
    evidenceBuilder.functions += new FunctionMapping("Walking_Id9", "walking", Vector("Id9"))
    evidenceBuilder.functions += new FunctionMapping("Walking_Id6", "walking", Vector("Id6"))
    evidenceBuilder.functions += new FunctionMapping("Walking_Id7", "walking", Vector("Id7"))
    evidenceBuilder.functions += new FunctionMapping("Walking_Id2", "walking", Vector("Id2"))
    evidenceBuilder.functions += new FunctionMapping("Walking_Id1", "walking", Vector("Id1"))
    evidenceBuilder.functions += new FunctionMapping("Walking_Id3", "walking", Vector("Id3"))
    evidenceBuilder.functions += new FunctionMapping("Walking_Id8", "walking", Vector("Id8"))
    evidenceBuilder.functions += new FunctionMapping("Walking_Id0", "walking", Vector("Id0"))
    evidenceBuilder.functions += new FunctionMapping("Abrupt_Id6", "abrupt", Vector("Id6"))
    evidenceBuilder.functions += new FunctionMapping("Abrupt_Id5", "abrupt", Vector("Id5"))
    evidenceBuilder.functions += new FunctionMapping("Abrupt_Id0", "abrupt", Vector("Id0"))
    evidenceBuilder.functions += new FunctionMapping("Abrupt_Id9", "abrupt", Vector("Id9"))
    evidenceBuilder.functions += new FunctionMapping("Abrupt_Id4", "abrupt", Vector("Id4"))
    evidenceBuilder.functions += new FunctionMapping("Abrupt_Id1", "abrupt", Vector("Id1"))
    evidenceBuilder.functions += new FunctionMapping("Abrupt_Id8", "abrupt", Vector("Id8"))
    evidenceBuilder.functions += new FunctionMapping("Abrupt_Id2", "abrupt", Vector("Id2"))
    evidenceBuilder.functions += new FunctionMapping("Abrupt_Id3", "abrupt", Vector("Id3"))
    evidenceBuilder.functions += new FunctionMapping("Abrupt_Id7", "abrupt", Vector("Id7"))
    evidenceBuilder.functions += new FunctionMapping("Running_Id7", "running", Vector("Id7"))
    evidenceBuilder.functions += new FunctionMapping("Running_Id0", "running", Vector("Id0"))
    evidenceBuilder.functions += new FunctionMapping("Running_Id4", "running", Vector("Id4"))
    evidenceBuilder.functions += new FunctionMapping("Running_Id6", "running", Vector("Id6"))
    evidenceBuilder.functions += new FunctionMapping("Running_Id1", "running", Vector("Id1"))
    evidenceBuilder.functions += new FunctionMapping("Running_Id5", "running", Vector("Id5"))
    evidenceBuilder.functions += new FunctionMapping("Running_Id9", "running", Vector("Id9"))
    evidenceBuilder.functions += new FunctionMapping("Running_Id2", "running", Vector("Id2"))
    evidenceBuilder.functions += new FunctionMapping("Running_Id8", "running", Vector("Id8"))
    evidenceBuilder.functions += new FunctionMapping("Running_Id3", "running", Vector("Id3"))
    evidenceBuilder.functions += new FunctionMapping("Appear_Id0", "appear", Vector("Id0"))
    evidenceBuilder.functions += new FunctionMapping("Appear_Id4", "appear", Vector("Id4"))
    evidenceBuilder.functions += new FunctionMapping("Appear_Id3", "appear", Vector("Id3"))
    evidenceBuilder.functions += new FunctionMapping("Appear_Id6", "appear", Vector("Id6"))
    evidenceBuilder.functions += new FunctionMapping("Appear_Id8", "appear", Vector("Id8"))
    evidenceBuilder.functions += new FunctionMapping("Appear_Id2", "appear", Vector("Id2"))
    evidenceBuilder.functions += new FunctionMapping("Appear_Id9", "appear", Vector("Id9"))
    evidenceBuilder.functions += new FunctionMapping("Appear_Id7", "appear", Vector("Id7"))
    evidenceBuilder.functions += new FunctionMapping("Appear_Id1", "appear", Vector("Id1"))
    evidenceBuilder.functions += new FunctionMapping("Appear_Id5", "appear", Vector("Id5"))
    evidenceBuilder.functions += new FunctionMapping("Disappear_Id1", "disappear", Vector("Id1"))
    evidenceBuilder.functions += new FunctionMapping("Disappear_Id5", "disappear", Vector("Id5"))
    evidenceBuilder.functions += new FunctionMapping("Disappear_Id9", "disappear", Vector("Id9"))
    evidenceBuilder.functions += new FunctionMapping("Disappear_Id4", "disappear", Vector("Id4"))
    evidenceBuilder.functions += new FunctionMapping("Disappear_Id0", "disappear", Vector("Id0"))
    evidenceBuilder.functions += new FunctionMapping("Disappear_Id8", "disappear", Vector("Id8"))
    evidenceBuilder.functions += new FunctionMapping("Disappear_Id7", "disappear", Vector("Id7"))
    evidenceBuilder.functions += new FunctionMapping("Disappear_Id3", "disappear", Vector("Id3"))
    evidenceBuilder.functions += new FunctionMapping("Disappear_Id6", "disappear", Vector("Id6"))
    evidenceBuilder.functions += new FunctionMapping("Disappear_Id2", "disappear", Vector("Id2"))*/

    for (atom <- mlnEvidenceAtoms) {
      val predicate = atom.predSymbol
      val args = atom.terms.map(x => lomrf.logic.Constant(x.tostring)).toVector
      evidenceBuilder.evidence += EvidenceAtom.asTrue(predicate, args)
    }

    for (atom <- inertiaAtoms) {
      val predicate = atom.predSymbol.capitalize
      val fluent = atom.terms.head.asInstanceOf[Literal]
      val fluentConst = s"${fluent.predSymbol.capitalize}_${fluent.terms.map(x => x.tostring.capitalize).mkString("_")}"
      if (!mlmConstsToAspAtomsMap.keySet.contains(fluentConst)) mlmConstsToAspAtomsMap(fluentConst) = fluent.tostring
      val timeConst = atom.terms.tail.head.tostring
      val args = Vector(fluentConst, timeConst).map(x => lomrf.logic.Constant(x)).toVector
      evidenceBuilder.evidence += EvidenceAtom.asTrue(predicate, args)
    }

    val evidence = evidenceBuilder.result()

    /*evidence.db.foreach { case (signature, aedb) =>
      println(s"Evidence for atom signature $signature")
      println("Summary:\n" + aedb)
      for (id <- aedb.identity.indices) {
        val args = aedb.identity.decode(id).get
        val tv = aedb(id)
        println(s"${signature.symbol}(${args.mkString(",")}) = $tv")
      }
    }*/

    println("  Predicate completion...")
    val resultedFormulas = PredicateCompletion(formulas, definiteClauses.toSet, PredicateCompletionMode.Decomposed)(kb.predicateSchema, kb.functionSchema, constants)
    val cnf = NormalForm.compileFastCNF(resultedFormulas)(constants).toVector

    // This prints out the lifted rules in CNF form.
    //println(cnf.map(_.toText()).mkString("\n"))

    val mln = MLN(kb.schema, evidence, queryAtoms, cnf)
    val builder = new MRFBuilder(mln, createDependencyMap = true)
    val mrf = builder.buildNetwork

    /* FOR DEBUGGING (print out the ground program) */
    /*val constraints = mrf.constraints.iterator()
    while (constraints.hasNext) {
      constraints.advance()
      val constraint = constraints.value()
      println(constraint.decodeFeature(10000)(mln))
    }*/

    val solver = ILP(mrf)
    //solver.infer()

    println("    Calling the solver...")
    val s = solver.infer

    var result = Map.empty[String, Boolean]

    val it = s.mrf.atoms.iterator()
    while (it.hasNext) {
      it.advance()
      val a = it.value()
      val atom = a.id.decodeAtom(mln).get
      val state = a.getState
      // keep only inferred as true atoms, get the rest via CWA.
      //if (state) result += atom -> state //result += atom -> state
      result += atom -> state
    }

    val resultToASP = inferredStateToASP(result, mlmConstsToAspAtomsMap)
    resultToASP //result
  }

  def inferredStateToASP(mapState: Map[String, Boolean], mlmConstsToAspAtomsMap: mutable.Map[String, String]) = {
    mapState.map { case (mlnAtom, truthValue) =>
      val _mlnAtom = {
        val (head, tail) = (mlnAtom.head, mlnAtom.tail)
        head.toString.toLowerCase() + tail
      }
      val aspAtom = mlmConstsToAspAtomsMap.foldLeft(_mlnAtom) { (x, y) =>
        x.replaceAll(y._1, y._2)
      }
      aspAtom -> truthValue
    }
  }

  /*
  This method extracts function mappings from the current batch, stuff like
  Running_ID0 = running(ID0)
  Enter_ID0 = enter(ID0)
  Meeting_ID0_ID0 = meeting(ID0, ID0)...
  It also converts ASP evidence atoms to MLN evidence atoms and
  generates next/2 instances for LoMRF. It needs to extract fluent/1, event/1 and next/2 signatures form the batch data.
 */
  def getFunctionMappings(exmpl: Example, bkFile: String) = {

    var functionMappings = scala.collection.mutable.Map[String, (String, String)]()

    val additonalDirectives = s"event(X) :- happensAt(X,_).\n#show.\n#show event/1.\n#show fluent_all/1.#show next/2."
    val source = Source.fromFile(bkFile)
    var bk = source.getLines().toList
    source.close()
    bk = bk :+ additonalDirectives

    val all = (exmpl.annotationASP ++ exmpl.narrativeASP ++ bk)

    val file = utils.Utils.getTempFile("function-mappings", ".lp")
    utils.Utils.writeLine(all.mkString("\n"), file.getCanonicalPath, "overwrite")
    val stuff = ASP.solve(task         = Globals.INFERENCE, aspInputFile = file).head.atoms

    val (fluents, events, nextAtoms) = stuff.foldLeft(List[String](), List[String](), List[String]()) { (x, y) =>
      if (y.startsWith("fluent")) (x._1 :+ y, x._2, x._3)
      else if (y.startsWith("event")) (x._1, x._2 :+ y, x._3)
      else if (y.startsWith("next")) (x._1, x._2, x._3 :+ y)
      else throw new RuntimeException(s"Unexpected input: $y")
    }

    // Populate the function mapping map.
    (fluents ++ events) foreach { x =>
      val parsed = Literal.parse(x)
      val atom = parsed.terms.head
      val atomStr = atom.tostring
      val functor = atom.asInstanceOf[Literal].predSymbol
      val args = atom.asInstanceOf[Literal].terms

      // This is the term that represents the MLN constant (function value) that is generated from this atom.
      // For instance, given the atom meeting(id0,id2), the corresponding constant term is Meeting_Id0_Id2
      val constantTerm = s"${functor.capitalize}_${args.map(_.tostring.capitalize).mkString("_")}"

      // This represent the MLN function that correspond to this term.
      // For instance, given the atom meeting(id0,id2), the corresponding function term is meeting(Id0,Id2)
      // This is represented by the string "meeting,Id0,Id2", so that the function symbol and the arguments may
      // be easily extracted by splitting with "," at the MAPInference and generate the input for populating the
      // functions of the evidence builder object.
      val functionTerm = s"$functor,${args.map(_.tostring.capitalize).mkString(",")}"

      if (!functionMappings.keySet.contains(atomStr)) functionMappings += atomStr -> (constantTerm, functionTerm)
    }
    file.delete()

    // These are used to match the terms in the atoms of the MAP-inferred state and facilitate the conversion to ASP form.
    // It is a map of the form
    // Meeting_ID0_ID2 -> meeting(id0,id2)
    val MLNConstantsToASPAtomsMap = functionMappings.map{ case (k, v) => v._1 -> k }

    // Convert ASP atoms to MLN representation.
    val MLNEvidenceAtoms = (exmpl.narrative ++ nextAtoms).map { x =>
      val parsed = Literal.parse(x)
      Literal.toMLNFlat(parsed)
    }
    (functionMappings, MLNEvidenceAtoms, MLNConstantsToASPAtomsMap)
  }

}

package woled.trialcode

import app.runutils.{Globals, RunningOptions}
import logic.Examples.Example
import logic.{Clause, Literal}
import lomrf.logic.{AtomSignature, Constant, EvidenceAtom, FunctionMapping, PredicateCompletion, PredicateCompletionMode}
import lomrf.logic.parser.KBParser
import lomrf.mln.grounding.MRFBuilder
import lomrf.mln.learning.structure.ClauseConstructor
import lomrf.mln.model.{ConstantsDomainBuilder, EvidenceBuilder, KB, MLN}
import oled.weightlearn.LoMRFTest.{annotationBuilder, constantsDomain, queryPredicates}
import utils.ASP
import woled.WoledUtils
import woled.WoledUtils.{format, getFunctionMappings}

import scala.io.Source

/**
 * Created by nkatz at 12/10/19
 */

/* Test code for weight learning with a fixed theory and predicate elimination. Learn a weighted CNF theory. */

object WLearnPredElimination {

  def learnWeights(e: Example, inps: RunningOptions) = {

    val queryAtoms = Set(AtomSignature("HoldsAt", 2))
    val mlnBKFile = "/home/nkatz/dev/BKExamples/BK-various-taks/DevTest/caviar-bk/meeting/MAPInferenceBK.mln"
    val (kb, constants) = KB.fromFile(mlnBKFile)
    val formulas = kb.formulas
    val parser = new KBParser(kb.predicateSchema.map { case (x, y) => x -> y.toVector }, kb.functionSchema)

    val source = Source.fromFile("/home/nkatz/dev/BKExamples/BK-various-taks/WeightLearning/Caviar/fragment/meeting/ASP/asp-rules-test")
    val list = source.getLines
    val rules = list.map(x => Clause.parse(x)).toList
    source.close

    val definiteClauses = rules.map { rule =>
      val head = Literal.toMLNClauseLiteral(rule.head.asLiteral).tostringMLN
      val body = rule.body.map(Literal.toMLNClauseLiteral(_).tostringMLN).mkString(" ^ ")
      parser.parseDefiniteClause(s"${format(rule.weight)} $head :- $body")
    }

    val (functionMappings, mlnEvidenceAtoms, mlmConstsToAspAtomsMap) = getFunctionMappings(e, inps.globals.BK_WHOLE_EC)

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

    /* For the entire CAVIAR */
    const += "event" -> "Inactive_Id0"
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
    const += "event" -> "Disappear_Id2"

    val evidenceBuilder = EvidenceBuilder(kb.predicateSchema, kb.functionSchema, queryAtoms, Set.empty, const.result())

    for (entry <- functionMappings) {
      val functionReturnConstant = entry._2._1
      val functionStr = entry._2._2
      val splitFunction = functionStr.split(",").toList
      val functionSymbol = splitFunction.head
      val functionArgs = splitFunction.tail.toVector
      evidenceBuilder.functions += new FunctionMapping(functionReturnConstant, functionSymbol, functionArgs)
    }

    // This is for the entire CAVIAR
    evidenceBuilder.functions += new FunctionMapping("Inactive_Id0", "inactive", Vector("Id0"))
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
    evidenceBuilder.functions += new FunctionMapping("Disappear_Id2", "disappear", Vector("Id2"))

    for (atom <- mlnEvidenceAtoms) {
      val predicate = atom.predSymbol
      val args = atom.terms.map(x => lomrf.logic.Constant(x.tostring)).toVector
      evidenceBuilder.evidence += EvidenceAtom.asTrue(predicate, args)
    }

    val evidence = evidenceBuilder.result()

    val resultedFormulas = PredicateCompletion(formulas, definiteClauses.toSet, PredicateCompletionMode.Simplification)(kb.predicateSchema, kb.functionSchema, constants)
    val cnf = ClauseConstructor.makeCNF(resultedFormulas)(constants).toVector
    val mln = MLN(kb.schema, evidence, queryAtoms, cnf)
    val builder = new MRFBuilder(mln, createDependencyMap = true)
    val mrf = builder.buildNetwork

    val annotationSchema = Map(AtomSignature("HoldsAt", 2) -> Vector("fluent", "time"))
    val annotationBuilder = EvidenceBuilder(annotationSchema, queryAtoms, Set.empty, const.result())

    /* Get the annotation */

    val program = {
      val annotation = e.annotation.map(x => s"true($e).").mkString(" ")
      val rule1 = s"annotationAtom(holdsAt(meeting(X,Y),T),true) :- person(X), person(Y), time(T), true(holdsAt(meeting(X,Y),T))."
      val rule2 = s"annotationAtom(holdsAt(meeting(X,Y),T),false) :- person(X), person(Y), time(T), not true(holdsAt(meeting(X,Y),T))."
      val show = s"#show.\n#show annotationAtom/2."
      Vector(annotation, rule1, rule2, show)
    }

    val f = woled.Utils.dumpToFile(program)
    val result = ASP.solve(task=Globals.INFERENCE, aspInputFile=f)
    val answer = if (result.nonEmpty) result.head.atoms else Nil

    answer foreach { atom =>
      val parsed = Literal.parse(atom)
      val (annotAtom, truthValue) = (parsed.terms.head.asInstanceOf[Literal], parsed.terms.tail.head.tostring.toBoolean)
      val predSymbol = annotAtom.predSymbol.capitalize // this is the HoldAt
      val (fluentTerm, timeTerm) = (annotAtom.terms.head.asInstanceOf[Literal], annotAtom.terms.tail.head.tostring)
      val fluentConstant = s"${fluentTerm.predSymbol.capitalize}_${fluentTerm.terms.head.tostring.capitalize}_${fluentTerm.terms.tail.head.tostring.capitalize}"
      val timeConstant = timeTerm

      if (truthValue) {
        annotationBuilder.evidence += EvidenceAtom.asTrue(predSymbol, Vector[Constant](Constant(fluentConstant), Constant(timeConstant)))
      } else {
        annotationBuilder.evidence += EvidenceAtom.asFalse(predSymbol, Vector[Constant](Constant(fluentConstant), Constant(timeConstant)))
      }
    }

    val annotationDB = annotationBuilder.result().db

  }

}

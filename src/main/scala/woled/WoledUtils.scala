package woled

import app.runutils.{Globals, RunningOptions}
import logic.Examples.Example
import logic.{Clause, Literal, LogicUtils, Theory}
import lomrf.logic.{AtomSignature, Constant, EvidenceAtom, FunctionMapping, NormalForm, PredicateCompletion, PredicateCompletionMode}
import lomrf.logic.parser.KBParser
import lomrf.mln.grounding.MRFBuilder
import lomrf.mln.inference.ILP
import lomrf.mln.model.{ConstantsDomainBuilder, EvidenceBuilder, KB, MLN}
import utils.{ASP, Utils}
import lomrf.mln.model.AtomIdentityFunctionOps._

import scala.collection.mutable
import scala.io.Source

object WoledUtils {


  def getInferredState(rules: List[Clause], e: Example, mode: String, inps: RunningOptions) = {

    // Other inference modes will be added here, e.g. WM (experts) and crisp (OLED)
    if (mode == "MAP") {
      MAPInference(rules, e, inps)
    } else {
      Map[String, Boolean]()
    }

  }

  def MAPInference(rules: List[Clause], e: Example, inps: RunningOptions) = {

    val queryAtoms = Set(
      AtomSignature("HoldsAt", 2),
      AtomSignature("InitiatedAt", 2),
      AtomSignature("TerminatedAt", 2)
    )

    // Get BK etc.
    val mlnBKFile = "/home/nkatz/dev/OLED-BK/BKExamples/BK-various-taks/WeightLearning/Caviar/fragment/meeting/MLN/MAPInferenceBK.mln"
    val (kb, constants) = KB.fromFile(mlnBKFile)
    val formulas = kb.formulas
    val parser = new KBParser(kb.predicateSchema.map { case (x, y) => x -> y.toVector }, kb.functionSchema)

    val definiteClauses = rules.map { rule =>
      val head = Literal.toMLNClauseLiteral(rule.head.asLiteral).tostring_mln
      val body = rule.body.map(Literal.toMLNClauseLiteral(_).tostring_mln).mkString(" ^ ")
      parser.parseDefiniteClause(s"${rule.mlnWeight} $head :- $body")
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
      val args = atom.terms.map(x => Constant(x.tostring)).toVector
      val domains = kb.predicateSchema(AtomSignature(atom.functor, args.length))
      domains.zip(args).foreach { case (domain, value) => const += domain -> value.symbol }
    }

    val evidenceBuilder = EvidenceBuilder(kb.predicateSchema, kb.functionSchema, queryAtoms, Set.empty, const.result())

    for (entry <- functionMappings) {
      val functionReturnConstant = entry._2._1
      val functionStr = entry._2._2
      val splitFunction = functionStr.split(",").toList
      val functionSymbol = splitFunction.head
      val functionArgs = splitFunction.tail.toVector
      //println(s"$functionReturnConstant = $functionSymbol(${functionArgs.mkString(",")})")
      evidenceBuilder.functions += new FunctionMapping(functionReturnConstant, functionSymbol, functionArgs)
    }

    /* For now we need these hard-coded. It's a LoMRF limitation not being able to have them dynamically*/
    evidenceBuilder.functions += new FunctionMapping("Inactive_A", "inactive", Vector("A"))
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

    for (atom <- mlnEvidenceAtoms) {
      val predicate = atom.functor
      val args = atom.terms.map(x => Constant(x.tostring)).toVector
      evidenceBuilder.evidence += EvidenceAtom.asTrue(predicate, args)
    }

    val evidence = evidenceBuilder.result()

    //println(evidence)

    val resultedFormulas = PredicateCompletion(formulas, definiteClauses.toSet, PredicateCompletionMode.Decomposed)(kb.predicateSchema, kb.functionSchema, constants)
    val cnf = NormalForm.compileCNF(resultedFormulas)(constants).toVector

    //cnf.map(_.toText()).foreach(println)

    val mln = MLN(kb.schema, evidence, queryAtoms, cnf)
    val builder = new MRFBuilder(mln, createDependencyMap = true)
    val mrf = builder.buildNetwork

    val solver = new ILP(mrf)
    solver.infer()

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
        // This is the way to get all atoms in the MAP-inferred state, either true of false.
        result += atomID.decodeAtom(mln).get -> state

        // Let's keep only the true atoms to reduce the size of the map
        //if (state) result += atomID.decodeAtom(mln).get -> state
      }
    }

    //result // This is a map with MLN-style atoms in the inferred state.

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

    val file = Utils.getTempFile("function-mappings", ".lp")
    Utils.writeLine(all.mkString("\n"), file.getCanonicalPath, "overwrite")
    val stuff = ASP.solve(task=Globals.INFERENCE, aspInputFile=file).head.atoms

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
      val functor = atom.asInstanceOf[Literal].functor
      val args =  atom.asInstanceOf[Literal].terms

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
    val MLNEvidenceAtoms = exmpl.narrative.map { x =>
      val parsed = Literal.parse(x)
      Literal.toMLNFlat(parsed)
    }
    (functionMappings, MLNEvidenceAtoms, MLNConstantsToASPAtomsMap)
  }



  def generateNewRules(topTheory: Theory, e: Example, globals: Globals) = {
    val bcs = generateBCs(topTheory, e, globals)
    bcs map { x =>
      val c = Clause(head=x.head, body = List())
      c.addToSupport(x)
      c
    }
  }

  def generateBCs(topTheory: Theory, e: Example, globals: Globals) = {
    val terminatedOnly = false
    val specialBKfile = globals.BK_WHOLE_EC
    val (_, varKernel) = LogicUtils.generateKernel(e.toMapASP, learningTerminatedOnly = terminatedOnly, bkFile = specialBKfile, globals=globals)
    val bottomTheory = topTheory.clauses flatMap(x => x.supportSet.clauses)
    val goodKernelRules = varKernel.filter(newBottomRule => !bottomTheory.exists(supportRule => newBottomRule.thetaSubsumes(supportRule)))
    goodKernelRules
  }

}

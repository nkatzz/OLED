package woled

import java.text.DecimalFormat

import app.runutils.{Globals, RunningOptions}
import logic.Examples.Example
import logic.{Clause, Constant, Literal, LogicUtils, Theory, Variable}
import lomrf.logic.{AtomSignature, Constant, EvidenceAtom, FunctionMapping, NormalForm, PredicateCompletion, PredicateCompletionMode}
import lomrf.logic.parser.KBParser
import lomrf.mln.grounding.MRFBuilder
import lomrf.mln.inference.ILP
import lomrf.mln.model.{AtomIdentityFunction, ConstantsDomainBuilder, EvidenceBuilder, KB, MLN}
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

  def format(x: Double) = {
    val defaultNumFormat = new DecimalFormat("0.############")
    defaultNumFormat.format(x)
  }

  def MAPInference(rules: List[Clause], e: Example, inps: RunningOptions) = {

    val queryAtoms = Set(
      AtomSignature("HoldsAt", 2),
      AtomSignature("InitiatedAt", 2),
      AtomSignature("TerminatedAt", 2)
    )

    /* Get BK etc */
    val mlnBKFile = s"${inps.entryPath}/MLN/MAPInferenceBK.mln"
    val (kb, constants) = KB.fromFile(mlnBKFile)
    val formulas = kb.formulas

    val parser = new KBParser(kb.predicateSchema.map { case (x, y) => x -> y.toVector }, kb.functionSchema)

    /* Input definitive clauses, whose structure is learnt over time */
    val definiteClauses = rules.map { rule =>
      val head = Literal.toMLNClauseLiteral(rule.head.asLiteral).tostring_mln
      val body = rule.body.map(Literal.toMLNClauseLiteral(_).tostring_mln).mkString(" ^ ")
      parser.parseDefiniteClause(s"${format(rule.mlnWeight)} $head :- $body")
    }

    /* Read the definite clauses from the BK file. FOR DEBUGGING */
    //val definiteClauses = kb.definiteClauses

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
    evidenceBuilder.functions += new FunctionMapping("Running_A", "running", Vector("A"))
    evidenceBuilder.functions += new FunctionMapping("Running_B", "running", Vector("B"))

    for (atom <- mlnEvidenceAtoms) {
      println(atom.tostring)
      val predicate = atom.functor
      val args = atom.terms.map(x => lomrf.logic.Constant(x.tostring)).toVector
      evidenceBuilder.evidence += EvidenceAtom.asTrue(predicate, args)
    }

    val evidence = evidenceBuilder.result()



    /* FOR DEBUGGING */
    //println(evidence.db(queryAtoms.head).numberOfUnknown)
    //println(evidence.db(queryAtoms.head).numberOfFalse)
    //println(evidence.db(queryAtoms.tail.head).numberOfUnknown)
    //println(evidence.db(queryAtoms.tail.head).numberOfFalse)

    //val resultedFormulas = PredicateCompletion(formulas, definiteClauses.toSet, PredicateCompletionMode.Decomposed)(kb.predicateSchema, kb.functionSchema, constants)

    val resultedFormulas = PredicateCompletion(formulas, definiteClauses.toSet, PredicateCompletionMode.Decomposed)(kb.predicateSchema, kb.functionSchema, constants)

    val cnf = NormalForm.compileCNF(resultedFormulas)(constants).toVector

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

    val solver = new ILP(mrf)
    //solver.infer()

    val s = solver.infer()

    var result = Map.empty[String, Boolean]

    val it = s.mrf.atoms.iterator()
    while(it.hasNext) {
      it.advance()
      val a = it.value()
      val atom = a.id.decodeAtom(mln).get
      val state = a.getState
      // keep only inferred as true atoms, get the rest via CWA.
      if (state) result += atom -> state //result += atom -> state


      if ( (atom.startsWith("Holds") || atom.startsWith("Initiated")) && state) {
        val stop = ""
      }

    }








    /*var result = Map.empty[String, Boolean]
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
        //result += atomID.decodeAtom(mln).get -> state

        /* DEBUG */
        val str = atomID.decodeAtom(mln).get
        if ( (str.startsWith("Holds") || str.startsWith("Initi")) && state) {
          val stop = ""
        }

        // Let's keep only the true atoms to reduce the size of the map
        // (get the )
        if (state) result += atomID.decodeAtom(mln).get -> state
      }
    }*/




    val resultToASP = inferredStateToASP(result, mlmConstsToAspAtomsMap)

    if (resultToASP.nonEmpty) {
      val stop = "stop"
    }

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
    val MLNEvidenceAtoms = (exmpl.narrative ++ nextAtoms).map { x =>
      val parsed = Literal.parse(x)
      Literal.toMLNFlat(parsed)
    }
    (functionMappings, MLNEvidenceAtoms, MLNConstantsToASPAtomsMap)
  }



  /* - mapAtoms are the true atoms in the MAP-inferred state
  *  - predictionsPerRuleMap is a map where each entry's key is a rule id
  *    and each value is a tuple with first/second coordinates being the actually correct/incorrect groundings of the rule
  *    and the third coordinate is is the count of correct non-terminations (for terminatedAt rules).
  *  - ruleIdsMap maps ids to actual rules.
  * */
  def getRulesMistakes(inferredAtoms: Set[String],
                              predictionsPerRuleMap: mutable.Map[Int, (mutable.SortedSet[String], mutable.SortedSet[String], Int)],
                              ruleIdsMap: Map[Int, Clause],
                              exmpl: Example,
                              inps: RunningOptions) = {

    val (_mapInferenceInit, _mapInferenceTerm, _mapInferenceHolds) = inferredAtoms.foldLeft(Vector[String](), Vector[String](), Vector[String]()) { (x, y) =>
      if (y.startsWith("initiated")) (x._1 :+ y, x._2, x._3)
      else if (y.startsWith("terminated")) (x._1, x._2 :+ y, x._3)
      else if (y.startsWith("holds")) (x._1, x._2, x._3 :+ y)
      else (x._1, x._2, x._3)
    }

    val (mapInferenceInit, mapInferenceTerm, mapInferenceHolds) = (_mapInferenceInit.toSet, _mapInferenceTerm.toSet, _mapInferenceHolds.toSet)

    val trueAtoms = exmpl.annotation.toSet
    val batchTPs = trueAtoms.intersect(mapInferenceHolds)
    val batchFPs = mapInferenceHolds.diff(trueAtoms)
    val batchFNs = trueAtoms.diff(mapInferenceHolds)

    println(s"MAP-HOLDS: ${mapInferenceHolds.size}, Batch TPs: ${batchTPs.size}, batch FPs: ${batchFPs.size}, batch FNs: ${batchFNs.size}")

    ruleIdsMap foreach { x =>
      val (id, rule) = (x._1, x._2)
      if (predictionsPerRuleMap.keySet.contains(id)) {
        val counts = predictionsPerRuleMap(id)

        // The correctlyTerminatedCount here is always zero for non-termination rules.
        val (correct, incorrect, correctlyNonTerminatedCount) = (counts._1, counts._2, counts._3)
        val compareWith = if (rule.head.functor == "initiatedAt") mapInferenceInit else mapInferenceTerm
        val actuallyTrueGroundingsInMapInferredState = compareWith.intersect(correct)
        val actuallyFalseGroundingsInMapInferredState = compareWith.intersect(incorrect)

        //println(s"True groundings: ${trueGroundingsInMapInferredState.size}, False groundings: ${falseGroundingsInMapInferredState.size}, Correctly not terminated: $correctlyNonTerminatedCount")

        //val currentSubgradient = inferredCounts(idx) - trueCounts(idx)

        // Just to be on the safe side...
        if (correctlyNonTerminatedCount != 0 && rule.head.functor != "terminatedAt") {
          throw new RuntimeException(s"Non-zero correctly non-terminated counts for non-terminated rule:\n${rule.tostring}")
        }

        // Update weights (AdaGrad)
        //val currentSubgradient = inferredCounts(idx) - trueCounts(idx)
        val lambda: Double = inps.adaRegularization //0.001 // 0.01 default
        val eta: Double = inps.adaLearnRate//1.0 // default
        val delta: Double = inps.adaGradDelta//1.0
        val currentSubgradient = actuallyTrueGroundingsInMapInferredState.size + actuallyFalseGroundingsInMapInferredState.size - correct.size
        rule.subGradient += currentSubgradient * currentSubgradient
        val coefficient = eta / (delta + math.sqrt(rule.subGradient))
        val value = rule.mlnWeight - coefficient * currentSubgradient
        val difference = math.abs(value) - (lambda * coefficient)
        if (difference > 0) rule.mlnWeight = if (value >= 0) difference else -difference
        else rule.mlnWeight = 0.0

        // Update coverage counts for rule.
        // For initiation rules TPs = correct + correctlyTerminatedCount (since correctlyTerminatedCount = 0)
        // FPs = incorrect
        // For termination rules TPs = correctly terminated + correctly not terminated = correct + correctlyTerminatedCount
        // FPs = incorrect


      }
    }

    (batchTPs, batchFPs, batchFNs)

  }

  def getTrueRulesGroundings(exmpl: Example, rules: Vector[Clause], inps: RunningOptions) = {

    val zipped = rules zip (1 to rules.length)
    val ruleIdsMap = zipped.map(x => x._2 -> x._1).toMap
    val metaRules = generateMetaRules(rules, ruleIdsMap, inps)

    val exmpls = (exmpl.narrativeASP ++ (exmpl.annotation.map(x => s"true($x)."))).mkString("\n")

    // These meta-rules ensure that we only get answers for fluents, e.g. in CAVIAR we won't gat stuff like
    // incorrectly_init(initiatedAt(meeting(a,a),4600),ruleId_45)

    val fluentMetarules =
      "correctly_initiated(initiatedAt(F,T),RuleId) :- correctly_init(initiatedAt(F,T),RuleId),fluent(F).\n"+
      "incorrectly_initiated(initiatedAt(F,T),RuleId) :- incorrectly_init(initiatedAt(F,T),RuleId),fluent(F).\n"+
      "correctly_terminated(terminatedAt(F,T),RuleId) :- correctly_term(terminatedAt(F,T),RuleId),fluent(F).\n" +
      "incorrectly_terminated(terminatedAt(F,T),RuleId) :- incorrectly_term(terminatedAt(F,T),RuleId),fluent(F).\n"

    val correctlyNotTerminatedCountsMetarules =
      "terminatedAt(F, T, Id) :- correctly_terminated(terminatedAt(F,T),Id).\n"+
        "terminatedAt(F, T, Id) :- incorrectly_terminated(terminatedAt(F,T),Id).\n"+
        "correctly_not_terminatedAt_count(Id, Count) :- terminatedRuleId(Id), Count = #count {F,T: not terminatedAt(F, T, Id), true(holdsAt(F, Te)), next(T, Te), fluent(F), time(T)}."

    // We need the ids of termination rules to reason with correctly not terminated rules.
    val terminatedRuleIds = ruleIdsMap.foldLeft(Vector[String]()) { (x, y) =>
      val (id, rule) = (y._1, y._2)
      if (rule.head.functor == "terminatedAt") x :+ s"terminatedRuleId(ruleId_$id)." else x
    }.mkString("\n")

    val show = s"#show.\n#show correctly_initiated/2.\n#show incorrectly_initiated/2.\n#show correctly_terminated/2.\n#show incorrectly_terminated/2.\n#show correctly_not_terminatedAt_count/2."

    val all = Vector(exmpls, metaRules.mkString("\n"), s"""#include "${inps.globals.BK_WHOLE_EC}".""", fluentMetarules, correctlyNotTerminatedCountsMetarules, terminatedRuleIds,  show).mkString("\n")

    val f = Utils.getTempFile("meta-scoring", ".lp")

    Utils.writeLine(all, f.getCanonicalPath, "overwrite")

    val t = ASP.solve(task=Globals.INFERENCE, aspInputFile=f)

    val answer = if (t.nonEmpty) t.head.atoms else Nil

    val (correctlyNotTerminatedCounts, rest) = answer.partition(x => x.startsWith("correctly_not_terminatedAt_count"))

    // The key to this map is a rule id, the values are tuples
    // with the first/second coordinates being a set of correct/incorrect groundings
    // and the third coordinate being the count of correctly NOT terminated groundings (in case of a termination rule).
    val predictionsPerRuleMap = scala.collection.mutable.Map[Int, (mutable.SortedSet[String], mutable.SortedSet[String], Int)]()

    rest foreach { atom =>
      val parsed = Literal.parse(atom)
      val funcSymbol = parsed.functor
      val fluentInstance = parsed.terms.head.tostring
      val ruleId = parsed.terms.tail.head.tostring.split("_")(1).toInt
      if (!predictionsPerRuleMap.keySet.contains(ruleId)) predictionsPerRuleMap(ruleId) = (mutable.SortedSet[String](), mutable.SortedSet[String](), 0)
      if (funcSymbol.startsWith("correctly")) {
        predictionsPerRuleMap(ruleId)._1 += fluentInstance
      } else if (funcSymbol.startsWith("incorrectly")) {
        predictionsPerRuleMap(ruleId)._2 += fluentInstance
      } else {
        throw new RuntimeException("Unexpected input")
      }
    }

    correctlyNotTerminatedCounts foreach { atom =>
      val parsed = Literal.parse(atom)
      val ruleId = parsed.terms.head.tostring.split("_")(1).toInt
      val count = parsed.terms.tail.head.tostring.toInt
      if (count != 0) {
        if (predictionsPerRuleMap.keySet.contains(ruleId)) {
          predictionsPerRuleMap(ruleId) = (predictionsPerRuleMap(ruleId)._1, predictionsPerRuleMap(ruleId)._2, count)
        } else {
          predictionsPerRuleMap(ruleId) = (mutable.SortedSet[String](), mutable.SortedSet[String](), 0)
        }
      }
    }

    (predictionsPerRuleMap, ruleIdsMap)
  }

  def generateMetaRules(rules: Vector[Clause], ruleIdsMap: Map[Int, Clause], inps: RunningOptions) = {

    // Here we generate meta-rules from each rule. The intention is to capture:
    // correctly_initiated, correctly_terminated, incorrectly_initiated, incorrectly_terminated

    // Example with rule:
    // initiatedAt(meet(X,Y),T) :- happensAt(active(X),T). (assume its id is 12)
    // Correctly initiated meta-rule:
    // correctly_init(initiatedAt(meet(X,Y),T), ruleId_12) :- happensAt(active(X),T), person(X), person(Y), time(T), true(holdsAt(meet(X,Y),Te)), next(T,Te).
    // Incorrectly initiated meta-rule:
    // incorrectly_init(initiatedAt(meet(X,Y),T), ruleId_12) :- happensAt(active(X),T), person(X), person(Y), time(T), not true(holdsAt(meet(X,Y),Te)), next(T,Te).

    // Example with rule:
    // terminatedAt(meet(X,Y),T) :- happensAt(walking(X),T). (assume its id is 23)
    // Correctly terminated meta-rule:
    // correctly_term(terminatedAt(meet(X,Y),T), ruleId_23) :- happensAt(walking(X),T), person(X), person(Y), time(T), true(holdsAt(meet(X,Y),T)), not true(holdsAt(meet(X,Y),Te)), next(T,Te).
    // Incorrectly terminated meta-rule:
    // incorrectly_term(terminatedAt(meet(X,Y),T), ruleId_23) :- happensAt(walking(X),T), person(X), person(Y), time(T), true(holdsAt(meet(X,Y),Te)), next(T,Te).

    val metaRules = ruleIdsMap.foldLeft(Vector[String]()) { (accum, r) =>
      val (ruleId, rule) = (r._1, r._2)
      val typeAtoms = rule.toLiteralList.flatMap(x => x.getTypePredicates(inps.globals)).distinct.map(x => Literal.parse(x))
      val timeVar = rule.head.terms.tail.head
      val ruleFluent = rule.head.terms.head
      val nextTimeVar = logic.Variable("Te")
      if (rule.head.functor == "initiatedAt") {
        val head1 = Literal(functor = "correctly_init", terms = List(rule.head, logic.Constant(s"ruleId_$ruleId")))
        val body1 = (rule.body ++ typeAtoms) ++ List(Literal(functor = "true", terms = List(Literal(functor = "holdsAt", terms = List(ruleFluent, nextTimeVar)))), Literal(functor = "next", terms = List(timeVar, nextTimeVar)))
        val correctly_init = Clause(head1, body1)
        val head2 = Literal(functor = "incorrectly_init", terms = List(rule.head, logic.Constant(s"ruleId_$ruleId")))
        val body2 = (rule.body ++ typeAtoms) ++ List(Literal(functor = "true", terms = List(Literal(functor = "holdsAt", terms = List(ruleFluent, nextTimeVar))), isNAF = true), Literal(functor = "next", terms = List(timeVar, nextTimeVar)))
        val incorrectly_init = Clause(head2, body2)
        accum ++ Vector(correctly_init.tostring, incorrectly_init.tostring)
      } else if (rule.head.functor == "terminatedAt") {

        val head1 = Literal(functor = "correctly_term", terms = List(rule.head, logic.Constant(s"ruleId_$ruleId")))
        val body1 = (rule.body ++ typeAtoms) ++ List(Literal(functor = "true", terms = List(Literal(functor = "holdsAt", terms = List(ruleFluent, timeVar)))), Literal(functor = "true", terms = List(Literal(functor = "holdsAt", terms = List(ruleFluent, nextTimeVar))), isNAF = true), Literal(functor = "next", terms = List(timeVar, nextTimeVar)))
        val correctly_term = Clause(head1, body1)
        val head2 = Literal(functor = "incorrectly_term", terms = List(rule.head, logic.Constant(s"ruleId_$ruleId")))
        val body2 = (rule.body ++ typeAtoms) ++ List(Literal(functor = "true", terms = List(Literal(functor = "holdsAt", terms = List(ruleFluent, nextTimeVar)))), Literal(functor = "next", terms = List(timeVar, nextTimeVar)))
        val incorrectly_term = Clause(head2, body2)
        accum ++ Vector(correctly_term.tostring, incorrectly_term.tostring)
      } else {
        throw new RuntimeException(s"Unexpected input: ${rule.head.tostring}")
      }
    }
    metaRules
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

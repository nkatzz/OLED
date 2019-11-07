package woled

import java.text.DecimalFormat

import app.runutils.{Globals, RunningOptions}
import logic.Examples.Example
import logic.{Clause, Constant, Literal, LogicUtils, Theory, Variable}
import lomrf.logic.{AtomSignature, Constant, EvidenceAtom, FunctionMapping, NormalForm, PredicateCompletion, PredicateCompletionMode}
import lomrf.logic.parser.KBParser
import lomrf.mln.grounding.MRFBuilder
import lomrf.mln.inference.ILP
import lomrf.mln.learning.structure.ClauseConstructor
import lomrf.mln.model.{AtomIdentityFunction, ConstantsDomainBuilder, EvidenceBuilder, KB, MLN}
import utils.{ASP, Utils}
import lomrf.mln.model.AtomIdentityFunctionOps._

import scala.collection.mutable
import scala.io.Source

object WoledUtils {


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
    //val mlnBKFile = s"${inps.entryPath}/MLN/MAPInferenceBK.mln"
    val mlnBKFile = s"${inps.entryPath}/MAPInferenceBK.mln"

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
      val domains = kb.predicateSchema(AtomSignature(atom.functor, args.length))
      domains.zip(args).foreach { case (domain, value) => const += domain -> value.symbol }
    }




    /* For CAVIAR fragment */
    const += "dist" -> "34"
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
    const += "event" -> "Running_A"

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
      val predicate = atom.functor
      val args = atom.terms.map(x => lomrf.logic.Constant(x.tostring)).toVector
      evidenceBuilder.evidence += EvidenceAtom.asTrue(predicate, args)
    }

    for (atom <- inertiaAtoms) {
      val predicate = atom.functor.capitalize
      val fluent = atom.terms.head.asInstanceOf[Literal]
      val fluentConst = s"${fluent.functor.capitalize}_${fluent.terms.map(x => x.tostring.capitalize).mkString("_")}"
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

    val resultedFormulas = PredicateCompletion(formulas, definiteClauses.toSet, PredicateCompletionMode.Decomposed)(kb.predicateSchema, kb.functionSchema, constants)
    val cnf = ClauseConstructor.makeCNF(resultedFormulas)(constants).toVector

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
  *  - exmplsCount is the total number of groundings in the current batch (the N for the Hoeffding test)
  * */
  def getRulesMistakes(inferredAtoms: Set[String],
                       predictionsPerRuleMap: mutable.Map[Int, (mutable.SortedSet[String], mutable.SortedSet[String], Int)],
                       ruleIdsMap: Map[Int, Clause],
                       exmpl: Example,
                       exmplsCount: Int,
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

    /* DEBUGGING INFO */
    val rules = ruleIdsMap.map(x => x._2).toVector
    println(s"Batch TPs: ${batchTPs.size}, batch FPs: ${batchFPs.size}, batch FNs: ${batchFNs.size}")
    println(s"weights before: ${rules.map(x => x.mlnWeight).mkString(" ")}")

    val parentRules = scala.collection.mutable.Set[Clause]()

    ruleIdsMap foreach { x =>
      val (id, rule) = (x._1, x._2)

      //println(rule.tostring)

      if (predictionsPerRuleMap.keySet.contains(id)) {

        val counts = predictionsPerRuleMap(id)

        // The correctlyTerminatedCount here is always zero for non-termination rules.
        val (correct, incorrect, correctlyNonTerminatedCount) = (counts._1, counts._2, counts._3)
        val compareWith = if (rule.head.functor == "initiatedAt") mapInferenceInit else mapInferenceTerm

        // Instances proved by the rule, which are true in both the map-inferred state, and in the true state
        val actuallyTrueGroundingsInMapInferredState = compareWith.intersect(correct)

        // Instances proved by the rule, which are true in the map-inferred state, but false in the true state
        val actuallyFalseGroundingsInMapInferredState = compareWith.intersect(incorrect)

        // Just to be on the safe side...
        if (correctlyNonTerminatedCount != 0 && rule.head.functor != "terminatedAt") {
          throw new RuntimeException(s"Non-zero correctly non-terminated counts for non-terminated rule:\n${rule.tostring}")
        }

        // Update weights (AdaGrad)
        //val currentSubgradient = inferredCounts(idx) - trueCounts(idx)
        val lambda = inps.adaRegularization //0.001 // 0.01 default
        val eta  = inps.adaLearnRate//1.0 // default
        val delta  = inps.adaGradDelta//1.0
        val currentSubgradient = actuallyTrueGroundingsInMapInferredState.size + actuallyFalseGroundingsInMapInferredState.size - correct.size
        rule.subGradient += currentSubgradient * currentSubgradient
        val coefficient = eta / (delta + math.sqrt(rule.subGradient))
        val value = rule.mlnWeight - coefficient * currentSubgradient
        val difference = math.abs(value) - (lambda * coefficient)
        if (difference > 0) rule.mlnWeight = if (value >= 0) difference else -difference
        else rule.mlnWeight = 0.0

        // This is used to score empty-bodied parent rules, which are not used for inference and thus they are not scored.
        if (rule.parentClause.body.isEmpty) {
          if (!parentRules.contains(rule.parentClause)) {
            rule.parentClause.seenExmplsNum += exmplsCount
            parentRules += rule.parentClause
          }
        }

        // Update coverage counts for the current rule.
        // For initiation rules:
        // TPs = correct + correctlyTerminatedCount (since correctlyTerminatedCount is always 0 for such rule)
        // FPs = incorrect
        // For termination rules:
        // TPs = correctly terminated + correctly not terminated = correct + correctlyTerminatedCount
        // FPs = incorrect

        // Should we replace these with actuallyTrueGroundingsInMapInferredState (for TPs) and actuallyFalseGroundingsInMapInferredState (for FPs)?
        rule.tps += correct.size //+ correctlyNonTerminatedCount
        if (rule.head.functor == "terminatedAt") {
          rule.fns += incorrect.size
        } else {
          rule.fps += incorrect.size
        }
        rule.seenExmplsNum += exmplsCount

        /*rule.tps += actuallyTrueGroundingsInMapInferredState.size + correctlyNonTerminatedCount
        if (rule.head.functor == "terminatedAt") {
          rule.fns += actuallyFalseGroundingsInMapInferredState.size
        } else {
          rule.fps += actuallyFalseGroundingsInMapInferredState.size
        }
        rule.seenExmplsNum += exmplsCount*/


      }
    }

    /* DEBUGGING INFO */
    println(s"weights after:  ${rules.map(x => x.mlnWeight).mkString(" ")}")

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

    val show = s"#show.\n#show correctly_initiated/2.\n#show incorrectly_initiated/2.\n#show correctly_terminated/2.\n#show incorrectly_terminated/2.\n#show correctly_not_terminatedAt_count/2.\n${inps.globals.SHOW_INTERPRETATIONS_COUNT}"

    val all = Vector(exmpls, metaRules.mkString("\n"), s"""#include "${inps.globals.BK_WHOLE_EC}".""", fluentMetarules, correctlyNotTerminatedCountsMetarules, terminatedRuleIds, inps.globals.EXAMPLE_COUNT_RULE, show).mkString("\n")

    val f = utils.Utils.getTempFile("meta-scoring", ".lp")

    utils.Utils.writeLine(all, f.getCanonicalPath, "overwrite")

    val t = ASP.solve(task=Globals.INFERENCE, aspInputFile=f)

    val answer = if (t.nonEmpty) t.head.atoms else Nil

    //val (correctlyNotTerminatedCounts, rest) = answer.partition(x => x.startsWith("correctly_not_terminatedAt_count"))

    val (correctlyNotTerminatedCounts, exmplCount, rest) = answer.foldLeft(Vector[String](), Vector[String](), Vector[String]()) { (x, y) =>
      if (y.startsWith("correctly_not_terminatedAt_count")) (x._1 :+ y, x._2, x._3)
      else if (y.startsWith("countGroundings")) (x._1, x._2 :+ y, x._3)
      else (x._1, x._2, x._3 :+ y)
    }

    val currentTotalExmplsGroundingsCount = exmplCount.head.split("\\(")(1).split("\\)")(0).toInt

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

    (predictionsPerRuleMap, ruleIdsMap, currentTotalExmplsGroundingsCount)
  }

  def generateMetaRules(rules: Vector[Clause], ruleIdsMap: Map[Int, Clause], inps: RunningOptions) = {

    // Here we generate meta-rules from each rule. The intention is to capture:
    // correctly_initiated, correctly_terminated, incorrectly_initiated, incorrectly_terminated

    // Example with rule:
    // initiatedAt(meet(X,Y),T) :- happensAt(active(X),T). (assume its id is 12)
    // Meta-rules:
    // correctly_init(initiatedAt(meet(X,Y),T), ruleId_12) :- happensAt(active(X),T), person(X), person(Y), time(T), true(holdsAt(meet(X,Y),Te)), next(T,Te).
    // correctly_init(initiatedAt(meet(X,Y),T), ruleId_12) :- happensAt(active(X),T), person(X), person(Y), time(T), true(holdsAt(meet(X,Y),T)).
    // incorrectly_init(initiatedAt(meet(X,Y),T), ruleId_12) :- happensAt(active(X),T), person(X), person(Y), time(T), not true(holdsAt(meet(X,Y),Te)), next(T,Te).


    // Example with rule:
    // terminatedAt(meet(X,Y),T) :- happensAt(walking(X),T). (assume its id is 23)
    // Meta-rules:
    // correctly_term(terminatedAt(meet(X,Y),T), ruleId_23) :- happensAt(walking(X),T), person(X), person(Y), time(T), not true(holdsAt(meet(X,Y),Te)), next(T,Te).
    // correctly_term(terminatedAt(meet(X,Y),T), ruleId_23) :- happensAt(walking(X),T), person(X), person(Y), time(T), not true(holdsAt(meet(X,Y),T)).
    // incorrectly_term(terminatedAt(meet(X,Y),T), ruleId_23) :- happensAt(walking(X),T), person(X), person(Y), time(T), true(holdsAt(meet(X,Y),Te)), next(T,Te).
    // incorrectly_term(terminatedAt(meet(X,Y),T), ruleId_23) :- happensAt(walking(X),T), person(X), person(Y), time(T), true(holdsAt(meet(X,Y),T)).

    val metaRules = ruleIdsMap.foldLeft(Vector[String]()) { (accum, r) =>
      val (ruleId, rule) = (r._1, r._2)
      val typeAtoms = rule.toLiteralList.flatMap(x => x.getTypePredicates(inps.globals)).distinct.map(x => Literal.parse(x))
      val timeVar = rule.head.terms.tail.head
      val ruleFluent = rule.head.terms.head
      val nextTimeVar = logic.Variable("Te")
      if (rule.head.functor == "initiatedAt") {

        val head1 = Literal(functor = "correctly_init", terms = List(rule.head, logic.Constant(s"ruleId_$ruleId")))
        val body11 = (rule.body ++ typeAtoms) ++ List(Literal(functor = "true", terms = List(Literal(functor = "holdsAt", terms = List(ruleFluent, nextTimeVar)))), Literal(functor = "next", terms = List(timeVar, nextTimeVar)))
        val body12 = (rule.body ++ typeAtoms) ++ List(Literal(functor = "true", terms = List(Literal(functor = "holdsAt", terms = List(ruleFluent, timeVar)))))
        val correctly_init1 = Clause(head1, body11)
        val correctly_init2 = Clause(head1, body12)

        val head2 = Literal(functor = "incorrectly_init", terms = List(rule.head, logic.Constant(s"ruleId_$ruleId")))
        val body2 = (rule.body ++ typeAtoms) ++ List(Literal(functor = "true", terms = List(Literal(functor = "holdsAt", terms = List(ruleFluent, nextTimeVar))), isNAF = true), Literal(functor = "next", terms = List(timeVar, nextTimeVar)))
        val incorrectly_init = Clause(head2, body2)
        accum ++ Vector(correctly_init1.tostring, correctly_init2.tostring, incorrectly_init.tostring)

      } else if (rule.head.functor == "terminatedAt") {

        val head1 = Literal(functor = "correctly_term", terms = List(rule.head, logic.Constant(s"ruleId_$ruleId")))
        val body11 = (rule.body ++ typeAtoms) ++ List(Literal(functor = "true", terms = List(Literal(functor = "holdsAt", terms = List(ruleFluent, nextTimeVar))), isNAF = true), Literal(functor = "next", terms = List(timeVar, nextTimeVar)))
        val body12 = (rule.body ++ typeAtoms) ++ List(Literal(functor = "true", terms = List(Literal(functor = "holdsAt", terms = List(ruleFluent, timeVar))), isNAF = true))
        val correctly_term1 = Clause(head1, body11)
        val correctly_term2 = Clause(head1, body12)

        val head2 = Literal(functor = "incorrectly_term", terms = List(rule.head, logic.Constant(s"ruleId_$ruleId")))
        val body21 = (rule.body ++ typeAtoms) ++ List(Literal(functor = "true", terms = List(Literal(functor = "holdsAt", terms = List(ruleFluent, nextTimeVar)))), Literal(functor = "next", terms = List(timeVar, nextTimeVar)))
        val body22 = (rule.body ++ typeAtoms) ++ List(Literal(functor = "true", terms = List(Literal(functor = "holdsAt", terms = List(ruleFluent, timeVar)))))

        val incorrectly_term1 = Clause(head2, body21)
        val incorrectly_term2 = Clause(head2, body22)
        accum ++ Vector(correctly_term1.tostring, correctly_term2.tostring, incorrectly_term1.tostring, incorrectly_term2.tostring)

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

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

import java.io.File

import app.runutils.Globals
import logic.{AnswerSet, Constant, Literal}
import logic.Examples.Example
import lomrf.logic.parser.KBParser
import lomrf.logic._
import lomrf.mln.grounding.MRFBuilder
import lomrf.mln.inference.{ILP, Solver}
import lomrf.mln.learning.weight.WLearning
import lomrf.mln.model
import lomrf.mln.model.mrf.MRF
import lomrf.mln.model.{AtomIdentityFunction, ConstantsSet, ConstantsSetBuilder, Evidence, EvidenceBuilder, EvidenceDB, KB, MLN, MLNSchema, PredicateSpace}
import utils.{ASP, Utils}

import scala.util.{Failure, Success}

object ASP2MLN {

  /*
  *
  * RESTRICTIONS:
  *
  * 1. Currently, all domain predicates (e.g. close/4, next/2 etc), i.e. all predicates
  * that are neither label predicates (initiatedAt/2, terminatedAt/2), nor event predicates
  * (happensAt/2) ARE ASSUMED TO BE FLAT. Therefore, I simply capitalize the predicate functor
  * and each predicate constant to convert them to MLN format. Supporting functions in domain
  * predicates requires some extra work in the conversion.
  *
  *
  * */

  // I need to automate the extraction of those from the predicates' definition file.
  // These need to be mapped to specific constants that appear in each data mini-batch.
  val types = List("time", "person", "dist", "event", "fluent")

  // Just a test
  val rules = List(
  "1 InitiatedAt(meeting(id1, id2), t) :- HappensAt(walking(id1), t) ^ HappensAt(active(id2), t)"
  )

  // This also needs to be extracted automatically.
  val annotationTemplate = "InitiatedAt(fluent,time)"

  def main(args: Array[String]): Unit = {

    val atoms = ASP.solve(task = Globals.INFERENCE,
      aspInputFile = new File("/home/nkatz/dev/OLED/iled/datasets/WeightLearning/Caviar/fragment/meeting/ASP/get-predicates.lp"))

    val (kb, constants) = KB.fromFile("/home/nkatz/dev/OLED/iled/datasets/WeightLearning/Caviar/fragment/meeting/MLN/move_HI.mln")

    val queryPredicates = Set[AtomSignature](AtomSignature("InitiatedAt", 2))

    val (fluentsMap, eventsMap, aspLabelAtoms, aspEventAtoms, allOtherAtoms) = getAllASPAtoms(atoms.head.atoms, "meeting", "initiatedAt")

    // First, gather all type predicates to form the constantsDomain.
    val parsedDomainAtoms = allOtherAtoms.map(x => Literal.parse(x))

    //split type predicates from simple domain atoms:
    val (typeAtoms, domainAtoms) = parsedDomainAtoms.foldLeft(Vector[Literal](), Vector[Literal]()) { (accum, lit) =>
      if (types.contains(lit.predSymbol)) (accum._1 :+ lit, accum._2) else (accum._1, accum._2 :+ lit)
    }

    val constantsDomain = getConstantsDomain(typeAtoms, fluentsMap, eventsMap)

    val evidenceBuilder = EvidenceBuilder(kb.predicateSchema, kb.functionSchema, queryPredicates, Set.empty, constantsDomain)
    val evidence = buildEvidence(aspEventAtoms, aspLabelAtoms, domainAtoms, eventsMap, fluentsMap, evidenceBuilder)

    val annotationSchema = getAnnotationSchema(annotationTemplate)
    val annotationBuilder = EvidenceBuilder(annotationSchema, queryPredicates, Set.empty, constantsDomain)
    val annotationDB = buildAnnotation(aspLabelAtoms, fluentsMap, annotationBuilder)

    val mln = constructMLN(kb, evidence, rules, queryPredicates)

    //val clausesWithUpdatedWeights = adagrad(mln, annotationDB)

    println("before")
    mln.clauses.foreach { x => println(x.toText(), x.weight) }

    adagrad2(mln, annotationDB, "precision")

    println("after")
    mln.clauses.foreach { x => println(x.toText(), x.weight) }

  }

  def test = {

    /*
    val mln = MLN() // an mln with query atoms holdsAt/2.

    val mrfBuilder = new MRFBuilder(mln, createDependencyMap = true)
    val mrf = mrfBuilder.buildNetwork
    val solver = new ILP(mrf, annotationDB = annotationDB, lossAugmented = false, ilpSolver = Solver.LPSOLVE)
    solver.infer()
    */
  }


  def adagrad2(mln: MLN, annotationDB: EvidenceDB, scoringFunction: String, postLearning: Boolean = false): MRF = {

    require(scoringFunction.toLowerCase == "precision" || scoringFunction.toLowerCase == "recall", "Eimai blakas!")

    val sigma: Double = 1.0
    val lambda: Double = 0.01
    //val eta: Double = 1.0 //default
    val eta: Double = 0.1
    val delta: Double = 1.0

    val numberOfClauses = mln.clauses.length

    //val t3 = System.nanoTime()

    val mrfBuilder = new MRFBuilder(mln, createDependencyMap = true)
    val mrf = mrfBuilder.buildNetwork

    //val t4 = System.nanoTime()
    //println(s"Build MRF time: ${(t4-t3)/1000000000.0}")

    // Perform inference for the current weight vector and count true groundings
    val solver = new ILP(mrf, annotationDB = annotationDB, lossAugmented = false, ilpSolver = Solver.LPSOLVE)
    solver.infer()

    val counts =
      if (scoringFunction == "precision") WLearning.calculatePrError(mrf, annotationDB)
      else WLearning.calculateReError(mrf, annotationDB)
    //println("Inferred Counts: [" + inferredCounts.deep.mkString(", ") + "]")

    /*
    mln.clauses.zipWithIndex.foreach { case (c, id) =>
      println(s"${c.toText(weighted = true)} precision: ${counts(id)}")
    }
    */

    mln.clauses.zipWithIndex.foreach { case (c, idx) =>
      val currentSubgradient =
        if (c.isHard) 0
        else if (counts(idx).isNaN) 1
        else counts(idx)

      c.subgradient += currentSubgradient * currentSubgradient

      val coefficient = eta / (delta + math.sqrt(c.subgradient))
      val value = c.weight - coefficient * currentSubgradient
      val difference = math.abs(value) - (lambda * coefficient)

      if (difference > 0) c.weight = if (value >= 0) difference else -difference
      else c.weight = 0.0
    }
    //mln.clauses

    if (postLearning) {
      val mrfBuilder = new MRFBuilder(mln, createDependencyMap = true)
      val postLearningMRF = mrfBuilder.buildNetwork
      val solver = new ILP(postLearningMRF, annotationDB = annotationDB, lossAugmented = false, ilpSolver = Solver.LPSOLVE)
      solver.infer()
      postLearningMRF
    }
    else mrf
  }

  def cda(mln: MLN, annotationDB: EvidenceDB, t: Int, postLearning: Boolean = false): MRF = {

    val sigma: Double = 1.0
    var weightedDeltaPhi = 0.0

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

    // Set default learning rate
    val defaultLearningRate = 1.0 / (sigma * t)

    val error = WLearning.calculatePrError(mrf, annotationDB).sum //WLearning.calculateError(mrf, annotationDB)
    println("Current loss: " + error)

    // Calculate true counts minus inferred counts
    mln.clauses.zipWithIndex.foreach { case (c, clauseIdx) =>
      if (!mrf.mln.clauses(clauseIdx).isHard) {
        c.subgradient = trueCounts(clauseIdx) - inferredCounts(clauseIdx)
        weightedDeltaPhi += c.weight * c.subgradient
      }
    }

    var loss = error - weightedDeltaPhi
    if(loss < 0) loss = 0.0

    val square2NormDeltaPhi = mln.clauses.map(c => c.subgradient * c.subgradient).sum

    val learningRate = if(square2NormDeltaPhi <= 0) 0.0
    else Math.min(defaultLearningRate, (error - ((t - 1) * weightedDeltaPhi) / t) / square2NormDeltaPhi)

    println("Learning rate: " + learningRate)

    // Update weights
    mln.clauses.foreach { c =>
        c.weight = (t - 1) * c.weight / t + learningRate * c.subgradient
    }

    if (postLearning) {
      val mrfBuilder = new MRFBuilder(mln, createDependencyMap = true)
      val postLearningMRF = mrfBuilder.buildNetwork
      val solver = new ILP(postLearningMRF, annotationDB = annotationDB, lossAugmented = false, ilpSolver = Solver.LPSOLVE)
      solver.infer()
      postLearningMRF
    }
    else mrf
  }

  def adagrad1(mln: MLN, annotationDB: EvidenceDB, postLearning: Boolean = false): MRF = {

    val sigma: Double = 1.0
    val lambda: Double = 0.01

    val eta: Double = 1.0 // default
    //val eta: Double = 0.1

    val delta: Double = 1.0

    val numberOfClauses = mln.clauses.length

    //val t3 = System.nanoTime()

    val mrfBuilder = new MRFBuilder(mln, createDependencyMap = true)
    val mrf = mrfBuilder.buildNetwork

    //val t4 = System.nanoTime()
    //println(s"Build MRF time: ${(t4-t3)/1000000000.0}")

    WLearning.setAnnotatedState(mrf, annotationDB)


    //val trueCounts = WLearning.countGroundings(mrf, numberOfClauses)

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

      val coefficient = eta / (delta + math.sqrt(c.subgradient))
      val value = c.weight - coefficient * currentSubgradient
      val difference = math.abs(value) - (lambda * coefficient)

      if (difference > 0) c.weight = if (value >= 0) difference else -difference
      else c.weight = 0.0
    }
    //mln.clauses

    if (postLearning) {
      val mrfBuilder = new MRFBuilder(mln, createDependencyMap = true)
      val postLearningMRF = mrfBuilder.buildNetwork
      val solver = new ILP(postLearningMRF, annotationDB = annotationDB, lossAugmented = false, ilpSolver = Solver.LPSOLVE)
      solver.infer()
      postLearningMRF
    }
    else mrf
  }


  def getAnnotationSchema(annotationTemplate: String): Map[AtomSignature, Vector[String]] = {
    val p = Literal.parse(annotationTemplate.toCharArray.take(1)(0).toString.toLowerCase() + annotationTemplate.drop(1))
    Map(AtomSignature(p.predSymbol.capitalize, p.arity) -> p.terms.map(x => x.name).toVector)
  }

  def parseRules(kb: KB, evidence: Evidence, clauses: Iterable[String]): Set[Clause] = {
    val signaturesMap = kb.predicateSchema.map { case (a, seq) => a -> seq.toVector }
    val parser = new KBParser(signaturesMap, kb.functionSchema)

    ///*
    val (definiteClauseStrings, formulaStrings) = clauses.partition(_.contains(":-"))
    val definiteClauses = definiteClauseStrings.map(parser.parseDefiniteClause)
    val formulas = formulaStrings.map(parser.parseWeightedFormula).toSet
    val completed = PredicateCompletion(formulas, definiteClauses.toSet, PredicateCompletionMode.Decomposed)(kb.predicateSchema, kb.functionSchema, evidence.constants)
    val cnf = NormalForm.compileCNF(completed)(evidence.constants)
    //*/

    // The code below does not perform completion on clauses with an empty body.
    // The reason is that on such clauses, after completion we get something like
    // 1 InitiatedAt(meeting(x0, x1),x2,RuleId_1)
    // Infinity !InitiatedAt(meeting(x0, x1),x2,RuleId_1).
    // The second hard constraint prevails, as a result the empty-bodied rule covers nothing.
    // although it has a large weight.
    // But a first attempt to ise the code below failed, it seems like the formula still
    // goes through completion and the hard constraint is generated. As a workaround, I do not
    // score empty-bodied rules at all.

    /*
    val (definiteClauseStrings, formulaStrings) =
      clauses.partition(_.contains(":-"))

    val definiteClauses =
      definiteClauseStrings.map(parser.parseDefiniteClause)
    val formulas =
      formulaStrings.map(parser.parseWeightedFormula).toSet

    val completed =
      PredicateCompletion(Set.empty, definiteClauses.toSet, PredicateCompletionMode.Decomposed)(kb.predicateSchema, kb.functionSchema, evidence.constants)

    val cnf = NormalForm.compileCNF(completed ++ formulas)(evidence.constants)
    */
    cnf
  }

  def constructMLN(kb: KB, evidence: Evidence, clauses: Iterable[String], queryPredicates: Set[AtomSignature]): MLN = {
    val cnf = parseRules(kb, evidence, clauses)
    val mlnSchema = MLNSchema(kb.predicateSchema, kb.functionSchema, Map.empty, Map.empty)
    val predicateSpace = PredicateSpace(mlnSchema, queryPredicates, evidence.constants)
    val mln = MLN(mlnSchema, evidence, predicateSpace, cnf.toVector)
    mln
  }


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


  def buildEvidence(eventAtoms: Iterable[String], fluentAtoms: Iterable[String], domainAtoms: Iterable[Literal],
                    eventsMap: Map[String, String], fluentsMap: Map[String, String], builder: EvidenceBuilder): Evidence = {

    val isNegated : String => Boolean = _.contains("false_")

    eventAtoms foreach { atom =>

      // The eventsMap contains mappings of the form walking(id1) -> Walking_Id1.
      // Retrieve the key:
      val eventSignatureAtom = eventsMap.keySet.find(x => atom.contains(x)).getOrElse(
        throw new RuntimeException(s"Cannot find mapping for atom $atom in events map: $eventsMap"))

      // We need to pass the current atom in an EvidenceBuilder like so:
      // builder.functions += FunctionMapping("Walking_A", "walking", Vector(Constant("A")))
      // So we first parse the atom into a logic.Literal instance to get its structure.
      // We assume that event signatures are functions that map "flat" ASP atoms to MLN constants like so:
      // eventName(arg1,...argn,) = EventName_arg1_..._arg1
      val eventSignatureAtomParsed = Literal.parse(eventSignatureAtom)

      // Add the current atom to the evidence builder like so:
      // builder.evidence += EvidenceAtom.asTrue("HappensAt", Vector[Constant](Constant("Walking_A"), Constant(0.toString)))
      val mlnEventConstant = lomrf.logic.Constant(eventsMap(eventSignatureAtom))
      val timeConstant = lomrf.logic.Constant(Literal.parse(atom).terms(1).tostring)
      builder.evidence += EvidenceAtom.asTrue("HappensAt", Vector(mlnEventConstant, timeConstant))

      // Add the function mapping for the current atom to the evidence builder like so:
      // builder.functions += FunctionMapping("Walking_A", "walking", Vector(Constant("A")))
      /*
      val eventAtomConstants = eventSignatureAtomParsed.terms.map(x => lomrf.logic.Constant(x.name.capitalize)).toVector
      val actualFunctionSymbol = eventSignatureAtomParsed.functor
      val correspondingMLNConstant = eventsMap(eventSignatureAtom)
      builder.functions += FunctionMapping(correspondingMLNConstant, actualFunctionSymbol, eventAtomConstants)
      */
    }

    // Handle function mappings for all event signatures. Adding function mappings for observed events
    // only won't work, since often we have rules that have negated event atoms in the body and we'll
    // not have function mappings for them.
    eventsMap foreach { x =>
      val (eventSignatureAtom, eventMLNConstant) = (x._1, x._2)
      val eventSignatureAtomParsed = Literal.parse(eventSignatureAtom)
      val eventAtomConstants = eventSignatureAtomParsed.terms.map(x => lomrf.logic.Constant(x.name.capitalize)).toVector
      val actualFunctionSymbol = eventSignatureAtomParsed.predSymbol
      val correspondingMLNConstant = eventMLNConstant
      builder.functions += FunctionMapping(correspondingMLNConstant, actualFunctionSymbol, eventAtomConstants)
    }


    // We need to handle function mappings for fluent atoms too.
    fluentAtoms foreach { atom =>
      val fluentSignatureAtom = fluentsMap.keySet.find(x => atom.contains(x)).getOrElse(
        throw new RuntimeException(s"Cannot find mapping for atom $atom in fluents map: $fluentsMap"))

      val fluentSignatureAtomParsed = Literal.parse(fluentSignatureAtom)

      val fluentAtomConstants = fluentSignatureAtomParsed.terms.map(x => lomrf.logic.Constant(x.name.capitalize)).toVector
      val actualFunctionSymbol = fluentSignatureAtomParsed.predSymbol
      val correspondingMLNConstant = fluentsMap(fluentSignatureAtom)
      if (isNegated(atom)) {
        builder.functions += FunctionMapping(correspondingMLNConstant, actualFunctionSymbol, fluentAtomConstants)
      } else {
        builder.functions += FunctionMapping(correspondingMLNConstant, actualFunctionSymbol, fluentAtomConstants)

      }
    }

    // Handle the rest of the domain atoms. These are assumed to be flat,
    // so we simply capitalize the functor and each of the constants.
    domainAtoms foreach { atom =>
      val constants = atom.terms.map(x => lomrf.logic.Constant(x.name.capitalize))
      builder.evidence += EvidenceAtom.asTrue(atom.predSymbol.capitalize, constants.toVector)
    }

    builder.result()
  }


  def buildAnnotation(fluentAtoms: Iterable[String],
                      fluentsMap: Map[String, String], builder: EvidenceBuilder): model.EvidenceDB = {

    val isNegated : String => Boolean = _.contains("false_")

    fluentAtoms foreach { atom =>
      val fluentSignatureAtom = fluentsMap.keySet.find(x => atom.contains(x)).getOrElse(
        throw new RuntimeException(s"Cannot find mapping for atom $atom in fluents map: $fluentsMap"))

      val parsed = Literal.parse(atom)
      val mlnfluentConstant = lomrf.logic.Constant(fluentsMap(fluentSignatureAtom))
      val timeConstant = lomrf.logic.Constant(parsed.terms(1).tostring)
      val ruleIdConstant = lomrf.logic.Constant(parsed.terms(2).tostring.capitalize)
      if (isNegated(atom)) {
        builder.evidence += EvidenceAtom.asFalse(parsed.predSymbol.split("_")(1).capitalize, Vector(mlnfluentConstant, timeConstant, ruleIdConstant))
      } else {
        builder.evidence += EvidenceAtom.asTrue(parsed.predSymbol.capitalize, Vector(mlnfluentConstant, timeConstant, ruleIdConstant))
      }
    }
    builder.result().db
  }


  def getConstantsDomain(typeAtoms: Vector[Literal],
                         fluentsMap: Map[String, String],
                         eventsMap: Map[String, String]): Map[String, ConstantsSet] = {
    val grouped =
      typeAtoms.filter(k => k.predSymbol !="event" && k.predSymbol != "fluent" ).
        groupBy(a => a.predSymbol).
        map{ case (k,v) => (k, ConstantsSetBuilder(v.map(z => z.terms.head.tostring.capitalize)).result()) }
    val constantsDomain =
      (scala.collection.mutable.Map[String, ConstantsSet]() ++= grouped) +=
      ("fluent" -> ConstantsSetBuilder(fluentsMap.values).result()) +=
      ("event" -> ConstantsSetBuilder(eventsMap.values).result())
    constantsDomain.toMap
  }








  def getAllASPAtoms(_atoms: List[String], targetHLE: String, targetClass: String) = {

    val atoms = _atoms.toVector

    def atomToMLNConstant(s: String) = {
      val atom = Literal.toLiteral1(s).terms.head.asInstanceOf[Literal]
      val constant = (List(s"${atom.predSymbol.capitalize}") ++ atom.terms.map(x => x.tostring.capitalize)).mkString("_")
      (atom.tostring, constant)
    }

    // Returns a mapping between fluent predicates in the domain and MLN constants, like so:
    // Map(meeting(id2,id1) -> Meeting_Id2_Id1, meeting(id1,id2) -> Meeting_Id1_Id2).
    // These are needed to populate the EvidenceBuilder.
    val fluentsMap_ = Utils.time { atoms.filter(x => x.contains("fluent") && x.contains(targetHLE)).map { atom => atomToMLNConstant(atom) }.toMap }
    val fluentsMap = fluentsMap_._1
    println(s"Fluents Map construction time: ${fluentsMap_._2}")

    // Similar to the previous method, returns a map between event atoms and MLN constants like so:
    // Map(walking(id1) -> Walking_Id1, active(id2) -> Active_Id2)
    val eventsMap_ = Utils.time { atoms.filter(x => x.contains("event") || x.contains("event1")).map { atom => atomToMLNConstant(atom) }.toMap }
    val eventsMap = eventsMap_._1
    println(s"Events Map construction time: ${eventsMap_._2}")

    val t0 = System.nanoTime()

    val (labelAtoms, eventAtoms, domainAtoms) = atoms.foldLeft(Vector[String](), Vector[String](), Vector[String]()) { (accum, atom) =>
      if (atom.contains(targetClass) && atom.contains(targetHLE)) { // target predicate instances & negations thereof
        (accum._1 :+ atom, accum._2, accum._3)
      } else if (atom.contains("happensAt")){ // event predicate instances
        (accum._1, accum._2 :+ atom, accum._3)
      } else {
        if (!atom.contains("initiatedAt") && !atom.contains("terminatedAt")) { // all other domain predicates
          (accum._1, accum._2, accum._3 :+ atom)
        } else {
          (accum._1, accum._2, accum._3)
        }
      }
    }

    val t1 = System.nanoTime()
    println(s"Splitting labelAtoms, eventAtoms and domainAtoms time: ${(t1-t0)/1000000000.0} (${atoms.size} ASP atoms)")

    (fluentsMap, eventsMap, labelAtoms, eventAtoms, domainAtoms)
  }





  //val labelPreds = labelPreds_.map ( x => ECPredToMLN(x, fluents) )
  //val eventPreds = eventPreds_.map ( x => ECPredToMLN(x, events) )

  /*
  val domainPreds = domainPreds_.map { atom =>
    val lit = Literal.toLiteral1(atom)
    val terms = lit.terms.map(x => Constant(x.tostring.capitalize))
    Literal(functor = lit.functor.capitalize, terms = terms).tostring
  }
  */

  def atomToMLNConstant(s: String) = {
    val atom = Literal.toLiteral1(s).terms.head.asInstanceOf[Literal]
    val constant = (List(s"${atom.predSymbol.capitalize}") ++ atom.terms.map(x => x.tostring.capitalize)).mkString("_")
    (atom.tostring, constant)
  }

  def ECPredToMLN(s: String, atomConstantsMap: Map[String, String]) = {
    val (ss, isNegated) = if (s.contains("false_")) (s.split("false_")(1), true) else (s, false)
    val w = atomConstantsMap.keySet.find(x => s.contains(x)).getOrElse(throw new RuntimeException(s"Cannot fluent mapping for atom $s in fluents map: $atomConstantsMap"))
    val mlnAtom = ss.capitalize.replace(w, atomConstantsMap(w))
    if (isNegated) s"!$mlnAtom" else mlnAtom
  }



  def solve(ex: Example, target: String): List[AnswerSet] = {
    val f = Utils.getTempFile(s"asp2mln",".lp")
    Utils.writeToFile(f, "append")(p => List(ex.annotationASP.filter(_.contains(target)) ++ ex.narrativeASP) foreach p.println)
    ASP.solve(task = Globals.INFERENCE, aspInputFile = new File(f.getCanonicalPath))
  }

}

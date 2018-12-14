package oled.winnow

import java.io.File

import app.runutils.{Globals, RunningOptions}
import com.typesafe.scalalogging.LazyLogging
import logic.Examples.Example
import logic.{Clause, Literal, Theory}
import utils.ClauseImplicits._
import oled.distributed.Structures.ClauseStats
import oled.functions.SingleCoreOLEDFunctions
import utils.{ASP, Utils}

/**
  * Created by nkatz at 1/11/2018
  */

object MessageTypes {

  class ProcessBatchMsg(val theory: Theory, val batch: Example, val targetClass: String = "")

  class FinishedBatchMsg(val theory: Theory, val ruleScoringTime: Double, val newRuleTestTime: Double,
                         val compressRulesTime: Double, val expandRulesTime: Double,
                         val newRuleGenerationTime: Double, val BatchProcessingTime: Double, val targetClass: String)

}

object AuxStructures {

  class InertiaAtom(val atom: String, val weight: Double)

  class Inetrtia

}

object Test extends App {
  import scala.util.control.Breaks._
  val x = 1 to 100
  breakable {
    x foreach { x =>
      if (x < 50) println(x) else break
    }
  }
}



object AuxFuncs extends LazyLogging {


  def reduceWeights(ruleIds: Vector[String], idsMap: Map[String, Clause], learningRate: Double) = {
    ruleIds.foreach{ id =>
      val rule = idsMap(id)
      //val newWeight = rule.w*0.5
      //val newWeight = rule.w * Math.pow(Math.E, -2.0)
      val newWeight = rule.w * Math.pow(Math.E, (-1.0) * learningRate)
      rule.w = newWeight
    }
  }

  def increaseWeights(ruleIds: Vector[String], idsMap: Map[String, Clause], learningRate: Double) = {
    ruleIds.foreach{ id =>
      val rule = idsMap(id)
      //val newWeight = rule.w*2
      //val newWeight = rule.w*Math.pow(Math.E, 2.0)
      val newWeight = rule.w * Math.pow(Math.E, 1.0 * learningRate)
      rule.w = if (newWeight.isPosInfinity) rule.w else newWeight
    }
  }

  def increaseWeights(rules: List[Clause], learningRate: Double) = {
    rules foreach { r =>
      val newWeight = r.w * Math.pow(Math.E, 1.0 * learningRate)
      r.w = if (newWeight.isPosInfinity) r.w else newWeight
    }
  }

  /* Learns a new termination rule from an FP data point. */
  def generateNewExpert(dataBatch: Example, fpAtom: String, globals: Globals, what: String, totalWeight: Double) = {

    val strippedBatch = Example(annot = List(fpAtom), nar = dataBatch.narrative, _time = dataBatch.time)

    val _newRule = SingleCoreOLEDFunctions.generateNewRules(Theory(), strippedBatch, what, globals)

    if (_newRule.isEmpty) {
      Clause.empty
    } else {
      val newRule = _newRule.head

      // newRule here is an empty-bodied rule along with the newly-generated bottom clause.
      // Populate the newRule's refinements

      newRule.generateCandidateRefs(globals)

      newRule.w = totalWeight
      newRule.refinements.foreach(x => x.w = totalWeight)
      newRule
    }
  }


  /*
  *
  * Computes the groundings of the rules in the current theory and returns a map. The key to each of
  * the map is an inferred atom and the value is a triplet of three sets: Ids of rules that
  * initiate the key atom, terminated the atom, or not terminate the atom (the third is redundant
  * because it can be extracted from the complement of the firing termination rules. I'll have to go over his again).
  *
  * */

  def computeRuleGroundings(inps: RunningOptions, markedProgram: String, markedMap: Map[String, Clause], batch: String) = {

    val targetFluent = {
      // We can take the first one of the head modes (the target fluent is the same
      // regardless of whether the mode atom is an initiation of termination one).
      // Then, to get the target fluent, simply retrieve the first one of the 'terms' arg.

      val t = inps.globals.MODEHS.head.varbed.terms.head
      // The 'if' is for cases where the target pred is of the form initiatedAt(#fluent, +time), as in
      // initiatedAt(#fluent, +time) where fluent(leisure) is in the BK.
      // The 'else' is for compound fluents.
      if (t.isVariabe) Literal(functor = t._type) else inps.globals.MODEHS.head.varbed.terms.head.asInstanceOf[Literal]
      //modehs.head.varbed.terms.head.asInstanceOf[Literal]
    }.tostring

    /*
    val tpRule1 = s"tp(I, holdsAt($targetFluent,Te)) :- rule(I), example( holdsAt($targetFluent,Te) ), " +
      s"marked(I, initiatedAt($targetFluent,Ts) ), next(Ts,Te), time(Te), time(Ts)."
    //val tpRule2 = s"tp(I, holdsAt($targetFluent,Te)) :- rule(I), example( holdsAt($targetFluent,Te) ), not marked(I, terminatedAt($targetFluent,Ts) ), next(Ts,Te), time(Te), time(Ts)."
    //val tpRule3 = s"tp(inertia, holdsAt($targetFluent,Te)) :- example( holdsAt($targetFluent,Te) ), inertia( holdsAt($targetFluent,Ts) ), next(Ts,Te), time(Te), time(Ts)."
    val fpRule = s"fp(I, holdsAt($targetFluent,Te)) :- rule(I), not example( holdsAt($targetFluent,Te) ), " +
      s"marked(I, initiatedAt($targetFluent,Ts) ), next(Ts,Te), time(Te), time(Ts)."
    //val fpRule2 = s"fp(inertia, holdsAt($targetFluent,Te)) :- not example( holdsAt($targetFluent,Te) ), inertia( holdsAt($targetFluent,Ts) ), next(Ts,Te), time(Te), time(Ts)."
    val fnRule = s"fn(I, holdsAt($targetFluent,Te)) :- rule(I), example( holdsAt($targetFluent,Te) ), " +
      s"marked(I, terminatedAt($targetFluent,Ts) ), next(Ts,Te), time(Te), time(Ts)."
    */
    //------------------------------------------------------------------------------------------------------------
    // Using the directive below (note that it includes the "termination-tp rule tpRule2",
    // where we count as TPs correct NON-FIRINGS of termination rules) is wrong (and very dangerous).
    // Look what might happen:
    // Say we only have 2 initiation rules, the first of which fires at a time point and the second does not.
    // Because of the NaF in "not marked(...)" in tpRule2, we'll have a tp/2 instance for the second rule,
    // at a time point where the rule does not actually fire. We'll therefore have that the inferred atom
    // is initiated by two rules (so it will be predicted as holding - leaving the inertia expert aside,
    // since we have no non-firing initiation rules), while the true state of affairs might be that the
    // non-firing rule has a larger weight than the firing one, so we should predict that the rule does not hold!
    // I found this out while trying to predict with a hand-crafted theory and I got zero error!
    // Surprisingly, using this directive (before realizing the problems) is how I got the best results
    // (~ 70 mistakes in total, regardless of the order in which the videos were seen).
    //------------------------------------------------------------------------------------------------------------
    //val directives = s"\n$tpRule1\n$tpRule2\n$fpRule\n$fnRule"


    val initGrndRule = s"grounding(I, holdsAt(F,Te)) :- rule(I), fluent(F), marked(I, initiatedAt(F,Ts) ), next(Ts,Te), time(Te), time(Ts)."
    val termGrndRule = s"grounding(I, holdsAt(F,Te)) :- rule(I), fluent(F), marked(I, terminatedAt(F,Ts) ), next(Ts,Te), time(Te), time(Ts)."

    val directives = s"\n$initGrndRule\n$termGrndRule\n"

    val program = batch + markedProgram + "\n#include \""+inps.entryPath+"/bk.lp\"." + directives + "\n#show.\n#show grounding/2." //"\n#show.\n#show tp/2.\n#show fp/2.\n#show fn/2."
    val f2 = Utils.getTempFile(s"quick-and-dirty",".lp")
    Utils.writeToFile(f2, "append")(p => List(program) foreach p.println)
    val paaath = f2.getCanonicalPath
    val _result = ASP.solve(task = Globals.SCORE_RULES, aspInputFile = new File(paaath))

    val result = if (_result.nonEmpty) _result.head.atoms.toSet else Set[String]()

    /*
    val (tpAtoms, fpAtoms, fnAtoms) = result.foldLeft(Set[String](), Set[String](), Set[String]()) { (x, atom) =>
      val (a, b, c) = (x._1, x._2, x._3)
      if (atom.startsWith("tp")) (a + atom, b, c)
      else if (atom.startsWith("fp")) (a, b+atom, c)
      else if (atom.startsWith("fn")) (a,b,c+atom)
      else throw new RuntimeException("FUCK This shouldn't have happened")
    }
    */

    //val allInferredAtoms = tpAtoms ++ fpAtoms ++ fnAtoms

    val allInferredAtoms = result

    // We'll use a map to process the results. The keys will be actual holdsAt/2 atoms.
    // The values will be three lists of clauses:
    // initiatedBy, terminatedBy and notTerminatedBy. Each list contains the ids of the rules that
    // respectively initiate, terminate and not terminate the key atom

    val map = scala.collection.mutable.Map[String, (Vector[String], Vector[String])]()

    allInferredAtoms foreach { s =>

      val l = Literal.parse(s)
      val functor = l.functor
      val ruleId = l.terms.head.tostring
      val holdsAtAtom = l.terms.tail.head.tostring
      val actualRule = markedMap(ruleId)

      if (actualRule.head.functor.contains("initiated")) {
        if (map.keySet.contains(holdsAtAtom)) {
          map(holdsAtAtom) = (map(holdsAtAtom)._1 :+ ruleId, map(holdsAtAtom)._2)
        } else {
          map(holdsAtAtom) = (Vector(ruleId), Vector[String]())
        }
      } else {
        if (map.keySet.contains(holdsAtAtom)) {
          map(holdsAtAtom) = (map(holdsAtAtom)._1, map(holdsAtAtom)._2 :+ ruleId)
        } else {
          map(holdsAtAtom) = (Vector[String](), Vector(ruleId))
        }
      }
    }

    /*
    allInferredAtoms foreach { s =>
      val l = Literal.parse(s)
      val functor = l.functor
      val ruleId = l.terms.head.tostring
      val holdsAtAtom = l.terms.tail.head.tostring
      val actualRule = markedMap(ruleId)

      //if (map.keySet.contains(holdsAtAtom)) {
      if (functor == "tp") {
        // INITIATED TP
        if (actualRule.head.functor.contains("initiated")) {
          // then the atom is correctly derived by the initiation rule.
          // Add the rule id to the initiatedBy field of the map (the 1st one)
          if (map.keySet.contains(holdsAtAtom)) {
            map(holdsAtAtom) = (map(holdsAtAtom)._1 :+ ruleId, map(holdsAtAtom)._2, map(holdsAtAtom)._3)
          } else {
            map(holdsAtAtom) = (Vector(ruleId), Vector[String](), Vector[String]())
          }
          // TERMINATED TP
        } else {
          // then the rule is correctly not terminated by the termination rule.
          // Add the rule id to the notTerminatedBy field of the map (the 3rd one)
          if (map.keySet.contains(holdsAtAtom)) {
            map(holdsAtAtom) = (map(holdsAtAtom)._1, map(holdsAtAtom)._2, map(holdsAtAtom)._3 :+ ruleId)
          } else {
            map(holdsAtAtom) = (Vector[String](), Vector[String](), Vector(ruleId))
          }
        }
      } else if (functor == "fp") {
        // This can only happen with initiation rules. Add the atom to the initiatedBy field of the map.
        if (map.keySet.contains(holdsAtAtom)) {
          map(holdsAtAtom) = (map(holdsAtAtom)._1 :+ ruleId, map(holdsAtAtom)._2, map(holdsAtAtom)._3)
        } else {
          map(holdsAtAtom) = (Vector(ruleId), Vector[String](), Vector[String]())
        }
      } else if (functor == "fn") {
        // This can only happen with termination rules. Add the atom to the terminatedBy field of the map.
        if (map.keySet.contains(holdsAtAtom)) {
          map(holdsAtAtom) = (map(holdsAtAtom)._1, map(holdsAtAtom)._2 :+ ruleId, map(holdsAtAtom)._3)
        } else {
          map(holdsAtAtom) = (Vector[String](), Vector(ruleId), Vector[String]())
        }
      } else {
        throw new RuntimeException(s"Unexpected predicate symbol: $functor")
      }
    }
    */

    map

  }

















  /**
    *
    * @return The marked rules and the marked rule preds (e.g. rule(234234)) as a single string ready for ASP use.
    *         Also a map of ruleId --> rule
    */
  def marked(allRules: Vector[Clause], globals: Globals): (String, Map[String, Clause]) = {

    val allRulesMarked = allRules map (x => markedQuickAndDirty(x, globals))

    val hashCodesClausesMap = (allRules map (x => x.##.toString -> x)).toMap
    val rulePredicates = hashCodesClausesMap.keySet.map(x => s"rule($x). ").mkString("\n")
    (allRulesMarked.map(_.tostring).mkString("\n")+rulePredicates, hashCodesClausesMap)
  }

  // The head of a weighted rule is of the form: marked(ruleId, "weight", actualHead)
  def marked(c: Clause, globals: Globals) = {
    val cc = Clause(head=Literal(functor = "marked", terms=List(c.##.toString, s""""${c.w}"""", c.head)), body=c.withTypePreds(globals).body)
    cc.w = c.w
    cc
  }

  // The head of a weighted rule is of the form: marked(ruleId, actualHead)
  def markedQuickAndDirty(c: Clause, globals: Globals) = {
    val cc = Clause(head=Literal(functor = "marked", terms=List(c.##.toString, c.head)), body=c.withTypePreds(globals).body)
    cc.w = c.w
    cc
  }

}



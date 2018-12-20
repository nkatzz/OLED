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

  import scala.util.Random

  val vals = Vector(1, -1)

  def getRanomVector = {
    (1 to 20).map( x => Random.shuffle(vals).head ).toVector
  }

  def getRandomVectors = {
    (1 to 100).map(_ => getRanomVector).toVector
  }

  // in this strategy we get one prediction pre vector
  def individualVoting(vecs: Vector[Vector[Int]]) = {
    val votes = vecs.map{ x => x.sum.toDouble/x.length }
    votes.sum/votes.length
  }

  def collectiveVoting(vecs: Vector[Vector[Int]]) = {
    val all = vecs.flatten
    all.sum.toDouble/all.length
  }

  def vecs = getRandomVectors
  println(individualVoting(vecs))
  println(collectiveVoting(vecs))

}



object AuxFuncs extends LazyLogging {


  def reduceWeights(ruleIds: Vector[String], idsMap: Map[String, Clause], learningRate: Double) = {
    ruleIds.foreach { id =>
      val rule = idsMap(id)
      //val newWeight = rule.w*0.5
      //val newWeight = rule.w * Math.pow(Math.E, -2.0)
      val newWeight = rule.w * Math.pow(Math.E, (-1.0) * learningRate)
      rule.w = newWeight
      rule.updateRunningWeightAvg(newWeight)

      // for presenting analytics
      rule.updateWeightsBuffer(rule.w)
    }
  }

  def increaseWeights(ruleIds: Vector[String], idsMap: Map[String, Clause], learningRate: Double) = {
    ruleIds.foreach { id =>
      val rule = idsMap(id)
      //val newWeight = rule.w*2
      //val newWeight = rule.w*Math.pow(Math.E, 2.0)
      val newWeight = rule.w * Math.pow(Math.E, 1.0 * learningRate)
      rule.w = if (newWeight.isPosInfinity) rule.w else newWeight
      rule.updateRunningWeightAvg(rule.w)

      // for presenting analytics
      rule.updateWeightsBuffer(rule.w)
    }
  }

  def increaseWeights(rules: List[Clause], learningRate: Double) = {
    rules foreach { r =>
      val newWeight = r.w * Math.pow(Math.E, 1.0 * learningRate)
      r.w = if (newWeight.isPosInfinity) r.w else newWeight

      // for presenting analytics
      r.updateWeightsBuffer(r.w)
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


  def reportMistake(what: String, atom: String, inertiaWeight: Double,
                     firingInitRulesNum: Int, nonfiringInitRulesNum: Int,
                     firingTermRulesNum: Int, nonfiringTermRulesNum: Int,
                     initWeightSum: Double, termWeightSum: Double,
                     noinitWeightSum: Double, notermWeightSum: Double, logger: org.slf4j.Logger) = {

    logger.info(s"\nMade $what mistake for atom: $atom.\nInertia weight: " +
      s"$inertiaWeight\nFiring initiation rules: $firingInitRulesNum, sum of weight: $initWeightSum\nNon " +
      s"firing initiation rules: $nonfiringInitRulesNum, sum of weight: $noinitWeightSum\nFiring " +
      s"termination rules: $firingTermRulesNum, sum of weight: $termWeightSum\nNon firing termination " +
      s"rules: $nonfiringTermRulesNum, sum of weight: $notermWeightSum")

  }

  def predict(inertiaPrediction: Double, initPrediction: Double, termPrediction: Double, strongInertia: Boolean) = {

    if (strongInertia) {
      // This does not take the firing initiation rules into account when something holds by inertia.
      // In weakly initiated settings it tends to generate more FPs, because when a fluent is actually
      // weakly initiated, the non-firing initiation rules are a good indicator for whether the fluent persists.
      // On the other hand, this is necessary for strongly initiated fluents, to allow for the persistence of fluents.
      if (inertiaPrediction > 0.0) {
        val w = inertiaPrediction - termPrediction
        if (inertiaPrediction > termPrediction) (true, w) else (false, w)
      } else {
        // this "if" here is necessary. We don't want something to be detected
        // just because the initiation weight sum is greater than the termination weight sum.
        // We also want the initiation sum to be positive, so that the fluent is actually initiated.
        if (initPrediction > 0.0) {
          val w = initPrediction - termPrediction
          if (initPrediction > termPrediction) (true, w) else (false, w)
        } else {
          (false, 0.0)
        }
      }
    } else {
      // This often makes wrong predictions (FNs) because non-firing initiation rules take over inertia.
      // But is usually has better results (less FPs) in weakly-ininitated fluents.
      if (inertiaPrediction + initPrediction > 0.0) {
        if (termPrediction > 0) {
          val w = inertiaPrediction + initPrediction - termPrediction
          if (inertiaPrediction + initPrediction > termPrediction) (true, w) else (false, w)
        } else {
          (true, inertiaPrediction + initPrediction)
        }
      } else {
        (false, 0.0)
      }
    }
  }


  def normalizeWeights(inertiaExpert: Map[String, Double],
                       firingInitRules: Vector[Clause], nonFiringInitRules: Vector[Clause],
                       firingTermRules: Vector[Clause], nonFiringTermRules: Vector[Clause]) = {

    val inertiaWeights = inertiaExpert.values.sum

    val allRules = firingInitRules ++ nonFiringInitRules ++ firingTermRules ++ nonFiringTermRules

    val totalWeight = allRules.map(x => x.w).sum + inertiaWeights

    allRules foreach {x => x.w = x.w/totalWeight}

  }




  /*
  *
  * Creates a prediction for a single rule (expert) w.r.t. a single atom by combining the predictions
  * of a sub-expert committee (rule's specializations).
  *
  * firingRuleIds and nonFiringRuleIds are respectively the ids of rules that fire/don't fire w.r.t. to a
  * single atom (the atom for which we're getting a prediction).
  *
  * */

  def getRulePrediction(rule: Clause, firingRuleIds: Vector[String], nonFiringRuleIds: Vector[String]) = {

    val abstains = rule.body.isEmpty // don't use the top rule's opinion if the rule is too immature.

    val wholeCommittee = if (!abstains) rule.refinements :+ rule else rule.refinements

    val rulesToWeightsMap = wholeCommittee.map( x => x.## -> x.w ).toMap

    val yesWeight = {
      val yes = firingRuleIds.toSet.intersect(rulesToWeightsMap.keySet.map(_.toString)).map(_.toInt)
      yes.map(id => rulesToWeightsMap(id)).sum
    }

    val noWeight = {
      val no = nonFiringRuleIds.toSet.intersect(rulesToWeightsMap.keySet.map(_.toString)).map(_.toInt)
      no.map(id => rulesToWeightsMap(id)).sum
    }

    yesWeight - noWeight

  }

  def getRulePrediction1(rule: Clause, firingRuleIds: Vector[String], nonFiringRuleIds: Vector[String]) = {

    val ruleAbstains = rule.body.isEmpty // don't use the top rule's opinion if the rule is too immature.

    /*
    val bestSubExpert =
      if (! ruleAbstains) {
        (rule.refinements :+ rule).sortBy { x => (- x.w, - x.score, x.body.length+1) }.head
      } else {
        rule.refinements.sortBy { x => (- x.w, - x.score, x.body.length+1) }.head
      }
    */
    val sorted = (rule.refinements :+ rule).filter(z => z.score >= 0.2).sortBy { x => (- x.score, - x.w, x.body.length+1) }

    val bestExpert = if (sorted.nonEmpty) sorted.head else Clause.empty

    if (bestExpert == Clause.empty) {
      0.0
    } else {
      if (firingRuleIds.toSet.contains(bestExpert.##.toString)) {
        bestExpert.w
      } else if (nonFiringRuleIds.toSet.contains(bestExpert.##.toString)) {
        - bestExpert.w
      } else {
        throw new RuntimeException(s"\nRule:\n${rule.tostring}\nwith id: ${rule.##} " +
          s"is contained neither in the firing, nor the non-firing rules")
      }
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

  def computeRuleGroundings(inps: RunningOptions, markedProgram: String,
                            markedMap: Map[String, Clause], batch: String, trueAtoms: Set[String]) = {

    /*
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
    */

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
      //val functor = l.functor
      val ruleId = l.terms.head.tostring.toInt.toString
      val holdsAtAtom = l.terms.tail.head.tostring
      val actualRule = markedMap(ruleId)

      if (actualRule.head.functor.contains("initiated")) {
        if (map.keySet.contains(holdsAtAtom)) {
          map(holdsAtAtom) = (map(holdsAtAtom)._1 :+ ruleId, map(holdsAtAtom)._2)
        } else {
          map(holdsAtAtom) = (Vector(ruleId), Vector[String]())
        }
      } else if (actualRule.head.functor.contains("terminated")) {
        if (map.keySet.contains(holdsAtAtom)) {
          map(holdsAtAtom) = (map(holdsAtAtom)._1, map(holdsAtAtom)._2 :+ ruleId)
        } else {
          map(holdsAtAtom) = (Vector[String](), Vector(ruleId))
        }
      } else {
        throw new RuntimeException("Fuck! this shouldn't have happened")
      }
    }

    ///*
    trueAtoms foreach { atom =>
      if (!map.keySet.contains(atom)) {
        map(atom) = (Vector[String](), Vector[String]())
      }
    }
    //*/

    map

  }


  // Generates the final theory to use on the test set. The input is a list with
  // the initiation part in the haed and the termination part in the body
  def getFinalTheory(theory: List[Theory], useAvgWeights: Boolean = true, logger: org.slf4j.Logger) = {

    //theory = List(newInit, newTerm)
    //theory = List(newInit.clauses.flatMap(x => x.refinements :+ x), newTerm.clauses.flatMap(x => x.refinements :+ x))

    // Here somewhere, maybe you also want to try to filter
    // filter(p => p.score > inps.pruneThreshold)

    val oldInit = theory.head.clauses
    val oldTerm = theory.tail.head.clauses

    val newInit = oldInit.flatMap(x => x.refinements :+ x).
      filter(z => z.body.nonEmpty)//.filter(x => x.avgWeight > 0.0 && x.avgWeight < 3.0)

    val newTerm = oldTerm.flatMap(x => x.refinements :+ x).
      filter(z => z.body.nonEmpty)//.filter(x => x.avgWeight > 0.0 && x.avgWeight < 3.0)

    val _merged = Theory(newInit ++ newTerm)

    logger.info(s"\n\n\nPerforming test with:\n\n${_merged.showWithStats}")

    if (useAvgWeights) {
      newInit foreach { r =>
        r.w = r.avgWeight
        r.refinements foreach( r1 => r1.w = r1.avgWeight )
      }

      newTerm foreach { r =>
        r.w = r.avgWeight
        r.refinements foreach( r1 => r1.w = r1.avgWeight )
      }
    }

    (newInit, newTerm)

  }


  // This is wrong, a rule is scored based on its true/false groundings, not the final prediction
  ///*
  def updateRulesScore(prediction: String,
                       firingInitRules: Vector[Clause],
                       nonFiringInitRules: Vector[Clause],
                       firingTermRules: Vector[Clause],
                       nonFiringTermRules: Vector[Clause]) = {

    firingInitRules foreach (x => x.seenExmplsNum +=  1)
    nonFiringInitRules foreach (x => x.seenExmplsNum +=  1)
    firingTermRules foreach (x => x.seenExmplsNum +=  1)
    nonFiringTermRules foreach (x => x.seenExmplsNum +=  1)

    prediction match {

      // The master predicted positive and it actually is.
      case "TP" =>
        // Count 1 TP for all initiation rules that fire and all termination rules that do not.
        firingInitRules foreach (x => x.tps +=  1)
        nonFiringTermRules foreach (x => x.tps +=  1)

        // Count 1 FN for all initiation rules that do not fire and all termination rules that fire.
        nonFiringInitRules foreach (x => x.fns +=  1)
        firingTermRules foreach (x => x.fns +=  1)

      // The master predicted positive but its not.
      case "FP" =>
        // Count 1 FP for all initiation rules that fire.
        firingInitRules foreach (x => x.fps +=  1)

        // Count 1 TN for all initiation rules that do not fire.
        nonFiringInitRules foreach (x => x.tns +=  1)

        // Count 1 TP for all termination rules that do not fire.
        nonFiringTermRules foreach (x => x.tps +=  1)

        // Count 1 TN for all termination rules that fire.
        firingTermRules foreach (x => x.tns +=  1)

      // The master predicted negative, but it's actually positive.
      case "FN" =>
        // Count 1 FN for all initiation rules that do not fire and all termination rules that do.
        nonFiringInitRules foreach (x => x.fns +=  1)
        firingTermRules foreach (x => x.fns +=  1)

        // Count 1 TP for all initiation rules that fire and all termination rules that do not fire.
        firingInitRules foreach (x => x.tps +=  1)
        nonFiringTermRules foreach (x => x.tps +=  1)

      // The master predicted negative, and it actually is.
      case "TN" =>
        // Count 1 TN for all initiation rules that do not fire and all termination rules that fire
        nonFiringInitRules foreach (x => x.tns +=  1)
        firingTermRules foreach (x => x.tns +=  1)

        // Count 1 FP for all initiation rules that fire
        firingInitRules foreach (x => x.fps +=  1)

        // Count 1 TP for all termination rules that fire.
        firingTermRules foreach (x => x.tps +=  1)

      case _ => throw new RuntimeException(s"Recieved $prediction as the prediction while updating the rules' scores.")
    }

  }
  //*/

















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



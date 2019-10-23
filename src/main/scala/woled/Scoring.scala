package woled

import app.runutils.{Globals, RunningOptions}
import logic.{Clause, Literal}
import logic.Examples.Example
import utils.ASP
import xhail.Xhail

import scala.util.Random

/**
 * Created by nkatz on 11/10/19.
 */
object Scoring {

  /*val BK =
    """
      |%tps(X) :- X = #count {F,T: annotation(holdsAt(F,T)), inferred(holdsAt(F,T), true)}.
      |%fps(X) :- X = #count {F,T: not annotation(holdsAt(F,T)), inferred(holdsAt(F,T), true)}.
      |%fns(X) :- X = #count {F,T: annotation(holdsAt(F,T)), inferred(holdsAt(F,T), false)}.
      |
      |coverage_counts(TPs, FPs, FNs) :-
      |       TPs = #count {F,T: annotation(holdsAt(F,T)), inferred(holdsAt(F,T), true)},
      |       FPs = #count {F,T: not annotation(holdsAt(F,T)), inferred(holdsAt(F,T), true)},
      |       FNs = #count {F,T: annotation(holdsAt(F,T)), inferred(holdsAt(F,T), false)}.
      |
      |actually_initiated_correct(F, T, RuleId) :- fires(initiatedAt(F, T), RuleId), annotation(holdsAt(F, Te)), next(T, Te).
      |actually_initiated_incorrect(F, T, RuleId) :- fires(initiatedAt(F, T), RuleId), not annotation(holdsAt(F, Te)), next(T, Te).
      |inferred_initiated_correct(F, T, RuleId) :- actually_initiated_correct(F, T, RuleId), inferred(initiatedAt(F, T), true).
      |inferred_initiated_incorrect(F, T, RuleId) :- actually_initiated_incorrect(F, T, RuleId), inferred(initiatedAt(F, T), true).
      |
      |actual_init_tps(RuleId, X) :- initiated_rule_id(RuleId), X = #count {F,T: actually_initiated_correct(F, T, RuleId)}.
      |actual_init_fps(RuleId, X) :- initiated_rule_id(RuleId), X = #count {F,T: actually_initiated_incorrect(F, T, RuleId)}.
      |inferred_init_tps(RuleId, X) :- initiated_rule_id(RuleId), X = #count {F,T: inferred_initiated_correct(F, T , RuleId)}.
      |inferred_init_fps(RuleId, X) :- initiated_rule_id(RuleId), X = #count {F,T: inferred_initiated_incorrect(F, T, RuleId)}.
      |
      |result_init(RuleId, ActualTPs, ActualFPs, InferredTPs, InferredFPs, Mistakes) :-
      |             initiated_rule_id(RuleId),
      |             actual_init_tps(RuleId, ActualTPs),
      |             actual_init_fps(RuleId, ActualFPs),
      |             inferred_init_tps(RuleId, InferredTPs),
      |             inferred_init_fps(RuleId, InferredFPs),
      |             Mistakes = InferredTPs + InferredFPs - ActualTPs.
      |
      |actually_terminated_correct(F, T, RuleId) :- fires(terminatedAt(F, T), RuleId), annotation(holdsAt(F, T)), not annotation(holdsAt(F,Te)), next(T, Te).
      |actually_not_terminated_correct(F, T, RuleId) :- terminated_rule_id(RuleId), not fires(terminatedAt(F, T), RuleId), annotation(holdsAt(F, Te)), next(T, Te).
      |actually_terminated_incorrect(F, T , RuleId) :- fires(terminatedAt(F, T), RuleId), annotation(holdsAt(F, Te)), next(T, Te).
      |
      |inferred_terminated_correct(F, T, RuleId) :- actually_terminated_correct(F, T, RuleId), inferred(terminatedAt(F, T), true).
      |inferred_not_terminated_correct(F, T, RuleId) :- actually_not_terminated_correct(F, T, RuleId), inferred(terminatedAt(F, T), false).
      |inferred_terminated_incorrect(F, T , RuleId) :- actually_terminated_incorrect(F, T , RuleId), inferred(terminatedAt(F, T), true).
      |
      |actual_term_tps_1(RuleId, X) :- terminated_rule_id(RuleId), X = #count {F,T: actually_terminated_correct(F, T, RuleId)}.
      |actual_term_tps_2(RuleId, X) :- terminated_rule_id(RuleId), X = #count {F,T: actually_not_terminated_correct(F, T, RuleId)}.
      |actual_term_fps(RuleId, X) :- terminated_rule_id(RuleId), X = #count {F,T: actually_terminated_incorrect(F, T, RuleId)}.
      |inferred_term_tps_1(RuleId, X) :- terminated_rule_id(RuleId), X = #count {F,T: inferred_terminated_correct(F, T, RuleId)}.
      |inferred_term_tps_2(RuleId, X) :- terminated_rule_id(RuleId), X = #count {F,T: inferred_not_terminated_correct(F, T, RuleId)}.
      |inferred_term_fps(RuleId, X) :- terminated_rule_id(RuleId), X = #count {F,T: inferred_terminated_incorrect(F, T, RuleId)}.
      |
      |result_term(RuleId, ActualTPs, ActualFPs, InferredTPs, InferredFPs, Mistakes) :-
      |             terminated_rule_id(RuleId),
      |             actual_term_tps_1(RuleId, ActualTPs1),
      |             actual_term_tps_2(RuleId, ActualTPs2),
      |             ActualTPs = ActualTPs1 + ActualTPs2,
      |             actual_term_fps(RuleId, ActualFPs),
      |             inferred_term_tps_1(RuleId, InferredTPs1),
      |             inferred_term_tps_2(RuleId, InferredTPs2),
      |             InferredTPs = InferredTPs1 + InferredTPs2,
      |             inferred_term_fps(RuleId, InferredFPs),
      |             Mistakes = InferredTPs + InferredFPs - ActualTPs.
      |
      |#show.
      |#show coverage_counts/3.
      |#show result_init/6.
      |#show result_term/6.
      |#show total_groundings/1.
      |
      |""".stripMargin*/





  /*val BK =
    """
      |%tps(X) :- X = #count {F,T: annotation(holdsAt(F,T)), inferred(holdsAt(F,T), true)}.
      |%fps(X) :- X = #count {F,T: not annotation(holdsAt(F,T)), inferred(holdsAt(F,T), true)}.
      |%fns(X) :- X = #count {F,T: annotation(holdsAt(F,T)), inferred(holdsAt(F,T), false)}.
      |
      |coverage_counts(TPs, FPs, FNs) :-
      |       TPs = #count {F,T: annotation(holdsAt(F,T)), inferred(holdsAt(F,T), true)},
      |       FPs = #count {F,T: not annotation(holdsAt(F,T)), inferred(holdsAt(F,T), true)},
      |       FNs = #count {F,T: annotation(holdsAt(F,T)), inferred(holdsAt(F,T), false)}.
      |
      |actually_initiated_correct(F, T, RuleId) :- fires(initiatedAt(F, T), RuleId), annotation(holdsAt(F, Te)), next(T, Te).
      |actually_initiated_incorrect(F, T, RuleId) :- fires(initiatedAt(F, T), RuleId), not annotation(holdsAt(F, Te)), next(T, Te).
      |inferred_initiated_correct(F, T, RuleId) :-
      |         actually_initiated_correct(F, T, RuleId),
      |         inferred(initiatedAt(F, T), true),
      |         inferred(holdsAt(F, T), true).
      |         %inferred(holdsAt(F, Te), true), % These generate much worse results.
      |         %next(T,Te).
      |inferred_initiated_incorrect(F, T, RuleId) :-
      |         actually_initiated_incorrect(F, T, RuleId),
      |         inferred(initiatedAt(F, T), true),
      |         inferred(holdsAt(F, T), false).
      |         %inferred(holdsAt(F, Te), false), % These generate much worse results.
      |         %next(T,Te).
      |
      |actual_init_tps(RuleId, X) :- initiated_rule_id(RuleId), X = #count {F,T: actually_initiated_correct(F, T, RuleId)}.
      |actual_init_fps(RuleId, X) :- initiated_rule_id(RuleId), X = #count {F,T: actually_initiated_incorrect(F, T, RuleId)}.
      |inferred_init_tps(RuleId, X) :- initiated_rule_id(RuleId), X = #count {F,T: inferred_initiated_correct(F, T , RuleId)}.
      |inferred_init_fps(RuleId, X) :- initiated_rule_id(RuleId), X = #count {F,T: inferred_initiated_incorrect(F, T, RuleId)}.
      |
      |result_init(RuleId, ActualTPs, ActualFPs, InferredTPs, InferredFPs, Mistakes) :-
      |             initiated_rule_id(RuleId),
      |             actual_init_tps(RuleId, ActualTPs),
      |             actual_init_fps(RuleId, ActualFPs),
      |             inferred_init_tps(RuleId, InferredTPs),
      |             inferred_init_fps(RuleId, InferredFPs),
      |             Mistakes = InferredTPs + InferredFPs - ActualTPs.
      |
      |% actually_terminated_correct(F, T, RuleId) :- fires(terminatedAt(F, T), RuleId), annotation(holdsAt(F, T)), not annotation(holdsAt(F,Te)), next(T, Te).
      |actually_terminated_correct(F, T, RuleId) :- fires(terminatedAt(F, T), RuleId), not annotation(holdsAt(F,Te)), next(T, Te).
      |actually_not_terminated_correct(F, T, RuleId) :- terminated_rule_id(RuleId), not fires(terminatedAt(F, T), RuleId), annotation(holdsAt(F, Te)), next(T, Te).
      |actually_terminated_incorrect(F, T , RuleId) :- fires(terminatedAt(F, T), RuleId), annotation(holdsAt(F, Te)), next(T, Te).
      |
      |inferred_terminated_correct(F, T, RuleId) :- actually_terminated_correct(F, T, RuleId), inferred(terminatedAt(F, T), true).
      |inferred_not_terminated_correct(F, T, RuleId) :- actually_not_terminated_correct(F, T, RuleId), inferred(terminatedAt(F, T), false).
      |inferred_terminated_incorrect(F, T , RuleId) :- actually_terminated_incorrect(F, T , RuleId), inferred(terminatedAt(F, T), true).
      |
      |%% These do not seem to work. They generate crazy results.
      |%*
      |inferred_terminated_correct(F, T, RuleId) :-
      |             actually_terminated_correct(F, T, RuleId),
      |             inferred(terminatedAt(F, T), true),
      |             inferred(holdsAtAt(F, T), true),
      |             inferred(holdsAtAt(F, Te), false),
      |             next(T, Te).
      |inferred_not_terminated_correct(F, T, RuleId) :-
      |             actually_not_terminated_correct(F, T, RuleId),
      |             inferred(terminatedAt(F, T), false),
      |             inferred(holdsAt(F, Te), true),
      |             next(T, Te).
      |inferred_terminated_incorrect(F, T , RuleId) :-
      |             actually_terminated_incorrect(F, T , RuleId),
      |             inferred(terminatedAt(F, T), true),
      |             inferred(holdsAt(F, Te), true),
      |             next(T, Te).
      |*%
      |
      |actual_term_tps_1(RuleId, X) :- terminated_rule_id(RuleId), X = #count {F,T: actually_terminated_correct(F, T, RuleId)}.
      |actual_term_tps_2(RuleId, X) :- terminated_rule_id(RuleId), X = #count {F,T: actually_not_terminated_correct(F, T, RuleId)}.
      |actual_term_fps(RuleId, X) :- terminated_rule_id(RuleId), X = #count {F,T: actually_terminated_incorrect(F, T, RuleId)}.
      |inferred_term_tps_1(RuleId, X) :- terminated_rule_id(RuleId), X = #count {F,T: inferred_terminated_correct(F, T, RuleId)}.
      |inferred_term_tps_2(RuleId, X) :- terminated_rule_id(RuleId), X = #count {F,T: inferred_not_terminated_correct(F, T, RuleId)}.
      |inferred_term_fps(RuleId, X) :- terminated_rule_id(RuleId), X = #count {F,T: inferred_terminated_incorrect(F, T, RuleId)}.
      |
      |result_term(RuleId, ActualTPs, ActualFPs, InferredTPs, InferredFPs, Mistakes) :-
      |             terminated_rule_id(RuleId),
      |             actual_term_tps_1(RuleId, ActualTPs1),
      |             % actual_term_tps_2(RuleId, ActualTPs2),
      |             % ActualTPs = ActualTPs1 + ActualTPs2,
      |             ActualTPs = ActualTPs1,
      |             actual_term_fps(RuleId, ActualFPs),
      |             inferred_term_tps_1(RuleId, InferredTPs1),
      |             %inferred_term_tps_2(RuleId, InferredTPs2),
      |             %InferredTPs = InferredTPs1 + InferredTPs2,
      |             InferredTPs = InferredTPs1,
      |             inferred_term_fps(RuleId, InferredFPs),
      |             Mistakes = InferredTPs + InferredFPs - ActualTPs.
      |
      |#show.
      |#show coverage_counts/3.
      |#show result_init/6.
      |#show result_term/6.
      |#show total_groundings/1.
      |
      |""".stripMargin*/






  val BK =
    """
      |%tps(X) :- X = #count {F,T: annotation(holdsAt(F,T)), inferred(holdsAt(F,T), true)}.
      |%fps(X) :- X = #count {F,T: not annotation(holdsAt(F,T)), inferred(holdsAt(F,T), true)}.
      |%fns(X) :- X = #count {F,T: annotation(holdsAt(F,T)), inferred(holdsAt(F,T), false)}.
      |
      |coverage_counts(TPs, FPs, FNs) :-
      |       TPs = #count {F,T: annotation(holdsAt(F,T)), inferred(holdsAt(F,T), true)},
      |       FPs = #count {F,T: not annotation(holdsAt(F,T)), inferred(holdsAt(F,T), true)},
      |       FNs = #count {F,T: annotation(holdsAt(F,T)), not startTime(T), inferred(holdsAt(F,T), false)}.
      |
      |actual_initiated_true_grounding(F, T, RuleId) :-
      |          fluent(F), % This is necessary for correct scoring
      |          fires(initiatedAt(F, T), RuleId),
      |          annotation(holdsAt(F, Te)),
      |          next(T, Te).
      |
      |%actual_initiated_true_grounding(F, T, RuleId) :-
      |%          fluent(F), % This is necessary for correct scoring
      |%          fires(initiatedAt(F, T), RuleId),
      |%          annotation(holdsAt(F, T)),
      |%          endTime(T).
      |
      |actual_initiated_false_grounding(F, T, RuleId) :-
      |          fluent(F), % This is necessary for correct scoring
      |          fires(initiatedAt(F, T), RuleId),
      |          not annotation(holdsAt(F, Te)), next(T, Te).
      |
      |inferred_initiated_true_grounding(F, T, RuleId) :-
      |          fluent(F), % This is necessary for correct scoring
      |          initiated_rule_id(RuleId),
      |          fires(initiatedAt(F, T), RuleId),
      |          inferred(initiatedAt(F, T), true).
      |
      |result_init(RuleId, ActualTrueGroundings, ActualFalseGroundings, InferredTrueGroundings, Mistakes) :-
      |           initiated_rule_id(RuleId),
      |           ActualTrueGroundings = #count {F,T: actual_initiated_true_grounding(F, T, RuleId)},
      |           InferredTrueGroundings = #count {F,T: inferred_initiated_true_grounding(F, T , RuleId)},
      |           ActualFalseGroundings = #count {F,T: actual_initiated_false_grounding(F, T, RuleId)},
      |           Mistakes = InferredTrueGroundings - ActualTrueGroundings.
      |
      |actually_terminated_true_grounding(F, T, RuleId) :-
      |          fluent(F), % This is necessary for correct scoring
      |          fires(terminatedAt(F, T), RuleId),
      |          not annotation(holdsAt(F,Te)), next(T, Te).
      |
      |actually_terminated_true_grounding(F, T, RuleId) :- % This is necessary for correct scoring...
      |          fluent(F), % This is necessary for correct scoring
      |          fires(terminatedAt(F, T), RuleId),
      |          endTime(T),
      |          not annotation(holdsAt(F,T)).
      |
      |actually_terminated_false_grounding(F, T, RuleId) :-
      |          fluent(F), % This is necessary for correct scoring
      |          fires(terminatedAt(F, T), RuleId),
      |          annotation(holdsAt(F,Te)), next(T, Te).
      |
      |inferred_terminated_true_grounding(F, T, RuleId) :-
      |          fluent(F), % This is necessary for correct scoring
      |          terminated_rule_id(RuleId),
      |          fires(terminatedAt(F, T), RuleId),
      |          inferred(terminatedAt(F, T), true).
      |
      |
      |actual_term_tps(RuleId, X) :- terminated_rule_id(RuleId), X = #count {F,T: actually_terminated_true_grounding(F, T, RuleId)}.
      |inferred_term_tps(RuleId, X) :- terminated_rule_id(RuleId), X = #count {F,T: inferred_terminated_true_grounding(F, T, RuleId)}.
      |
      |result_term(RuleId, ActualTrueGroundings, ActualFalseGroundings, InferredTrueGroundings, Mistakes) :-
      |             terminated_rule_id(RuleId),
      |             ActualTrueGroundings = #count {F,T: actually_terminated_true_grounding(F, T, RuleId)},
      |             InferredTrueGroundings = #count {F,T: inferred_terminated_true_grounding(F, T, RuleId)},
      |             ActualFalseGroundings = #count {F,T: actually_terminated_false_grounding(F, T, RuleId)},
      |             Mistakes = InferredTrueGroundings - ActualTrueGroundings.
      |
      |
      |#show.
      |#show coverage_counts/3.
      |#show result_init/5.
      |#show result_term/5.
      |#show total_groundings/1.
      |
      |
      |""".stripMargin





  def scoreAndUpdateWeights(data: Example, inferredState: Map[String, Boolean], rules: Vector[Clause], inps: RunningOptions, logger: org.slf4j.Logger) = {

    val bk = BK

    val zipped = rules zip (1 to rules.length)
    val ruleIdsMap = zipped.map(x => x._2 -> x._1).toMap

    val ruleIdPreds = {
      ruleIdsMap.map{ case (id, rule) => if(rule.head.functor == "initiatedAt") s"initiated_rule_id($id)." else s"terminated_rule_id($id)." }
    } mkString(" ")

    val metaRules = ruleIdsMap.foldLeft(Vector[String]()) { (accum, r) =>
      val (ruleId, rule) = (r._1, r._2)
      val typeAtoms = rule.toLiteralList.flatMap(x => x.getTypePredicates(inps.globals)).distinct.map(x => Literal.parse(x))
      val metaRule = s"fires(${rule.head.tostring}, $ruleId) :- ${(rule.body ++ typeAtoms).map(_.tostring).mkString(",")}."
      accum :+ metaRule
    }

    val totalExmplsCount = {
      val targetPred = inps.globals.EXAMPLE_PATTERNS.head
      val tpstr = targetPred.tostring
      val vars = targetPred.getVars.map(x => x.name).mkString(",")
      val typePreds = targetPred.getTypePredicates(inps.globals).mkString(",")

      val groundingsRule = s"grounding($tpstr) :- $typePreds, X0!=X1."
      val groundingsCountRule = s"total_groundings(X) :- X = #count {Y: grounding(Y)}."

      //s"total_groundings(X) :- X = #count {$vars: $tpstr: $typePreds} .\n"
      s"$groundingsRule\n$groundingsCountRule"
    }

    val observationAtoms = data.narrative.map(_+".")
    val annotationAtoms = data.annotation.map(x => s"annotation($x).")
    val inferredAtoms = inferredState.map{ case (k, v) => s"inferred($k,$v)." }
    val include = s"""#include "${inps.globals.BK_WHOLE_EC}"."""

    val metaProgram = {
      Vector("% Annotation Atoms:\n", annotationAtoms.mkString(" "),
        "\n% Inferred Atoms:\n", inferredAtoms.mkString(" "),
        "\n% Observation Atoms:\n", observationAtoms.mkString(" "),
        "\n% Marked Rules:\n", metaRules.mkString("\n")+ruleIdPreds,
        "\n% Meta-rules for Scoring:\n", s"$include\n", totalExmplsCount, bk)
    }

    /* SOLVE */
    val f = woled.Utils.dumpToFile(metaProgram)
    val t = ASP.solve(Globals.INFERENCE, aspInputFile=f)
    val answer = if (t.nonEmpty) t.head.atoms else Nil

    /* PARSE RESULTS */
    val (batchTPs, batchFPs, batchFNs, totalGroundings, rulesResults) = answer.foldLeft(0,0,0,0,Vector.empty[String]) { (x, y) =>
      if (y.startsWith("total_groundings")) {
        val num = y.split("\\(")(1).split("\\)")(0).toInt
        (x._1, x._2, x._3, num, x._5)
      } else if (y.startsWith("coverage_counts")) {
        val split = y.split(",")
        val tps = split(0).split("\\(")(1)
        val fps = split(1)
        val fns = split(2).split("\\)")(0)
        (tps.toInt, fps.toInt, fns.toInt, x._4, x._5)
      } else {
        (x._1, x._2, x._3, x._4, x._5 :+ y)
      }
    }

    var prevTotalWeightVector = Vector.empty[Double] // used for the experts update
    var _rules = Vector.empty[Clause]           // used for the experts update

    /* UPDATE WEIGHTS */
    rulesResults foreach { x =>
      val split = x.split(",")
      val ruleId = split(0).split("\\(")(1).toInt
      val actualTrueGroundings = split(1).toInt
      val actualFalseGroundings = split(2).toInt
      val inferredTrueGroundings = split(3).toInt
      val mistakes = split(4).split("\\)")(0).toInt

      val rule = ruleIdsMap(ruleId)

      rule.mistakes += mistakes

      val prevWeight = rule.mlnWeight

      println(s"Before: ${rule.mlnWeight}")

      prevTotalWeightVector = prevTotalWeightVector :+ prevWeight // used for the experts update
      _rules = rules :+ rule                           // used for the experts update

      // Adagrad
      /*val lambda = inps.adaRegularization //0.001 // 0.01 default
      val eta  = inps.adaLearnRate//1.0 // default
      val delta  = inps.adaGradDelta//1.0
      val currentSubgradient = mistakes
      rule.subGradient += currentSubgradient * currentSubgradient
      val coefficient = eta / (delta + math.sqrt(rule.subGradient))
      val value = rule.mlnWeight - coefficient * currentSubgradient
      val difference = math.abs(value) - (lambda * coefficient)
      if (difference > 0) rule.mlnWeight = if (value >= 0) difference else -difference
      else rule.mlnWeight = 0.0*/

      // Experts:
      var newWeight = if (totalGroundings!=0) rule.mlnWeight * Math.pow(0.8, rule.mistakes/totalGroundings) else rule.mlnWeight * Math.pow(0.8, rule.mistakes)

      if (newWeight.isNaN) {
        val stop = "stop"
      }

      if (newWeight == 0.0 | newWeight.isNaN) newWeight = 0.00000001
      rule.mlnWeight = if(newWeight.isPosInfinity) rule.mlnWeight else newWeight





      println(s"After: ${rule.mlnWeight}")


      /*if (prevWeight != rule.mlnWeight) {
        logger.info(s"\nPrevious weight: $prevWeight, current weight: ${rule.mlnWeight}, actualTPs: $actualTrueGroundings, actualFPs: $actualFalseGroundings, inferredTPs: $inferredTrueGroundings, mistakes: $mistakes\n${rule.tostring}")
      }*/

      rule.tps += actualTrueGroundings
      rule.fps += actualFalseGroundings
    }

    val prevTotalWeight = prevTotalWeightVector.sum
    val _newTotalWeight = _rules.map(x => x.mlnWeight).sum
    val newTotalWeight = _newTotalWeight
    rules.foreach(x => x.mlnWeight = x.mlnWeight * (prevTotalWeight/newTotalWeight))

    val newNewTotalWeight = _rules.map(x => x.mlnWeight).sum

    if (newNewTotalWeight.isNaN) {
      val stop = "stop"
    }

    println(s"Before | After: $prevTotalWeight | $newNewTotalWeight")

    (batchTPs, batchFPs, batchFNs, totalGroundings)
  }



  def generateNewRules(fps: Map[Int, Set[Literal]], fns: Map[Int, Set[Literal]], batch: Example,
                       inps: RunningOptions, logger: org.slf4j.Logger) = {

    val (initTopRules, termTopRules) = (inps.globals.state.initiationRules, inps.globals.state.terminationRules)

    val fpSeedAtoms = fps.map(x => x._1 -> x._2.map(x => Literal(functor = "terminatedAt", terms = x.terms)))
    val fnSeedAtoms = fns.map(x => x._1 -> x._2.map(x => Literal(functor = "initiatedAt", terms = x.terms)))

    // These should be done in parallel...
    val initBCs = if(fnSeedAtoms.nonEmpty) pickSeedsAndGenerate(fnSeedAtoms, batch, inps, 3, initTopRules.toVector, logger) else Set.empty[Clause]
    val termBCs = if(fpSeedAtoms.nonEmpty) pickSeedsAndGenerate(fpSeedAtoms, batch, inps, 3, termTopRules.toVector, logger) else Set.empty[Clause]

    val newRules = (inp: Set[Clause]) => {
      inp map { rule =>
        val c = Clause(head=rule.head, body = List())
        c.addToSupport(rule)
        c
      }
    }

    val initNewRules = newRules(initBCs)
    val termNewRules = newRules(termBCs)

    // These rules are generated to correct current mistakes, so set their weight to 1, sot they are applied immediately.
    initNewRules foreach (x => x.mlnWeight = 1.0)
    termNewRules foreach (x => x.mlnWeight = 1.0)

    (initNewRules, termNewRules)
  }



  def pickSeedsAndGenerate(mistakeAtomsGroupedByTime: Map[Int, Set[Literal]], batch: Example, inps: RunningOptions,
                           numOfTrials: Int, currentTopRules: Vector[Clause], logger: org.slf4j.Logger) = {


    def generateBC(seedAtom: String, batch: Example, inps: RunningOptions) = {
      val examples = batch.toMapASP
      val f = (x: String) => if (x.endsWith(".")) x else s"$x."
      val interpretation = examples("annotation").map(x => s"${f(x)}") ++ examples("narrative").map(x => s"${f(x)}")
      val infile = woled.Utils.dumpToFile(interpretation)
      val (_, kernel) = Xhail.generateKernel(List(seedAtom), "", examples, infile, inps.globals.BK_WHOLE, inps.globals)
      infile.delete()
      kernel
    }

    val random = new Random

    val existingBCs = currentTopRules.flatMap(x => x.supportSet.clauses).toSet

    val newBCs = (1 to numOfTrials).foldLeft(existingBCs, Set.empty[Clause]) { (x, _) =>
      val _newBCs = x._2
      val seed = random.shuffle(mistakeAtomsGroupedByTime).head
      val seedAtoms = seed._2.map(_.tostring)

      val _BCs = seedAtoms.flatMap(generateBC(_, batch, inps))
      val BCs = _BCs.filter(newBC => !(existingBCs++_newBCs).exists(oldBC => newBC.thetaSubsumes(oldBC)))

      if (BCs.nonEmpty) {
        logger.info(s"\nStart growing new rules from BCs:\n${BCs.map(_.tostring).mkString("\n")}")
        (x._1 ++ BCs, x._2 ++ BCs)
      } else {
        logger.info("Generated BCs already existed in the bottom theory.")
        x
      }
    }
    newBCs._2
  }



  def getMistakes(inferredState:  Map[String, Boolean], batch: Example) = {
    val annotation = batch.annotation.toSet
    val (tps, fps, fns) = inferredState.foldLeft(Set.empty[String], Set.empty[String], Set.empty[String]) { (accum, y) =>
      val (atom, predictedTrue) = (y._1, y._2)
      if (atom.startsWith("holds")) {
        if (predictedTrue) {
          if (!annotation.contains(atom)) (accum._1, accum._2 + atom, accum._3) // FP
          else (accum._1 + atom, accum._2, accum._3) // TP, we don't care.
        } else {
          if (annotation.contains(atom)) (accum._1, accum._2, accum._3 + atom) // FN
          else accum // TN, we don't care.
        }
      } else {
        accum
      }
    }

    val (tpCounts, fpCounts, fnCounts) = (tps.size, fps.size, fns.size)
    val groupedByTime = (in: Set[String]) => in.map(x => Literal.parse(x)).groupBy(x => x.terms.tail.head.tostring.toInt)
    (groupedByTime(fps), groupedByTime(fns), tpCounts, fpCounts, fnCounts)
  }







}

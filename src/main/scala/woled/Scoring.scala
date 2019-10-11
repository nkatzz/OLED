package woled

import app.runutils.{Globals, RunningOptions}
import logic.{Clause, Literal}
import logic.Examples.Example
import utils.ASP

/**
 * Created by nkatz on 11/10/19.
 */
object Scoring {

  val BK =
    """
      |tps(X) :- X = #count {F,T: evidence(holdsAt(F,T)), inferred(holdsAt(F,T), true)}.
      |fps(X) :- X = #count {F,T: not evidence(holdsAt(F,T)), inferred(holdsAt(F,T), true)}.
      |fns(X) :- X = #count {F,T: evidence(holdsAt(F,T)), inferred(holdsAt(F,T), false)}.
      |
      |actually_initiated_correct(F, T, RuleId) :- fires(initiatedAt(F, T), RuleId), evidence(holdsAt(F, Te)), next(T, Te).
      |actually_initiated_incorrect(F, T, RuleId) :- fires(initiatedAt(F, T), RuleId), not evidence(holdsAt(F, Te)), next(T, Te).
      |inferred_initiated_correct(F, T, RuleId) :- actually_initiated_correct(F, T, RuleId), inferred(initiatedAt(F, T), true).
      |inferred_initiated_incorrect(F, T, RuleId) :- actually_initiated_incorrect(F, T, RuleId), inferred(initiatedAt(F, T), true).
      |
      |actual_init_tps(RuleId, X) :- initiated_rule_id(RuleId), X = #count {F,T: actually_initiated_correct(F, T, RuleId)}.
      |actual_init_fps(RuleId, X) :- initiated_rule_id(RuleId), X = #count {F,T: actually_initiated_incorrect(F, T, RuleId)}.
      |inferred_init_tps(RuleId, X) :- initiated_rule_id(RuleId), X = #count {F,T: inferred_initiated_correct(F, T , RuleId)}.
      |inferred_init_fps(RuleId, X) :- initiated_rule_id(RuleId), X = #count {F,T: inferred_initiated_incorrect(F, T, RuleId)}.
      |
      |result_init(RuleId, ActualTPs, ActualFPs, InferredTPs, InferredFPs, Mistakes) :-
      |             initiated_rule_id(RuleId), actual_init_tps(RuleId, ActualTPs),
      |             actual_init_fps(RuleId, ActualFPs), inferred_init_tps(RuleId, InferredTPs),
      |             inferred_init_fps(RuleId, InferredFPs), Mistakes = InferredTPs + InferredFPs - ActualTPs.
      |
      |actually_terminated_correct(F, T, RuleId) :- fires(terminatedAt(F, T), RuleId), evidence(holdsAt(F, T)), not evidence(holdsAt(F,Te)), next(T, Te).
      |actually_not_terminated_correct(F, T, RuleId) :- terminated_rule_id(RuleId), not fires(terminatedAt(F, T), RuleId), evidence(holdsAt(F, Te)), next(T, Te).
      |actually_terminated_incorrect(F, T , RuleId) :- fires(terminatedAt(F, T), RuleId), evidence(holdsAt(F, Te)), next(T, Te).
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
      |             terminated_rule_id(RuleId), actual_term_tps_1(RuleId, ActualTPs1), actual_term_tps_1(RuleId, ActualTPs2), ActualTPs = ActualTPs1 + ActualTPs2,
      |             actual_term_fps(RuleId, ActualFPs),
      |             inferred_term_tps_1(RuleId, InferredTPs1), inferred_term_tps_2(RuleId, InferredTPs2), InferredTPs = InferredTPs1 + InferredTPs2,
      |             inferred_term_fps(RuleId, InferredFPs),
      |             Mistakes = InferredTPs + InferredFPs - ActualTPs.
      |
      |#show.
      |#show tps/1.
      |#show fps/1.
      |#show fns/1.
      |#show result_init/6.
      |#show result_term/6.
      |#show total_groundings/1.
      |
      |""".stripMargin

  def score(data: Example, inferredState: Map[String, Boolean], rules: Vector[Clause], inps: RunningOptions) = {

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
      val groundingsRule = s"grnd($tpstr) :- $typePreds."

      s"total_groundings(X) :- X = #count {$vars: $tpstr, $typePreds} .\n"
    }

    val observationAtoms = data.narrative.map(_+".")
    val evidenceAtoms = data.annotation.map(x => s"evidence($x).")
    val inferredAtoms = inferredState.map{ case (k, v) => s"inferred($k,$v)." }
    val include = s"""#include "${inps.globals.BK_WHOLE_EC}"."""

    val metaProgram = {
      Vector("% Evidence Atoms:\n", evidenceAtoms.mkString(" "),
        "\n% Inferred Atoms:\n", inferredAtoms.mkString(" "),
        "\n% Observation Atoms:\n", observationAtoms.mkString(" "),
        "\n% Marked Rules:\n", metaRules.mkString("\n")+ruleIdPreds,
        "\n% Meta-rules for Scoring:\n", s"$include\n", totalExmplsCount, bk)
    }

    val f = woled.Utils.dumpToFile(metaProgram)
    val t = ASP.solve(Globals.INFERENCE, aspInputFile=f)
    val answer = if (t.nonEmpty) t.head.atoms else throw  new RuntimeException("Empty answer from scoring program.") // Nill

    println(answer)
  }



}

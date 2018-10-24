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

package app.runutils

import com.typesafe.scalalogging.LazyLogging
import logic.Literal
import logic.Modes.ModeAtom

/**
  * Created by nkatz at 18/10/2018
  */

object BKHandling extends LazyLogging {

  def getCoverageDirectives(varbedExmplPatterns: List[String]) = {
    //val varbedExmplPatterns = for (x <- eps2) yield x.varbed.tostring
    val tps = (x: String) => s"\ntps($x):- $x, example($x).\n"
    val tpsMarked = (x: String) => s"\ntps(I,$x):- marked(I,$x), example($x), rule(I).\n"
    val fps = (x: String) => s"\nfps($x):- $x, not example($x).\n"
    val fpsMarked = (x: String) => s"\nfps(I,$x):- marked(I,$x), not example($x), rule(I).\n"
    val fns = (x: String) => s"\nfns($x) :- example($x), not $x.\n"
    val fnsMarked = (x: String) => s"\nfns(I,$x):- not marked(I,$x), example($x), rule(I).\n"
    val coverAllPositivesConstraint = (x: String) => s"\n:- example($x), not $x.\n"
    val excludeAllNegativesConstraint = (x: String) => s"\n:- $x, not example($x).\n"
    val (tp,fp,fn,tpm,fpm,fnm,allpos,allnegs) =
      varbedExmplPatterns.
        foldLeft(List[String](),List[String](),List[String](),List[String](),
          List[String](),List[String](),List[String](),List[String]()){ (x,y) =>
          (x._1 :+ tps(y) ,x._2 :+ fps(y), x._3 :+ fns(y),
            x._4 :+ tpsMarked(y), x._5 :+ fpsMarked(y) ,
            x._6 :+ fnsMarked(y), x._7 :+ coverAllPositivesConstraint(y), x._8 :+ excludeAllNegativesConstraint(y))
        }
    val mkString = (x: List[String]) => x.mkString("\n")
    (mkString(tp), mkString(fp), mkString(fn), mkString(tpm), mkString(fpm), mkString(fnm), mkString(allpos), mkString(allnegs))
  }

  def tpsCountRules(eps: List[Literal]) = {
    eps.map{ x =>
      s"\ntps(I,X) :- rule(I), X = #count { ${x.getVars.toList.map(_.tostring).mkString(",")}: marked(I,${x.tostring}), " +
        s"example(${x.tostring}) }."
    }.mkString("\n")
  }

  def fpsCountRules(eps: List[Literal]) = {
    eps.map{ x =>
      s"\nfps(I,X) :- rule(I), X = #count { ${x.getVars.toList.map(_.tostring).mkString(",")}: marked(I,${x.tostring}), " +
        s"not example(${x.tostring}) }."
    }.mkString("\n")
  }

  def fnsCountRules(eps: List[Literal]) = {
    eps.map{ x =>
      s"\nfns(I,X) :- rule(I), X = #count { ${x.getVars.toList.map(_.tostring).mkString(",")}: example(${x.tostring}), " +
        s"not marked(I,${x.tostring}) }."
    }.mkString("\n")
  }


  /* This method is used to generate the ASP code that scores initiation and termination rules  */

  def generateScoringBK(modehs: List[ModeAtom]) = {

    if (modehs.isEmpty) { logger.error("No head mode declarations found.") ; System.exit(-1)}

    if (Globals.glvalues("with-ec").toBoolean) { // We're learning with the Event Calculus in the BK.
      // We can get the fluent from the head modes.
      val targetFluent = {
        // We can take the first one of the head modes (the target fluent is the same
        // regardless of whether the mode atom is an initiation of termination one).
        // Then, to get the target fluent, simply retrieve the first one of the 'terms' arg.

        val t = modehs.head.varbed.terms.head
        // The 'if' is for cases where the target pred is of the form initiatedAt(#fluent, +time), as in
        // initiatedAt(#fluent, +time) where fluent(leisure) is in the BK.
        // The 'else' is for compound fluents.
        if (t.isVariabe) Literal(functor = t._type) else modehs.head.varbed.terms.head.asInstanceOf[Literal]
        //modehs.head.varbed.terms.head.asInstanceOf[Literal]
      }

      val varNamesTostr = targetFluent.getVars.map(x => x.name).mkString(",")

      // The 'if' is for cases where the target pred is of the form initiatedAt(#fluent, +time)
      // the 'else' is for compound fluents.
      val vars = if (varNamesTostr == "") "X0,Te,Ts" else s"$varNamesTostr,Te,Ts"
      val fluent = if (varNamesTostr == "") "X0" else s"${targetFluent.tostring}"
      val typePreds = if (varNamesTostr == "") s"${targetFluent.tostring}(X0), next(Ts,Te), time(Te), time(Ts)" else "next(Ts,Te), time(Te), time(Ts)"

      /* Initiation scoring rules: */
      val initScoringRule1 = s"tps(I, X) :- rule(I), X = #count {$vars: example( holdsAt($fluent,Te) ), " +
        s"marked(I, initiatedAt($fluent,Ts) ), $typePreds }."

      val initScoringRule2 = s"fps(I, X) :- rule(I), X = #count {$vars: not example( holdsAt($fluent,Te) ), " +
        s"marked(I, initiatedAt($fluent,Ts) ), $typePreds }."

      val initScoringRule3 = s"fns(I, X) :- rule(I), X = #count {$vars: example( holdsAt($fluent,Te) ), " +
        s"not marked(I, initiatedAt($fluent,Ts) ), $typePreds }."

      /* Termination scoring rules: */
      val termScoringRule1 = s"tps(I, X) :- rule(I), X = #count {$vars: example( holdsAt($fluent,Te) ), " +
        s"not marked(I, terminatedAt($fluent,Ts) ), $typePreds }."

      val termScoringRule2 = s"tps(I, X) :- rule(I), X = #count {$vars: example( holdsAt($fluent,Ts) ), " +
        s"not example( holdsAt($fluent,Te) ), marked(I, terminatedAt($fluent,Ts) ), $typePreds }."

      val termScoringRule3 = s"fns(I, X) :- rule(I), X = #count {$vars: example( holdsAt($fluent,Te) ), " +
        s"marked(I, terminatedAt($fluent,Ts) ), $typePreds }."

      val termScoringRule4 = s"fps(I, X) :- rule(I), X = #count {$vars: example( holdsAt($fluent,Ts) ), " +
        s"not example( holdsAt($fluent,Te) ), not marked(I, terminatedAt($fluent,Ts) ), $typePreds }."

      val initRulesToStr = List(initScoringRule1, initScoringRule2, initScoringRule3).mkString("\n")
      val termRulesToStr = List(termScoringRule1, termScoringRule2, termScoringRule3, termScoringRule4).mkString("\n")

      (initRulesToStr, termRulesToStr)

    } else { // No Event Calculus
      val targetPred = modehs.head.varbed
      val varNamesTostr = targetPred.getVars.map(x => x.name).mkString(",")
      val tpsScoringRule = s"tps(I, X) :- rule(I), X = #count {$varNamesTostr: example(${targetPred.tostring}), marked(I, ${targetPred.tostring}) }."
      val fpsScoringRule = s"fps(I, X) :- rule(I), X = #count {$varNamesTostr: not example(${targetPred.tostring}), marked(I, ${targetPred.tostring}) }."
      val fnsScoringRule = s"fns(I, X) :- rule(I), X = #count {$varNamesTostr: example(${targetPred.tostring}), not marked(I, ${targetPred.tostring}) }."
      val scoringRules = List(tpsScoringRule, fpsScoringRule, fnsScoringRule).mkString("\n")
      (scoringRules, scoringRules)
    }
  }




}

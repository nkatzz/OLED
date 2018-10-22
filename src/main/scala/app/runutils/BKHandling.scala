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
        modehs.head.varbed.terms.head.asInstanceOf[Literal]
      }

      val varNamesTostr = targetFluent.getVars.map(x => x.name).mkString(",")

      /* Initiation scoring rules: */
      val initScoringRule1 = s"tps(I, X) :- rule(I), X = #count {$varNamesTostr,Te,Ts: example( holdsAt(${targetFluent.tostring},Te) ), " +
        s"marked(I, initiatedAt(${targetFluent.tostring},Ts) ), next(Ts,Te), time(Te), time(Ts) }."

      val initScoringRule2 = s"fps(I, X) :- rule(I), X = #count {$varNamesTostr,Te,Ts: not example( holdsAt(${targetFluent.tostring},Te) ), " +
        s"marked(I, initiatedAt(${targetFluent.tostring},Ts) ), next(Ts,Te), time(Te), time(Ts) }."

      val initScoringRule3 = s"fns(I, X) :- rule(I), X = #count {$varNamesTostr,Te,Ts: example( holdsAt(${targetFluent.tostring},Te) ), " +
        s"not marked(I, initiatedAt(${targetFluent.tostring},Ts) ), next(Ts,Te), time(Te), time(Ts) }."

      /* Termination scoring rules: */
      val termScoringRule1 = s"tps(I, X) :- rule(I), X = #count {$varNamesTostr,Te,Ts: example( holdsAt(${targetFluent.tostring},Te) ), " +
        s"not marked(I, terminatedAt(${targetFluent.tostring},Ts) ), next(Ts,Te), time(Te), time(Ts) }."

      val termScoringRule2 = s"tps(I, X) :- rule(I), X = #count {$varNamesTostr,Te,Ts: example( holdsAt(${targetFluent.tostring},Ts) ), " +
        s"not example( holdsAt(${targetFluent.tostring},Te) ), marked(I, terminatedAt(${targetFluent.tostring},Ts) ), next(Ts,Te), " +
        s"time(Te), time(Ts) }."

      val termScoringRule3 = s"fns(I, X) :- rule(I), X = #count {$varNamesTostr,Te,Ts: example( holdsAt(${targetFluent.tostring},Te) ), " +
        s"marked(I, terminatedAt(${targetFluent.tostring},Ts) ), next(Ts,Te), time(Te), time(Ts) }."

      val termScoringRule4 = s"fps(I, X) :- rule(I), X = #count {$varNamesTostr,Te,Ts: example( holdsAt(${targetFluent.tostring},Ts) ), " +
        s"not example( holdsAt(${targetFluent.tostring},Te) ), not marked(I, terminatedAt(${targetFluent.tostring},Ts) ), next(Ts,Te), " +
        s"time(Te), time(Ts) }."

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

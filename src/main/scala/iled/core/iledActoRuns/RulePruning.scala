package iled.core.iledActoRuns

import akka.actor.Actor
import com.typesafe.scalalogging.LazyLogging
import iled.core.Iled._
import iled.core.{Crossvalidation, Core}
import iled.structures.Examples.Example
import iled.structures._
import iled.utils.Database
import jep.Jep

/**
  * Created by nkatz on 12/20/15.
  */
object RulePruning extends LazyLogging {

  def pruneBadWeaks(seen: List[Example], theory: Theory, DB: Database,
                    totalTime: Double, pruningThreshold: Either[(String, Int), Int] = Right(2000),
                    testingSet: List[(Int, Int)] = Nil, jep: Jep) = {


    val pruneThreshold = pruningThreshold match {
      case Right(x) => x // hard-coded threshold
      case Left(x) =>
        val hle = x._1
        val percentage = x._2
        // compute the pruning threshold as a percentage of the total positives count in the training set
        val totalTps = seen.foldLeft(0) {
          (x, y) =>
            val positives = y.annotation.toSeq.count(p => p.contains(hle))
            x + positives
        }
        percentage / 100.0 * totalTps.toDouble
    }
    logger.info(s"\nPruning threshold: >= ${pruneThreshold.toInt}")
    val grouped = seen.grouped(20).map(x => Example.merge(x)).toList
    grouped.foldLeft(()) {
      // evaluate rules on each example
      (_, e) =>
        scoreRules(e, theory, jep)
        //theory.clauses.foreach(x => print(s"Scores from ${e.time}: ${x.tps} "))
    }
    val pruned = Theory(theory.clauses.filter(p => !p.fromWeakExample | (p.fromWeakExample && p.tps > pruneThreshold)))
    pruned.clauses foreach {
      x => logger.info(s"\n ${if (x.fromWeakExample) "weak" else "strong"} rule: \n ${x.tostring} \n tps: ${x.tps} \n fps: ${x.fps} \n fns: ${x.fns}")
    }
    wrapUp(DB, new PriorTheory(retainedRules = pruned), batchTimes = List[Double](), totalTime, testingSet = testingSet, jep = jep)
  }


  def scoreRules(e: Example, theory: Theory, jep: Jep): Unit = {
    def updateRuleStatsMap(what: String, t: Theory, map: Map[Int, List[Literal]]) = {
      for ((k, v) <- map) {
        updateRuleStats(what, t.clauses(k), v.length)
      }
    }
    def updateRuleStats(what: String, clause: Clause, newValue: Int) = {
      what match {
        case "tps" => clause.tps = clause.tps + newValue
        case "fps" => clause.fps = clause.fps + newValue
        case "fns" => clause.fns = clause.fns + newValue
      }
    }

    val program = markedProgram(e, theory)
    val file = iled.utils.Utils.getTempFile(prefix = "evalRules", suffix = "lp")
    iled.utils.Utils.writeLine(program, file.getCanonicalPath, "overwrite")
    val out = iled.utils.ASP.solveASP(jep = jep, task = "inference", aspFile = file.getCanonicalPath)
    if (out.nonEmpty) {
      val model = out.head.atoms
      val (tps, fps, fns) = {
        model.foldLeft(List[Literal](), List[Literal](), List[Literal]()) {
          (x, y) =>
            val (_tps, _fps, _fns) = (x._1, x._2, x._3)
            val lit = Literal.toLiteral(y)
            lit.functor match {
              case "tps" => (_tps :+ lit, _fps, _fns)
              case "fps" => (_tps, _fps :+ lit, _fns)
              case "fns" => (_tps, _fps, _fns :+ lit)
            }
        }
      }
      val groupAtoms = (x: List[Literal]) => x.groupBy(_.terms.head.asInstanceOf[Constant].name.toInt)
      val tpsPerClause = groupAtoms(tps)
      val fpsPerClause = groupAtoms(fps)
      val fnsPerClause = groupAtoms(fns)
      updateRuleStatsMap("tps", theory, tpsPerClause)
      updateRuleStatsMap("fps", theory, fpsPerClause)
      updateRuleStatsMap("fns", theory, fnsPerClause)
    }

  }

  def markedProgram(e: Example, theory: Theory) = {
    val include = s"\n#include " + "\"" + Core.bkMarkedFile_2 + "\".\n"
    val data = (e.annotationASP ++ e.narrativeASP).mkString("\n")
    val rules = markedRules(theory).map(_.tostring).mkString("\n")
    val markInitiation = "\ninitiatedAt(F,T) :- marked(I,initiatedAt(F,T)),rule(I).\n"
    val markTermination = "\nterminatedAt(F,T) :- marked(I,terminatedAt(F,T)),rule(I).\n"
    val ruleCounter = s"rule(0..${theory.clauses.length-1}).\n"
    val varbedExmplPatterns = for (x <- Core.examplePatterns) yield x.varbed.tostring
    val posCoveredConstr = varbedExmplPatterns.map(x => s"\ntps(I,$x):- marked(I,$x), example($x),rule(I).\n").mkString("\n")
    val negsCoveredConstr = varbedExmplPatterns.map(x => s"\nfps(I,$x):- marked(I,$x), not example($x),rule(I).\n").mkString("\n")
    val posNotCoveredConstr = varbedExmplPatterns.map(x => s"\nfns(I,$x):- not marked(I,$x), example($x),rule(I).\n").mkString("\n")
    val show = s"\n#show.\n#show tps/2.\n#show fps/2.\n#show fns/2.\n"
    val program = include + data + "\n" + rules + markInitiation + markTermination + ruleCounter + posCoveredConstr + negsCoveredConstr + posNotCoveredConstr + show
    program
  }

  def markedRules(theory: Theory) = {
    for ((c, i) <- theory.clauses zip List.range(0, theory.clauses.length);
         ctyped = c.withTypePreds();
         head = Literal(functor = "marked", terms = List(Constant(i.toString), ctyped.head));
         body = ctyped.body;
         clause = Clause(head = head, body = body)
    ) yield clause
  }


}

package oled.weightlearn

import app.runutils.{CMDArgs, RunningOptions}
import logic.Examples.Example
import logic.{Clause, Literal, Theory}
import oled.functions.WeightLearningFunctions.{buildMLN, getMRFAsStringSet, learnWeights}
import utils.Utils
import utils.parsers.ClausalLogicParser

import scala.io.Source

object Test {

  def main(args: Array[String]) = {

    import scala.sys.process._

    val cwd = System.getProperty("user.dir") // Current working dir
    val scriptsPath = cwd+"/scripts/weight-learn-eval"

    val bkFile = scriptsPath+"/bk.mln"
    val resultsFile = scriptsPath+"/results"
    val inferScript = scriptsPath+"/infer.sh"
    val compileScript = scriptsPath+"/compile.sh"
    val compiled = scriptsPath+"/compiled.mln"

    val mlnClauses = List("1 InitiatedAt(meet(x0,x1),x2) :- Happens(walking(x0),x2) ^ Close(x0,x1,25,x2)",
      "2 TerminatedAt(meet(x0,x1),x2) :- Happens(exit(x0),x2)",
      "3 TerminatedAt(meet(x0,x1),x2) :- Happens(exit(x1),x2)")

    val bk = Source.fromFile(bkFile).getLines.
      toList.takeWhile(line => line != "// LEARNT RULES FROM HERE!") ++
      List("// LEARNT RULES FROM HERE!") ++ mlnClauses

    Utils.writeLine(bk.mkString("\n"), bkFile, "overwrite")

    /* Compile the MLN */
    val command1 = Seq(compileScript, bkFile, compiled).mkString(" ")
    val res1 = command1.lineStream_!

    /* Run inference */
    val command2 = Seq(inferScript, compiled,
      "/home/nkatz/dev/CAVIAR_MLN/CAVIAR_MLN/meet/fold_7/testing/19-Meet_WalkTogether1.id3_id4.db",
      resultsFile).mkString(" ")
    val res2 = command2.lineStream_!

  }

  def getGroundNetwork_OLD_WAY(topTheory: Theory, e: Example, targetClass: String, step: Int, inps: RunningOptions) = {
    var newTopTheory = topTheory
    val allClauses = newTopTheory.clauses.flatMap { x =>
      if (x.body.nonEmpty) List(x) ++ x.refinements
      else x.refinements
    }
    val enumClauses = (1 to allClauses.length).toList
    val uuidsToRuleIdsMap = (allClauses.map(_.uuid) zip enumClauses) toMap
    val mlnClauses = (enumClauses zip allClauses toMap) map { case (k, v) => v.tostring_MLN(k) }

    val build_mln_timed = Utils.time{

      val targetTemplate =
        if(targetClass == "initiatedAt") "InitiatedAt(fluent,time,ruleId)"
        else "TerminatedAt(fluent,time,ruleId)"
      buildMLN(List("time", "person", "dist", "event", "event1", "fluent", "ruleId"), targetTemplate, mlnClauses, e, enumClauses, inps)
    }
    val mlnInfo = build_mln_timed._1
    val (mln, annotationDB, exmplCount) = (mlnInfo.mln, mlnInfo.annotationDB, mlnInfo.exmplCount)
    //println(s"Building MLN time: ${build_mln_timed._2} sec")

    val learn_weights_timed = Utils.time { learnWeights(mln, annotationDB, step, postLearning=true) }
    val mrf = learn_weights_timed._1
    //println(s"AdaGrad time: ${learn_weights_timed._2} sec")
    getMRFAsStringSet(mrf)
  }






}

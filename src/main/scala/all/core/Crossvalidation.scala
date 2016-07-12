package all.core

import all.globalValues.GlobalValues
import all.structures.Examples.Example
import jep.Jep

import scala.sys.process._
import all.parsers.ASPResultsParser
import all.utils.{ASP, Utils}
import all.structures.Exceptions._
import all.structures.Clause


class Crossvalidation(val examples: Map[String, List[String]], val theory: List[Clause], val withInertia: Boolean = true, val jep: Jep) extends ASPResultsParser {

  val FNS_PRED = "posNotCovered"
  val FPS_PRED = "negsCovered"
  val TPS_PRED = "posCovered"
  val aspInputFile = Utils.getTempFile(prefix = "crossVal", suffix = ".lp", deleteOnExit = true)
  //val bk = if (withInertia) List(s"\n#include " + "\"" + Core.bkFile + "\".\n") else List(s"\n#include " + "\"" + Core.bkFileNoInertia + "\".\n")

  val bk = if (withInertia) List(s"\n#include " + "\"" + GlobalValues.BK_CROSSVAL + "\".\n") else List(s"\n#include " + "\"" + Core.bkFileNoInertia + "\".\n")

  val command = Seq("python", Core.ASPHandler, s"aspfile=${aspInputFile.getCanonicalPath}")
  val varbedExmplPatterns = for (x <- Core.examplePatterns) yield x.varbed.tostring
  val coverageConstr =
    varbedExmplPatterns flatMap (x => List(s"\nposNotCovered($x) :- example($x), not $x.", s"\nnegsCovered($x):- $x, not example($x).", s"\nposCovered($x):- $x, example($x).\n"))

  Utils.toASPprogram(
      program = bk ++ examples("annotation") ++ examples("narrative") ++
        theory.map(x => x.tostring) ++ coverageConstr,
      show = List("posNotCovered/1","negsCovered/1","posCovered/1"), writeToFile = aspInputFile.getCanonicalPath
  )

  val res = ASP.solveASP(jep=jep,"crossvalidation",aspFile=aspInputFile.getCanonicalPath)
  val model = if (res.isEmpty) List[String]() else res.head.atoms

  def get(what: String) = {
    model match {
      case Nil => 0
      case _ =>
        model count (_.contains(what))
    }
  }

  val fns = get(FNS_PRED)
  val fps = get(FPS_PRED)
  val tps = get(TPS_PRED)
  val precision = tps.toFloat / (tps + fps)
  val recall = tps.toFloat / (tps + fns)
  val fscore = 2 * (precision * recall) / (precision + recall)
  val out = (tps, fps, fns, precision, recall, fscore)
}
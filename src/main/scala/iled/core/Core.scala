package iled.core


import com.typesafe.scalalogging._
import experiments.iledWeakStrongCleanData.KRExperiments
import iled.globalValues.GlobalValues
import iled.parsers.ModesParser
import jep.Jep

import scala.io.Source
import scala.util.matching._


object Core extends ModesParser with LazyLogging {


  var glvalues = GlobalValues.glvalues


  val cwd = GlobalValues.cwd
  val ASPHandler = s"$cwd/asp/ASPHandler.py"
  //val ASPHandler = s"$cwd/asp/ASPHandler-old.py"


  //val inputPath = s"$cwd/Caviar"
  //val fromDB = "CAVIAR-01-Walk1"

  //val inputPath = s"$cwd/datasets/CTM/DrivingStyle" //path to bk modes etc
  //val inputPath = s"$cwd/datasets/CTM/DrivingQuality"
  //val fromDB = "CTM"


  //val inputPath = s"$cwd/datasets/Caviar_clean_from_RTEC"
  //val fromDB = "CAVIAR_RTEC_CLEAN"

  //val inputPath = s"$cwd/datasets/Arindam"
  //val fromDB = "Arindam"

  val inputPath = GlobalValues.inputPath
  val fromDB = GlobalValues.fromDB



  //val aspSolverPath = inputPath // clingo must be in here

  val modesFile = GlobalValues.modesFile
  val bkFile = GlobalValues.BK_WHOLE_EC
  val bkFileNoInertia = inputPath + "/bk-no-inertia.lp"
  val bkMarkedFile = inputPath + "/bk-score-initiated.lp"
  val bkMarkedFileTerm = inputPath + "/bk-score-terminated.lp"
  val bkMarkedFile_2 = inputPath + "/bk-no-inertia-marked-2.lp"
  val bkInitiatedOnly = inputPath + "/bk-initiated-only.lp"
  val bkTerminatedOnly = inputPath + "/bk-terminated-only.lp"
  val theoryFile = inputPath + "/theory"

  val ABDUCTION = "abduction"
  val DEDUCTION = "deduction"
  val GET_QUERIES = "getQueries"
  val GET_GROUNDINGS = "getGroundings"
  val XHAIL = "xhail"
  val CHECKSAT = "checksat"
  val FIND_ALL_REFMS = "findAllRefs"
  val UNSAT = "UNSAT"
  val ILED = "iled"
  val INFERENCE = "inference"
  val SEARCH_MODELS = "search_models" //used to search alternative abductive explanations with iterative abductive search




  val matches = (p: Regex, str: String) => p.pattern.matcher(str).matches

  val modes =
    Source.fromFile(modesFile).getLines.toList.filter(line => !matches( """""".r, line))

  val modehs =
    modes.filter(m => m.contains("modeh") && !m.startsWith("%")).map(x => x).map(x => getParseResult(parseModes(modeh, x)))

  val modebs =
    modes.filter(m => m.contains("modeb") && !m.startsWith("%")).map(x => getParseResult(parseModes(modeb, x)))

  val x =
    modes.filter(m => m.contains("examplePattern") && !m.startsWith("%")).map(x => getParseResult(parseModes(exmplPattern, x)))

  val examplePatterns = x match {
    case List() => modehs // If no example patterns are found, use the head mode declarations for them
    case _ => x
  }

  val varbedExamplePatterns = this.examplePatterns map (p => p.varbed)

  val markRulesDirectives = {
    val varbedExmplPatterns = for (x <- examplePatterns) yield x.varbed.tostring
    val tps = varbedExmplPatterns.map(x => s"\ntps(I,$x):- marked(I,$x), example($x), rule(I).\n").mkString("\n")
    val fps = varbedExmplPatterns.map(x => s"\nfps(I,$x):- marked(I,$x), not example($x), rule(I).\n").mkString("\n")
    //val fns = varbedExmplPatterns.map(x => s"\nfns(I,$x):- not marked(I,$x), example($x), rule(I).\n").mkString("\n")
    List(tps,fps).mkString("\n")
  }

  def setGlobalParams(args: Array[String]): Unit = {
    val inargs: Map[String, String] = (
      for (arg <- args; split = arg.split("=")
      ) yield split(0) -> split(1)).toMap

    def setParams(param: String) = {
      if (inargs.keySet.contains(param)) Core.glvalues(param) = inargs(param)
    }
    Core.glvalues.keySet.foreach(x => setParams(x))
    /*
    if (inargs.keySet.contains("cwa")) Core.glvalues("cwa") = inargs("cwa")
    if (inargs.keySet.contains("iter-deepening")) Core.glvalues("iter-deepening") = inargs("iter-deepening")
    if (inargs.keySet.contains("mode")) Core.glvalues("mode") = inargs("mode")
    if (inargs.keySet.contains("perfect-fit")) Core.glvalues("perfect-fit") = inargs("perfect-fit")
    if (inargs.keySet.contains("iterations")) Core.glvalues("iterations") = inargs("iterations")
    if (inargs.keySet.contains("variableDepth")) Core.glvalues("variableDepth") = inargs("variableDepth")
    */
    logger.info(s"${iled.utils.Utils.lined("Running configuration:")} \n" + (for ((k, v) <- Core.glvalues) yield s"$k: $v").mkString("\n"))
  }


}



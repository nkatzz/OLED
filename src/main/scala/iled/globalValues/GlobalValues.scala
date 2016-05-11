package iled.globalValues

import iled.parsers.ModesParser

import scala.io.Source
import scala.reflect.io.File
import scala.util.matching.Regex

/**
  * Created by nkatz on 2/26/16.
  */

object GlobalValues {


  // This has not been used anywhere in the code yet. It should
  // (instead of Core). Each call to Core to get the statics
  // generates some stupid vals, which I don't know if it's
  // safe to turn to defs throughout the code and it's not the
  // time to refactor it. AT SOME POINT I'LL TO CLEAN THAT MESS.

  /*
 Default values (fall-back to these before trying to fix any bugs at any time):
    "cwa" -> "true",
    "iter-deepening"->"false",
    "mode" -> "incremental",
    "perfect-fit" -> "true",
    "iterations" -> "1000",
    "variableDepth" -> "1",
    "withWeaks" -> "false",
    "withBacktracking" -> "true",
    "refinementSearch" -> "fullSearch", // either setCover or fullSearch
    "specializeOnly" -> "true"
 */


  var glvalues =
    scala.collection.mutable.Map[String, String](
      "cwa" -> "true",
      "iter-deepening"->"false",
      "mode" -> "incremental",
      "perfect-fit" -> "true",
      "iterations" -> "1", //"1000",
      "variableDepth" -> "1",
      "withWeaks" -> "false",
      "withBacktracking" -> "true",
      "refinementSearch" -> "setCover", // either setCover or fullSearch
      // specializeOnly does not generate new kernel sets on new examples,
      // it only tries to refine an initial hypothesis based
      // on an initial kernel set, acquired from the first window
      "specializeOnly" -> "false",
      "compressKernels" -> "true",
      "ruleEvaluationFunction" -> "precision",//"mestimate"//"precision"
      // specializationDepth is used by OLED only. It specifies how "deep" in the
      // specialization lattice of a bottom clause we want to search. For instance
      // with specializationDepth=2, OLED generates candidate refinements of a clause
      // by using the 1-subsets and the 2-subsets of the corresponding bottom clause.
      // with specializationDepth=2 it uses 1-subsets, 2-subsets and 3-subsets and so on
      "specializationDepth" -> "1"
    )

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

  val cwd = System.getProperty("user.dir") // Current working dir

  val fromDB = "CAVIAR_Real_FixedBorders"
  //val fromDB = "CAVIAR_meeting_CLEAN"
  //val fromDB = "CAVIAR_moving_CLEAN"
  //val fromDB = "CAVIAR-MERGED-COPIES"

  //val inputPath = s"$cwd/datasets/CaviarMLN"  // Path to bk and modes files
  val inputPath = s"$cwd/datasets/Caviar"

  val modesFile = inputPath + "/modes"     // Mode Declarations file

  val ASPHandler = s"$cwd/asp/ASPHandler.py"

  val BK_INITIATED_ONLY = inputPath + "/bk-initiated-only.lp"
  val BK_TERMINATED_ONLY = inputPath + "/bk-terminated-only.lp"
  val BK_INITIATED_ONLY_MARKDED = inputPath + "/bk-score-initiated.lp"
  val BK_TERMINATED_ONLY_MARKDED = inputPath + "/bk-score-terminated.lp"
  val BK_WHOLE_EC = inputPath + "/bk.lp"
  val BK_CROSSVAL = inputPath + "/bk-for-crossval.lp"
  val BK_OLED_WITH_INERTIA = inputPath + "/bk-oled-with-inertia.lp"

  val HAND_CRAFTED_RULES = new java.io.File(inputPath).getParentFile.getCanonicalPath + "/hand-crafted-rules/caviar"
}

class GlobalValues extends ModesParser{

  /**
    * Global values and utilities used throughout the application
    *
    */

  val cwd = GlobalValues.cwd
  //val inputPath = s"$cwd/datasets/Caviar"  // Path to bk and modes files
  val inputPath = GlobalValues.inputPath
  val modesFile = GlobalValues.modesFile
  val fromDB = GlobalValues.fromDB

  val BK_INITIATED_ONLY = GlobalValues.BK_INITIATED_ONLY
  val BK_TERMINATED_ONLY = GlobalValues.BK_TERMINATED_ONLY
  val BK_OLED_WITH_INERTIA = inputPath + "/bk-oled-with-inertia.lp"
  val BK_INITIATED_ONLY_MARKDED = GlobalValues.BK_INITIATED_ONLY_MARKDED
  val BK_TERMINATED_ONLY_MARKDED = GlobalValues.BK_TERMINATED_ONLY_MARKDED
  val BK_WHOLE_EC = GlobalValues.BK_WHOLE_EC
  val BK_CROSSVAL = GlobalValues.BK_CROSSVAL

  private val matches = (p: Regex, str: String) => p.pattern.matcher(str).matches

  val MODES = Source.fromFile(modesFile).getLines.toList.filter(line => !matches( """""".r, line))
  val MODEHS = MODES.filter(m => m.contains("modeh") && !m.startsWith("%")).map(x => x).map(x => getParseResult(parseModes(modeh, x)))
  val MODEBS = MODES.filter(m => m.contains("modeb") && !m.startsWith("%")).map(x => getParseResult(parseModes(modeb, x)))

  // Example patterns as a list[ModeAtom] (helper)
  private val eps1 = MODES.filter(m => m.contains("examplePattern") && !m.startsWith("%")).map(x => getParseResult(parseModes(exmplPattern, x)))

  // Example patterns as a list[ModeAtom] (helper)
  private val eps2 = eps1 match {
    case List() => MODEHS // If no example patterns are found, use the head mode declarations for them
    case _ => eps1
  }



  private val coverageDirectives = {
    val varbedExmplPatterns = for (x <- eps2) yield x.varbed.tostring
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

  val EXAMPLE_PATTERNS = eps2 map (p => p.varbed)
  val EXAMPLE_PATTERNS_AS_STRINGS = EXAMPLE_PATTERNS map (_.tostring)

  val TPS_RULES = coverageDirectives._1
  val FPS_RULES = coverageDirectives._2
  val FNS_RULES = coverageDirectives._3
  val TPS_RULES_MARKED = coverageDirectives._4
  val FPS_RULES_MARKED = coverageDirectives._5
  val FNS_RULES_MARKED = coverageDirectives._6
  val CONSTRAINT_COVER_ALL_POSITIVES = coverageDirectives._7
  val CONSTRAINT_EXCLUDE_ALL_NEGATIVES = coverageDirectives._8

  val SHOW_TPS_ARITY_1 = "\n#show tps/1."
  val SHOW_TPS_ARITY_2 = "\n#show tps/2."
  val SHOW_FPS_ARITY_1 = "\n#show fps/1."
  val SHOW_FPS_ARITY_2 = "\n#show fps/2."
  val SHOW_FNS_ARITY_1 = "\n#show fns/1."
  val SHOW_FNS_ARITY_2 = "\n#show fns/2."
  val SHOW_TIME = "\n#show times/1."
  val SHOW_INTERPRETATIONS_COUNT = "\n#show examplesCount/1."
  val INCLUDE_BK = (file: String) => s"\n\n#include " + "\""+file+"\".\n"
  val HIDE = "\n#show.\n"
  val UNSAT = "UNSAT"
  val SAT = "SAT"

  val TPS_COUNT_RULE = {
    //tps(I,X) :- rule(I), X = #count { X0,X1,X2: marked(I,holdsAt(moving(X0,X1),X2)), example(holdsAt(moving(X0,X1),X2)) }.
    EXAMPLE_PATTERNS.map{ x =>
      s"\ntps(I,X) :- rule(I), X = #count { ${x.getVars.toList.map(_.tostring).mkString(",")}: marked(I,${x.tostring}), example(${x.tostring}) }."
    }.mkString("\n")
  }

  val FPS_COUNT_RULE = {
    //fps(I,X) :- rule(I), X = #count { X0,X1,X2: marked(I,holdsAt(moving(X0,X1),X2)), not example(holdsAt(moving(X0,X1),X2)) }.
    EXAMPLE_PATTERNS.map{ x =>
      s"\nfps(I,X) :- rule(I), X = #count { ${x.getVars.toList.map(_.tostring).mkString(",")}: marked(I,${x.tostring}), not example(${x.tostring}) }."
    }.mkString("\n")
  }

  val FNS_COUNT_RULE = {
    //fns(I,X) :- rule(I), X = #count { X0,X1,X2: example(holdsAt(moving(X0,X1),X2)), not marked(I,holdsAt(moving(X0,X1),X2)) }.
    EXAMPLE_PATTERNS.map{ x =>
      s"\nfns(I,X) :- rule(I), X = #count { ${x.getVars.toList.map(_.tostring).mkString(",")}: example(${x.tostring}), not marked(I,${x.tostring}) }."
    }.mkString("\n")
  }

  // FNs for terminated: not marked(I,exmpl), example(exmpl) (same as FNs for initiated)
  // TPs for terminated: marked(I, exmpl), example(exmpl) (same as TPs for initiated)

  val TIMES_COUNT_RULE = "\ntimes(X) :- X = #count { Z: time(Z) }.\n"






}

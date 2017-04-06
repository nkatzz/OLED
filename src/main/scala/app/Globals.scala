package app

import logic.{Clause, Literal}
import parsers.ModesParser
import utils.LookAheads.LookAheadSpecification
import scala.util.matching.Regex
import scala.io.Source
import utils.lookaheads._

/**
  * Created by nkatz on 9/13/16.
  */

object Globals {

  def apply(): Unit = {
    new Globals("","")
  }

  var totalPos = 0
  var totalNegs = 0

  // This may be set to a different value (e.g. passed from cmd) during the construction of the Globals instance
  var MAX_CLAUSE_LENGTH = 15

  var LEARNING_WHOLE_THEORIES = false // not really used anywhere

  val cwd = System.getProperty("user.dir") // Current working dir

  val ASPHandler = s"$cwd/asp/ASPHandler.py"

  /* Global names */

  val FIND_ALL_REFMS = "findAllRefs"
  val ABDUCTION = "abduction"
  val DEDUCTION = "deduction"
  val GET_QUERIES = "getQueries"
  val GET_GROUNDINGS = "getGroundings"
  val XHAIL = "xhail"
  val CHECKSAT = "checksat"
  val ILED = "iled"
  val INFERENCE = "inference"
  val SEARCH_MODELS = "search_models" //used to search alternative abductive explanations with iterative abductive search
  val SCORE_RULES = "score_rules"
  val GROW_NEW_RULE_TEST = "grow_new_rule_test"

  // These values may be set during the construction of the Globals instance
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
      "specializationDepth" -> "1",
      // if OLEDdownscoreBySimilarity is true then OLED penalizes candidate clauses
      // that are too similar to existing clauses in the current hypothesis,
      // to allow for exploring the quality of different clauses (that may be never
      // selected, because of a tie in score with other clauses)
      "OLEDdownscoreBySimilarity" -> "true",
      "distributed" -> "false",
      "with-jep" -> "true"
    )

  // if jep is used "UNSAT" else "UNSATISFIABLE"
  def UNSAT = if (glvalues("with-jep").toBoolean) "UNSAT" else "UNSATISFIABLE"

}


class Globals(val entryPath: String, val fromDB: String) { //extends ModesParser{

  /*
   * Global values and utilities used throughout the application
   */

  val cwd = System.getProperty("user.dir") // Current working dir
  val inputPath = entryPath // Path to bk and modes files
  val modesFile = s"$inputPath/modes" // Mode Declarations file



  //val AUXILIARY_PREDS = "auxiliaryPredicates"

  val BK_INITIATED_ONLY =  s"$inputPath/bk-initiated-only.lp"
  val BK_TERMINATED_ONLY = s"$inputPath/bk-terminated-only.lp"
  val BK_OLED_WITH_INERTIA = s"$inputPath/bk-oled-with-inertia.lp"
  val BK_INITIATED_ONLY_MARKDED = s"$inputPath/bk-score-initiated.lp"
  val BK_TERMINATED_ONLY_MARKDED = s"$inputPath/bk-score-terminated.lp"
  val BK_WHOLE_EC = s"$inputPath/bk.lp"
  val BK_CROSSVAL = s"$inputPath/bk-for-crossval.lp"

  val ILED_NO_INERTIA = inputPath + "/bk-no-inertia.lp"

  private val matches = (p: Regex, str: String) => p.pattern.matcher(str).matches

  val modesParser = new ModesParser

  val MODES = Source.fromFile(modesFile).getLines.toList.filter(line => !matches( """""".r, line))
  val MODEHS = MODES.filter(m => m.contains("modeh") && !m.startsWith("%")).map(x => x).map(x => modesParser.getParseResult(modesParser.parseModes(modesParser.modeh, x)))
  val MODEBS = MODES.filter(m => m.contains("modeb") && !m.startsWith("%")).map(x => modesParser.getParseResult(modesParser.parseModes(modesParser.modeb, x)))

  // Example patterns as a list[ModeAtom] (helper)
  private val eps1 =
    MODES.filter(m => m.contains("examplePattern") && !m.startsWith("%")).map(x => modesParser.getParseResult(modesParser.parseModes(modesParser.exmplPattern, x)))

  // Example patterns as a list[ModeAtom] (helper)
  private val eps2 = eps1 match {
    case List() => MODEHS // if no example patterns are found, use the head mode declarations for them
    case _ => eps1
  }

  def getAdditionalLanguageBias(predicateName: String) = {
    val f = Source.fromFile(modesFile).getLines.toList.filter(line => line.startsWith(s"$predicateName"))
    f.map(x => x.split(s"$predicateName\\(")(1).split("\\)")(0)).filter(p => (MODEHS++MODEBS).exists(q => q.functor == p))
  }


  val FORCE_PREDS = getAdditionalLanguageBias("force")
  val BASIC_PREDS = getAdditionalLanguageBias("basic")
  val AUXILIARY_PREDS = getAdditionalLanguageBias("auxiliary")

  /*
  val LOOK_AHEADS = {
    val f = Source.fromFile(modesFile).getLines.toList.filter(line => line.startsWith("lookahead"))
    if (f.nonEmpty) f.map( x => new LookAheadSpecification(x) ) else Nil
  }
  */

  val LOOK_AHEADS_TEST = {
    val f = Source.fromFile(modesFile).getLines.toList.filter(line => line.startsWith("lookahead"))
    if (f.nonEmpty) f.map( x => new LookAheadUtils.LookAheadSpecification(x) ) else Nil
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
  val SHOW_INTERPRETATIONS_COUNT = "\n#show exampleGrounding/1." //"\n#show examplesCount/1."
  val INCLUDE_BK = (file: String) => s"\n\n#include " + "\""+file+"\".\n"
  val HIDE = "\n#show.\n"

  // if jep is used "UNSAT" else "UNSATISFIABLE"
  def UNSAT = if (Globals.glvalues("with-jep").toBoolean) "UNSAT" else "UNSATISFIABLE"

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

  def EXAMPLE_COUNT_RULE = this.EXAMPLE_PATTERNS.map(x => s"exampleGrounding(${x.tostring}):-${x.getTypePredicates(this).mkString(",")}.").mkString("\n")+"\n"

  val HAND_CRAFTED_RULES = new java.io.File(inputPath).getParentFile.getCanonicalPath + "/hand-crafted-rules/caviar"


  var EVALUATION_FUNCTION = "precision_recall" // alternative is foil_gain
  var MAX_CLAUSE_LENGTH = 15
  var LEARNING_WHOLE_THEORIES = false
  var TOP_THEORY_SCORE = 0.0
  var TOP_THEORY_SCORE_COUNT = 0

  /*
   * The following are not used anywhere, they are for debugging
   */
  /*
  private val initHead = "initiatedAt(meeting(X0,X1),X2)"
  private val initHead1 = "initiatedAt(meeting(X1,X0),X2)"
  private val termHead = "terminatedAt(meeting(X0,X1),X2)"
  private val termHead1 = "terminatedAt(meeting(X1,X0),X2)"
  private val BCBodyLits =
    List("happensAt(inactive(X1),X2)","happensAt(inactive(X0),X2)",
      "happensAt(active(X1),X2)","happensAt(active(X0),X2)",
      "happensAt(walking(X1),X2)","happensAt(walking(X0),X2)",
      "happensAt(running(X1),X2)","happensAt(running(X0),X2)",
      "happensAt(appear(X1),X2)","happensAt(appear(X0),X2)",
      "happensAt(disappear(X1),X2)","happensAt(disappear(X0),X2)",
      "not happensAt(disappear(X1),X2)","not happensAt(disappear(X0),X2)",
      "close(X0,X1,24,X2)","close(X1,X0,24,X2)","close(X0,X1,25,X2)","close(X1,X0,25,X2)",
      "close(X0,X1,30,X2)","close(X1,X0,30,X2)","close(X0,X1,34,X2)","close(X1,X0,34,X2)",
      "far(X0,X1,24,X2)","far(X1,X0,24,X2)","far(X0,X1,25,X2)","far(X1,X0,25,X2)",
      "far(X0,X1,30,X2)","far(X1,X0,30,X2)","far(X0,X1,34,X2)","far(X1,X0,34,X2)")

  val initBC1 = {
    val h = Literal.toLiteral(initHead)
    val b = BCBodyLits map (x => Literal.toLiteral(x))
    Clause(List(h) ++ b)
  }

  val initBC2 = {
    val h = Literal.toLiteral(initHead1)
    val b = BCBodyLits map (x => Literal.toLiteral(x))
    Clause(List(h) ++ b)
  }

  val termBC1 = {
    val h = Literal.toLiteral(termHead)
    val b = BCBodyLits map (x => Literal.toLiteral(x))
    Clause(List(h) ++ b)
  }

  val termBC2 = {
    val h = Literal.toLiteral(termHead1)
    val b = BCBodyLits map (x => Literal.toLiteral(x))
    Clause(List(h) ++ b)
  }
  */
}


object CMDArgsNames {
  val INPUT_PATH = "inpath"
  val EVAL_EXISTING = "evalth"
  val EVAL_ON_WHOLE_TRAINING_SET = "evalonhole"
  val TRAIN_SIZE = "trainsize"
  val MULTCOUNTS = "multcounts"
  val SPECIALIZATION_DEPTH = "spdepth"
  val REPEAT_FOR = "repfor"
  val MIN_SEEN = "minseen"
  val PRUNING_THRESHOLD = "prune"
  val TIE_BREAKING_THRESHOLD = "ties"
  val DELTA = "delta"
  val DB = "db"
  val WITH_JEP = "wjep"
  val CHUNK_SIZE = "chunksize"
  val ONLINE_PRUNING = "onlineprune"
  val POST_PRUNING = "postprune"
  val HLE = "hle"

}

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

import java.io.PrintWriter

import logic.Modes.ModeAtom
import logic.{AtomSignature, Clause, Literal, Theory, Variable}
import utils.lookaheads._
import utils.parsers.ModesParser
import BKHandling._
import com.typesafe.scalalogging.LazyLogging
import woled.State

import scala.io.Source
import scala.util.matching.Regex

/**
  * Created by nkatz on 9/13/16.
  */

object Globals {

  def apply(): Unit = {
    new Globals("")
  }

  //var hedgePredictionThreshold = 0.0 // Quick & dirty, for experiments

  var sleepingExpertsLearningRate = 0.0 // Quick & dirty, for experiments
  var sleepingExpertsFeedBackBias = 0.0
  var hedgeInertia = false

  var timeDebug = List[Double]()

  var scoringFunction = "default" // precision for initiation, recall for termination

  var totalPos = 0
  var totalNegs = 0

  // This may be set to a different value (e.g. passed from cmd) during the construction of the Globals instance
  var MAX_CLAUSE_LENGTH = 15

  var LEARNING_WHOLE_THEORIES = false // not really used anywhere

  val cwd: String = System.getProperty("user.dir") // Current working dir

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
      "iter-deepening" -> "false",
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
      "ruleEvaluationFunction" -> "precision", //"mestimate"//"precision"
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
      "with-jep" -> "false",
      "domain" -> "any",
      // Use this to get non-empty revisions at any point. This is necessary
      // because there are cases where the model corresponding to an empty
      // theory may have lower cost (in the optimization) than a model that
      // corresponds to a theory (e.g. when including any initiation rule in the theory yields
      // many fps). In such cases the solver will opt for an empty theory, which is not
      // always desirable. This parameter is used by the MCTS version of OLED.
      "smallest-nonempty" -> "false",
      // Weights on examples
      "tp-weight" -> "1",
      "fp-weight" -> "1",
      "fn-weight" -> "1",
      "with-inertia" -> "false",
      "weight-learning" -> "false",
      "with-ec" -> "true"
    )

  // if jep is used "UNSAT" else "UNSATISFIABLE"
  def UNSAT = if (glvalues("with-jep").toBoolean) "UNSAT" else "UNSATISFIABLE"

  // This is a storage of the current initiation/termination
  // parts of the theory. These fields are used by the monolithic version
  // of OLED only, when learning with inertia (from edge interval points)
  // to get the joint theory and see if it satisfies each new example. Abduction
  // and new clauses are generated if not.
  //--------------------------------------------------------------------------------------
  // UPDATE: Testing for new clause generation using the satisfiability of the
  // current joint theory works, for srongly-initiated fluents with no or very
  // small amount of noise, but it takes a lot of time in large learning tasks.
  // The reason is that the joint theory is unsatisfiable in most cases, since it
  // contains over-general rules that erroneously re-initiate or terminate a target
  // fluent. This means that abduction and new kernel set generation takes place
  // almost always, in every new mini-batch, which causes great delays in the execution.
  // For this to work we'd need a more ILED-style apporach, where clauses are not scored,
  // but corrected at every new mistake. In the absence on noise this makes the joint
  // theory to quickly converge to the correct one. On the other hand, if there is a
  // substantial amount of noise in the data, therefore the edge interval points are
  // frequently corrupted, there is no hope to learn strongly-initiated fluents, so there
  // is no point discussing it or trying to fix it with simple modifications in the BK.
  //--------------------------------------------------------------------------------------
  var CURRENT_THEORY_INITIATED: Vector[Clause] = Vector[Clause]()
  var CURRENT_THEORY_TERMINATED: Vector[Clause] = Vector[Clause]()

  def getCurrentJointTheory() = {
    Theory((CURRENT_THEORY_INITIATED ++ CURRENT_THEORY_TERMINATED).toList)
  }

  //var errorProb = Vector.empty[Int]

}





class Globals(val entryPath: String) extends LazyLogging {

  /*
   * Global values and utils.
   */

  val state = new State

  val cwd: String = System.getProperty("user.dir") // Current working dir
  val inputPath: String = entryPath // Path to bk and modes files
  val modesFile: String = s"$inputPath/modes" // Mode Declarations file

  //val AUXILIARY_PREDS = "auxiliaryPredicates"

  val BK_INITIATED_ONLY =  s"$inputPath/bk-initiated-only.lp"
  val BK_TERMINATED_ONLY = s"$inputPath/bk-terminated-only.lp"
  val ABDUCE_WITH_INERTIA = s"$inputPath/abduce-with-inertia.lp"
  val INITIATED_ONLY_INERTIA = s"$inputPath/initiated-only-with-inertia.lp"
  val BK_INITIATED_ONLY_MARKDED = s"$inputPath/bk-score-initiated.lp" // BK for scoring initiation rules
  val BK_TERMINATED_ONLY_MARKDED = s"$inputPath/bk-score-terminated.lp" // BK for scoring termination rules
  val BK_RULE_SCORING_MARKDED = s"$inputPath/bk-score.lp" // BK for rule scoring when learning without the EC.

  val USER_BK = s"$inputPath/bk"

  val BK_WHOLE_EC = s"$inputPath/bk.lp"
  val BK_WHOLE = s"$inputPath/bk.lp" // for learning without the EC, no practical difference
  val BK_CROSSVAL = s"$inputPath/bk-for-crossval.lp"

  val ILED_NO_INERTIA: String = inputPath + "/bk-no-inertia.lp"

  def matches(p: Regex, str: String) = p.pattern.matcher(str).matches

  val modesParser = new ModesParser

  val MODES: List[String] = Source.fromFile(modesFile).getLines.toList.filter(line => !matches( """""".r, line) && !line.startsWith("%"))


  val MODEHS: List[ModeAtom] = MODES.filter(m => m.contains("modeh") && !m.startsWith("%")).map(x => x).
    map(x => modesParser.getParseResult(modesParser.parseModes(modesParser.modeh, x)))

  if (MODEHS.isEmpty) logger.error("No head mode declarations found.")

  val MODEBS: List[ModeAtom] = MODES.filter(m => m.contains("modeb") && !m.startsWith("%")).
    map(x => modesParser.getParseResult(modesParser.parseModes(modesParser.modeb, x)))

  if (MODEBS.isEmpty) logger.error("No body mode declarations found.")


  /* The input to this method is a Literal representation of mode atoms and example pattern atoms (variabilized). */

  def getTypeAxioms(m: Literal): Set[String] = {
    val plmrkTerms = m.placeMarkers
    val (posPlmrkTerms, negPlrmTerms, grndPlmrkTerms) = (plmrkTerms._1, plmrkTerms._2, plmrkTerms._3)
    val allPlmrks = (posPlmrkTerms ++ negPlrmTerms ++ grndPlmrkTerms).map(x => x.asInstanceOf[Variable]).toSet

    allPlmrks.foldLeft(Set[String]()) { (accum, y) =>
      val allOtherPlmrks = allPlmrks diff Set(y)
      if (y.inOrOutVar == "+" || y.inOrOutVar == "-") {
        val result_ = s"${y._type}(${{y.name}}) :- ${m.tostring}."

        // the regex below matches variable symbols which do not appear in predicate of function
        // names. So it will match X0 in p(X0) but not in pX0(X0), pxX0(X0), pX_0(X0), p_2X0(X0) and so on
        val result = allOtherPlmrks.foldLeft(result_) { (x1, y1) => x1.replaceAll(s"(?<![a-zA-Z0-9_]+)${y1.name}", "_") }
        accum + result
      } else {
        accum
      }
    }
  }

  // Example patterns as a list[ModeAtom] (helper)
  val eps1: List[ModeAtom] =
    MODES.filter(m => m.contains("examplePattern") && !m.startsWith("%")).
      map(x => modesParser.getParseResult(modesParser.parseModes(modesParser.exmplPattern, x)))

  val eps2: List[ModeAtom] = eps1 match {
    case List() => MODEHS // if no example patterns are found, use the head mode declarations for them
    case _ => eps1
  }

  // Auxiliary predicates. These are input predicates which are not part of the target language
  // but are necessary for extracting the types of entities in the domain (e.g. think of coords/4 in CAVIAR).
  private val inputPreds: List[ModeAtom] = {
    MODES.filter(m => m.contains("inputPredicate") && !m.startsWith("%")).
      map(x => modesParser.getParseResult(modesParser.parseModes(modesParser.inputPred, x)))
  }


  if (inputPreds.exists(p => p.isNAF)) {
    logger.error(s"NAF is not allowed in input predicates.")
    System.exit(-1)
  }


  // This method generates types axioms for the mode declarations,
  // i.e. rules of the form: time(X1) :- happensAt(active(_),X1).
  private val typeAxioms = {
    val m = inputPreds.filter(x => !x.isNAF).map(x => x.varbed)
    val x = m.flatMap(getTypeAxioms).toSet
    //x foreach println
    x
  }

  /*
  * Comparison predicates compare numerical values to a threshold, e.g:
  *
  * close(p1, p2, 30, 10)
  *
  * meaning that the Euclidean distance of p1, p2 at time 10 is less than 30.
  *
  * Comparison predicates may be declared in the modes file like this:
  *
  * comparisonPredicate(close(+person,+person,#numvalue,+time), lessThan, comparison_term_position(3))
  *
  * The #numvalue placemarker indicates the position of the actual numerical threshold
  * while the 'lessThan' term (can also be 'greaterThan') declares the intended "semantics"
  * of the predicate. Note that numvalue has to be the type of this term in the corresponding body declaration. The
  * comparison_term_position(3) indicates the position of the comparison term in the atom. In folded atoms the whole
  * "path" to this term needs to be specified e.g.
  *
  * comparisonPredicate(far(+person,+person,test(+person, p(#threshold_value)),+time), greaterThan, comparison_term_position(3,2,1))
  *
  * Here to find the comparison term take atom.terms(3).terms(2).terms(1). See also the method getComparisonTerm
  * in the Modes class and the getComparisonTerm in the Literal class.
  *
  * Comparison predicate declarations are used internally to allow for two tasks that simplify the learning process:
  *
  * 1. Reduce clauses: When a comparison predicate in the lessThan semantics and with numvalue1 is added to a rule,
  *    then any other similar predicate with numvalue2 such that numvalue2 > numvalue1 is removed from the rule.
  *    Rules with comparison predicate in the greaterThan semantics are reduced accordingly.
  * 2. When generating candidate specializations, rules that consist of comparison predicates only (e.g. close/4
  *    predicates only) are omitted.
  * */
  val comparisonPredicates: List[ModeAtom] = {
    MODES.filter(m => m.contains("comparisonPredicate") && !m.startsWith("%")).
      map(x => modesParser.getParseResult(modesParser.parseModes(modesParser.compPred, x)))
  }

  MODEBS foreach { m =>
    val x = comparisonPredicates.find(z => z == m).getOrElse(ModeAtom())
    if (x != ModeAtom()) {
      m.compRelation = x.compRelation
      m.comparisonTermPosition = x.comparisonTermPosition
    }
  }

  val headAtomSignatures: List[AtomSignature] = {
    MODEHS.map(x => new AtomSignature(x.functor, x.arity))
  }

  val bodyAtomSignatures: List[AtomSignature] = {
    MODEBS.map(x => new AtomSignature(x.functor, x.arity))
  }

  /* Reads the background knowledge from  $inputPath/bk.lp and produces helper files (e.g. for rule evaluation,
     bottom clause generation etc.) */

  def generateBKFiles_Event_Calculus() = {

    //private val PY_LESSTHAN =
    //  "#script (python)\nfrom gringo import Fun\nimport math\n\ndef less_than(x,y):\n    return float(x) < float(y)\n\n#end."

    val EC_AXIOM_1 = "holdsAt(F,Te) :- fluent(F), not sdFluent(F), initiatedAt(F,Ts), next(Ts, Te)."
    val EC_AXIOM_2 = "holdsAt(F,Te) :- fluent(F), not sdFluent(F), holdsAt(F,Ts), " +
      "not terminatedAt(F,Ts), next(Ts, Te)."
    //private val RIGHT_BEFORE_DEF = "right_before(X,Z) :- time(X), time(Z), Z = X+40."
    ///*
    /*val RIGHT_BEFORE_DEF ="\n#script (python)\ntimes = []\ndef collect_all(a):\n    times.append(a)\n    " +
      "return 1\ndef sorted():\n    times.sort()\n    return zip(range(len(times)), times)\n#end.\ncollect_all." +
      "\ncollect_all :- time(X), @collect_all(X) == 0.\nsorted_pair(X,N) :- collect_all, " +
      "(X,N) = @sorted().\nnext(X, Y) :- sorted_pair(A,X), sorted_pair(A+1,Y).\n"*/

    val RIGHT_BEFORE_DEF =
      """
        |#script (python)
        |times = []
        |def collect_all(a):
        |    times.append(a)
        |    return 1
        |def sorted():
        |    times.sort()
        |    return zip(range(len(times)), times)
        |def end_time():
        |    times.sort()
        |    return times[-1]
        |def start_time():
        |    times.sort()
        |    return times[0]
        |#end.
        |collect_all.
        |collect_all :- time(X), @collect_all(X) == 0.
        |sorted_pair(X,N) :- collect_all, (X,N) = @sorted().
        |next(X, Y) :- sorted_pair(A,X), sorted_pair(A+1,Y).
        |start_end :- collect_all.
        |start_end(X,Y) :- start_end, X = @start_time(), Y = @end_time().
        |%endTime(X) :- X = @end_time().
        |startTime(X) :- X = @start_time().
        |""".stripMargin

    //*/
    val INIT_TIME_DEF = "initialTime(X) :- time(X), #false : X > Y, time(Y)."
    val INIT_HOLDS_DEF = "%THIS SHOULD NOT BE HERE!\nholdsAt(F,T) :- initialTime(T), example(holdsAt(F,T))."
    val CORE_EVENT_CALCULUS_BK = List(EC_AXIOM_1, EC_AXIOM_2, RIGHT_BEFORE_DEF, INIT_TIME_DEF, INIT_HOLDS_DEF)
    val CROSSVAL_EVENT_CALCULUS_BK = List(EC_AXIOM_1, EC_AXIOM_2, RIGHT_BEFORE_DEF)
    val INITIATED_ONLY_EVENT_CALCULUS_BK = List(EC_AXIOM_1, RIGHT_BEFORE_DEF, INIT_TIME_DEF, INIT_HOLDS_DEF)
    val TERMINATED_ONLY_EVENT_CALCULUS_BK =
      List(EC_AXIOM_1, EC_AXIOM_2, RIGHT_BEFORE_DEF, INIT_TIME_DEF, INIT_HOLDS_DEF,
        "holdsAt(F,T) :- fluent(F), not sdFluent(F), examplesInitialTime(T), example(holdsAt(F,T)).",
        "examplesInitialTime(X) :- example(holdsAt(_,X)), #false : X > Y, example(holdsAt(_,Y)).")

    // Read the user-input BK
    val userBK = Source.fromFile(USER_BK).getLines.toList.mkString("\n")

    // Generate the ASP scoring rules:
    val scoringRules = generateScoringBK(MODEHS)

    // Type axioms:
    val tas = this.typeAxioms.mkString("\n")

    // Generate bk.lp file (it will be used for reasoning)
    val bkFile = new java.io.File(BK_WHOLE_EC)
    val pw1 = new PrintWriter(bkFile)
    pw1.write(userBK+"\n")
    pw1.write(CORE_EVENT_CALCULUS_BK.mkString("\n"))
    pw1.write("\n"+tas)
    pw1.close()
    bkFile.deleteOnExit()

    // Generate initiation-only BK file
    val initOnlyBKFile = new java.io.File(BK_INITIATED_ONLY)
    val pw2 = new PrintWriter(initOnlyBKFile)
    pw2.write(userBK+"\n")
    pw2.write(INITIATED_ONLY_EVENT_CALCULUS_BK.mkString("\n"))
    pw2.write("\n"+tas)
    pw2.close()
    initOnlyBKFile.deleteOnExit()

    // Generate termination-only BK file
    val termOnlyBKFile = new java.io.File(BK_TERMINATED_ONLY)
    val pw3 = new PrintWriter(termOnlyBKFile)
    pw3.write(userBK+"\n")
    pw3.write(TERMINATED_ONLY_EVENT_CALCULUS_BK.mkString("\n"))
    pw3.write("\n"+tas)
    pw3.close()
    termOnlyBKFile.deleteOnExit()

    // Generate initiation-scoring rules
    val scoreInitFile = new java.io.File(BK_INITIATED_ONLY_MARKDED)
    val pw4 = new PrintWriter(scoreInitFile)
    pw4.write(userBK+"\n")
    pw4.write("\n"+scoringRules._1+"\n"+RIGHT_BEFORE_DEF+"\n")
    pw4.write("\n"+tas)
    pw4.close()
    scoreInitFile.deleteOnExit()

    // Generate termination-scoring rules
    val scoreTermFile = new java.io.File(BK_TERMINATED_ONLY_MARKDED)
    val pw5 = new PrintWriter(scoreTermFile)
    pw5.write(userBK+"\n")
    pw5.write("\n"+scoringRules._2+"\n"+RIGHT_BEFORE_DEF+"\n")
    pw5.write("\n"+tas)
    pw5.close()
    scoreTermFile.deleteOnExit()

    // Generate cross-validation file
    val crossValFile = new java.io.File(BK_CROSSVAL)
    val pw6 = new PrintWriter(crossValFile)
    pw6.write(userBK+"\n")
    pw6.write(CROSSVAL_EVENT_CALCULUS_BK.mkString("\n"))
    pw6.write("\n"+tas)
    pw6.close()
    crossValFile.deleteOnExit()

  }


  def generateBKFiles_No_Event_Calculus() = {
    // Read the user-input BK
    val userBK = Source.fromFile(USER_BK).getLines.toList.mkString("\n")

    // Generate the ASP scoring rules:
    val scoringRules = generateScoringBK(MODEHS)

    // Type axioms:
    val tas = this.typeAxioms.mkString("\n")

    // Generate bk.lp file (it will be used for reasoning)
    val bkFile = new java.io.File(BK_WHOLE)
    val pw1 = new PrintWriter(bkFile)
    pw1.write(userBK+"\n")
    pw1.write("\n"+tas)
    pw1.close()
    bkFile.deleteOnExit()

    // Generate BK file for rule scoring
    val scoreTermFile = new java.io.File(BK_RULE_SCORING_MARKDED)
    val pw5 = new PrintWriter(scoreTermFile)
    pw5.write(userBK+"\n")
    pw5.write("\n"+scoringRules._2+"\n")
    pw5.write("\n"+tas)
    pw5.close()
    scoreTermFile.deleteOnExit()

    // Generate cross-validation file
    val crossValFile = new java.io.File(BK_CROSSVAL)
    val pw6 = new PrintWriter(crossValFile)
    pw6.write(userBK+"\n")
    pw6.write("\n"+tas)
    pw6.close()
    crossValFile.deleteOnExit()

  }


  if(Globals.glvalues("with-ec").toBoolean) {
    generateBKFiles_Event_Calculus()
  } else {
    generateBKFiles_No_Event_Calculus()
  }

  val EXAMPLE_PATTERNS: List[Literal] = eps2 map (p => p.varbed)
  val EXAMPLE_PATTERNS_AS_STRINGS: List[String] = EXAMPLE_PATTERNS map (_.tostring)

  private val coverageDirectives = getCoverageDirectives(EXAMPLE_PATTERNS_AS_STRINGS)

  val TPS_RULES: String = coverageDirectives._1
  val FPS_RULES: String = coverageDirectives._2
  val FNS_RULES: String = coverageDirectives._3
  val TPS_RULES_MARKED: String = coverageDirectives._4
  val FPS_RULES_MARKED: String = coverageDirectives._5
  val FNS_RULES_MARKED: String = coverageDirectives._6
  val CONSTRAINT_COVER_ALL_POSITIVES: String = coverageDirectives._7
  val CONSTRAINT_EXCLUDE_ALL_NEGATIVES: String = coverageDirectives._8

  val SHOW_TPS_ARITY_1 = "\n#show tps/1."
  val SHOW_TPS_ARITY_2 = "\n#show tps/2."
  val SHOW_FPS_ARITY_1 = "\n#show fps/1."
  val SHOW_FPS_ARITY_2 = "\n#show fps/2."
  val SHOW_FNS_ARITY_1 = "\n#show fns/1."
  val SHOW_FNS_ARITY_2 = "\n#show fns/2."
  val SHOW_TIME = "\n#show times/1."
  val SHOW_INTERPRETATIONS_COUNT = "\n#show countGroundings/1."
  val INCLUDE_BK: String => String = (file: String) => s"\n\n#include " + "\""+file+"\".\n"
  val HIDE = "\n#show.\n"

  // if jep is used "UNSAT" else "UNSATISFIABLE"
  def UNSAT = if (Globals.glvalues("with-jep").toBoolean) "UNSAT" else "UNSATISFIABLE"

  val SAT = "SAT"

  val TPS_COUNT_RULE: String = tpsCountRules(EXAMPLE_PATTERNS)

  val FPS_COUNT_RULE: String = fpsCountRules(EXAMPLE_PATTERNS)

  val FNS_COUNT_RULE: String = fnsCountRules(EXAMPLE_PATTERNS)

  // FNs for terminated: not marked(I,exmpl), example(exmpl) (same as FNs for initiated)
  // TPs for terminated: marked(I, exmpl), example(exmpl) (same as TPs for initiated)

  val TIMES_COUNT_RULE = "\ntimes(X) :- X = #count { Z: time(Z) }.\n"

  /*
  def EXAMPLE_COUNT_RULE =
    this.EXAMPLE_PATTERNS.map{ x =>
      s"exampleGrounding(${x.tostring}):-${x.getTypePredicates(this).mkString(",")}.\n"+
      s"countGroundings(X) :- X = #count { ${x.getVars.toList.map(_.tostring).mkString(",")}: " +
        s"exampleGrounding(${x.tostring}),${x.getTypePredicates(this).mkString(",")} }."
    }.mkString("\n")+"\n"
  */

  /* I NEED TO FIND A WAY TO MAKE THIS GENERIC (NON- EVENT CALCULUS SPECIFIC).
   * FOR EXAMPLE, THE USER COULD SPECIFY IT IN THE MODES FILE. */

  /*
  def EXAMPLE_COUNT_RULE = "exampleGrounding(holdsAt(F,T)):-fluent(F),time(T).\n"+
    "countGroundings(X) :- X = #count { F,T: exampleGrounding(holdsAt(F,T)),fluent(F),time(T) }.\n"
  */
  def EXAMPLE_COUNT_RULE = {
    val targetPred = EXAMPLE_PATTERNS.head
    val tpstr = targetPred.tostring
    val vars = targetPred.getVars.map(x => x.name).mkString(",")
    val typePreds = targetPred.getTypePredicates(this).mkString(",")
    s"exampleGrounding($tpstr) :- $typePreds.\ncountGroundings(X) :- X = #count { $vars: exampleGrounding($tpstr),$typePreds }.\n"

  }


  /*
   val LOOK_AHEADS = {
     val f = Source.fromFile(modesFile).getLines.toList.filter(line => line.startsWith("lookahead"))
     if (f.nonEmpty) f.map( x => new LookAheadSpecification(x) ) else Nil
  }
  */

  private val LOOK_AHEADS_TEST = {
    val f = Source.fromFile(modesFile).getLines.toList.filter(line => line.startsWith("lookahead"))
    if (f.nonEmpty) f.map( x => new LookAheadUtils.LookAheadSpecification(x) ) else Nil
  }


  /*
    def getAdditionalLanguageBias(predicateName: String) = {
      val f = Source.fromFile(modesFile).getLines.toList.filter(line => line.startsWith(s"$predicateName"))
     f.map(x => x.split(s"$predicateName\\(")(1).split("\\)")(0)).filter(p => (MODEHS++MODEBS).exists(q => q.functor == p))
    }

    val FORCE_PREDS = getAdditionalLanguageBias("force")
    val BASIC_PREDS = getAdditionalLanguageBias("basic")
    val AUXILIARY_PREDS = getAdditionalLanguageBias("auxiliary")
  */

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






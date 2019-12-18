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

package logic

import java.io.File

import app.runutils.Globals
import com.typesafe.scalalogging.LazyLogging
import logic.Examples.Example
import utils.{Utils, ASP}

import scala.collection.mutable.ListBuffer

object Theory {

  val empty = Theory()

  def apply(c: Clause) = new Theory(List(c))

  def mergeTheories(T: List[Theory]) = Theory(T flatMap (x => x.clauses))

  def compressTheory(l: Iterable[Clause]): List[Clause] = {
    val compressed = new ListBuffer[Clause]
    val included = (c: Clause) => compressed.toList.exists(x => x.thetaSubsumes(c) && c.thetaSubsumes(x))
    for (c <- l) {
      if (!included(c)) compressed += c
    }
    compressed.toList
  }

}

case class Theory(clauses: List[Clause] = List()) extends Expression with LazyLogging {

  var tps = new ListBuffer[String]
  var fps = new ListBuffer[String]
  var fns = new ListBuffer[String]

  /*
  def clearStats() = {
    this.tps = new ListBuffer[String]
    this.fps = new ListBuffer[String]
    this.fns = new ListBuffer[String]
  }

  def f1 = if (this.stats._6.toDouble.isNaN) 0.0 else this.stats._6.toDouble
  */

  def size = this.clauses.size

  def stats = {
    val tps = this.tps.distinct.length.toFloat
    val fps = this.fps.distinct.length.toFloat
    val fns = this.fns.distinct.length.toFloat
    val precision = tps / (tps + fps)
    val recall = tps / (tps + fns)
    val fscore = 2 * precision * recall / (precision + recall)
    (tps.toInt, fps.toInt, fns.toInt, precision, recall, fscore)
  }
  def clearStats() = {
    this.tps = new ListBuffer[String]
    this.fps = new ListBuffer[String]
    this.fns = new ListBuffer[String]
  }

  def showWithStats = (this.clauses map (_.showWithStats)).mkString("\n")

  def showWithStats_NoEC = (this.clauses map (_.showWithStats_NoEC)).mkString("\n")

  override val tostring =
    // if (Core.glvalues("withWeaks") == "true") {
    this.clauses.map { x => if (x.fromWeakExample) x.tostring + "  (weak rule)" else x.tostring }.mkString("\n")
  // } else {
  //   this.clauses.map { x => x.tostring }.mkString("\n")
  // }

  val isEmpty = this == Theory.empty

  def toPriorTheory = new PriorTheory(retainedRules = this)

  def thetaSubsumes(that: Theory): Boolean = {
    that.clauses.forall(p => this.clauses.exists(q => q.thetaSubsumes(p)))
  }

  def withTypePreds(globals: Globals) = clauses.map(_.withTypePreds(globals))

  /**
    * Returns "initiated" or "terminated". This is used by the streaming version
    */

  def getTargetClass = {
    val what = this.clauses.map(x => x.head.functor).toSet
    if (what.size > 1) {
      val msg = s"\nI'm learning both initiated and terminated rules in the same process!\n\nERROR:\n\n${this.tostring}"
      throw new RuntimeException(msg)
    }
    if (what.nonEmpty) what.head else "empty"
  }

  /**
    *
    * @return The marked rules and the marked rule preds (e.g. rule(234234)) as a single string ready for ASP use.
    *         Also a map of ruleId --> rule
    */
  def marked(globals: Globals): (String, Map[String, Clause]) = {
    val allRefinements = this.clauses flatMap (_.refinements)
    val allRules = this.clauses ++ allRefinements
    val markedTheory = this.clauses map (_.marked(globals))
    val markedRefinements = allRefinements map (_.marked(globals))
    val allRulesMarked = markedTheory ++ markedRefinements
    val hashCodesClausesMap = (allRules map (x => x.##.toString -> x)).toMap
    val rulePredicates = hashCodesClausesMap.keySet.map(x => s"rule($x). ").mkString("\n")
    (allRulesMarked.map(_.tostring).mkString("\n") + rulePredicates, hashCodesClausesMap)
  }

  /* Used when learning rules separately. */
  def scoreRules(example: Example, globals: Globals, postPruningMode: Boolean = false): Unit = {
    val targetClass = getTargetClass
    // If a rule has just been expanded its refinements are empty, so generate new
    if (!postPruningMode) {
      // Just to be on the safe side in the distributed case...

      /*
      if (Globals.glvalues("distributed").toBoolean) {
        if (this.clauses.exists(rule => rule.refinements.isEmpty)) {
          throw new RuntimeException(s"Found a rule with empty refinements set. That's an error because" +
            s" in the distributed setting the refinements' set is generated right after clause construction.")
        }
      }
      */
      this.clauses foreach (rule => if (rule.refinements.isEmpty) rule.generateCandidateRefs(globals))
    }

    // debug:
    //this.clauses.foreach(x => println(x.score))

    // Proceed to scoring
    val e = (example.annotationASP ++ example.narrativeASP).mkString("\n")
    val _marked = marked(globals)
    val markedProgram = _marked._1
    val markedMap = _marked._2
    //val countRules = globals.TIMES_COUNT_RULE
    val exmplCountRules = globals.EXAMPLE_COUNT_RULE

    //--------------------------------------------------------
    // For debugging
    //val allRefinements = this.clauses flatMap(_.refinements)
    //val allRules = this.clauses ++ allRefinements
    //println(s"rules being evaluated: ${allRules.size}")
    //--------------------------------------------------------

    val show = globals.SHOW_TPS_ARITY_2 + globals.SHOW_FPS_ARITY_2 + globals.SHOW_FNS_ARITY_2 +
      globals.SHOW_TIME + globals.SHOW_INTERPRETATIONS_COUNT
    val include = {
      targetClass match {
        case "initiatedAt" => globals.INCLUDE_BK(globals.BK_INITIATED_ONLY_MARKDED)
        case "terminatedAt" => globals.INCLUDE_BK(globals.BK_TERMINATED_ONLY_MARKDED)
      }
    }
    val all = e + include + exmplCountRules + markedProgram + show
    val f = Utils.getTempFile(s"isConsistent", ".lp")
    Utils.writeToFile(f, "append")(p => List(all) foreach p.println)
    val path = f.getCanonicalPath
    val answerSet = ASP.solve(task         = Globals.SCORE_RULES, aspInputFile = new File(path))

    f.delete()

    answerSet match {
      case Nil =>
        throw new RuntimeException("Got an empty answer set during rule evaluation (at least times count should be returned)")
      case _ =>
        val (exampleCounts, coverageCounts) = answerSet.head.atoms.foldLeft(List[String](), List[String]()){ (x, y) =>
          val exCount = x._1
          val coverageCounts = x._2
          if (y.startsWith("tps") || y.startsWith("fps") || y.startsWith("fns")) {
            (exCount, coverageCounts :+ y)

          } else if (y.startsWith("countGroundings")) {
            (exCount :+ y, coverageCounts)
          } else {
            throw new RuntimeException(s"Don't know what to do with what the solver" +
              s" returned.\nExpected tps/2,fps/2,fns/2,countGroundings/1 got\n${answerSet.head.atoms}")
          }
        }

        /*
        *
        * Don't throw this exception. There are cases where we do not have any groundings.
        * For instance consider this example from the maritime domain: We are learning the concept
        * terminatedAt(highSpeedIn(Vessel,Area),Time) and an example comes with no area in it.
        * Then there are no groundings and, correctly, the "seen examples" from our existing rules
        * should not be increased.
        * */
        //if (exampleCounts.isEmpty) throw new RuntimeException("No example count returned")

        // Normally, only one countGroundings/1 atom should be returned, with the number of
        // target concept groundings as its argument. If we have more than one target concept
        // then we could have more such atoms, but OLED does not handle that.
        if (exampleCounts.length > 1)
          throw new RuntimeException(s"Only one countGroundings/1 atom was expected, got ${exampleCounts.mkString(" ")} instead.")

        // increase the count for seen examples
        //val c = exampleCounts.size

        val c = exampleCounts.head.split("\\(")(1).split("\\)")(0).toInt

        this.clauses foreach { x =>
          x.seenExmplsNum += c //times//*100 // interps
          x.refinements.foreach(y => y.seenExmplsNum += c)
          x.supportSet.clauses.foreach(y => y.seenExmplsNum += c)
        }

        val parse = (atom: String) => {
          val tolit = Literal.parse(atom)
          val (what, hashCode, count) = (tolit.predSymbol, tolit.terms.head.tostring, tolit.terms.tail.head.tostring)
          (what, hashCode, count)
        }

        val updateCounts = (what: String, hashCode: String, count: String) => {
          val clause = markedMap(hashCode)
          what match {
            case "tps" => clause.tps += count.toInt
            case "fps" => clause.fps += count.toInt
            case "fns" => clause.fns += count.toInt
          }
        }

        coverageCounts foreach { x =>
          val (what, hashCode, count) = parse(x)
          updateCounts(what, hashCode, count)
        }
    }
  }

  def scoreRulesNoEC(example: Example, globals: Globals, postPruningMode: Boolean = false): Unit = {
    // If a rule has just been expanded its refinements are empty, so generate new
    if (!postPruningMode) this.clauses foreach (rule => if (rule.refinements.isEmpty) rule.generateCandidateRefs(globals))

    // Proceed to scoring
    val e = (example.annotationASP ++ example.narrativeASP).mkString("\n")
    val _marked = marked(globals)
    val markedProgram = _marked._1
    val markedMap = _marked._2
    //val countRules = globals.TIMES_COUNT_RULE
    val exmplCountRules = globals.EXAMPLE_COUNT_RULE

    val show = globals.SHOW_TPS_ARITY_2 + globals.SHOW_FPS_ARITY_2 + globals.SHOW_FNS_ARITY_2 + globals.SHOW_TIME + globals.SHOW_INTERPRETATIONS_COUNT
    val include = globals.INCLUDE_BK(globals.BK_RULE_SCORING_MARKDED)
    val all = e + include + exmplCountRules + markedProgram + show
    val f = Utils.getTempFile(s"isConsistent", ".lp")
    Utils.writeToFile(f, "append")(p => List(all) foreach p.println)
    val path = f.getCanonicalPath
    val answerSet = ASP.solve(task         = Globals.SCORE_RULES, aspInputFile = new File(path))

    f.delete()

    answerSet match {
      case Nil =>
        throw new RuntimeException("Got an empty answer set during rule evaluation (at least times count should be returned)")
      case _ =>
        val (exampleCounts, coverageCounts) = answerSet.head.atoms.foldLeft(List[String](), List[String]()){ (x, y) =>
          val exCount = x._1
          val coverageCounts = x._2
          if (y.startsWith("tps") || y.startsWith("fps") || y.startsWith("fns")) {
            (exCount, coverageCounts :+ y)

          } else if (y.startsWith("countGroundings")) {
            (exCount :+ y, coverageCounts)
          } else {
            throw new RuntimeException(s"Don't know what to do with what the solver" +
              s" returned.\nExpected tps/2,fps/2,fns/2,countGroundings/1 got\n${answerSet.head.atoms}")
          }
        }

        /*
        *
        * Don't throw this exception. There are cases where we do not have any groundings.
        * For instance consider this example from the maritime domain: We are learning the concept
        * terminatedAt(highSpeedIn(Vessel,Area),Time) and an example comes with no area in it.
        * Then there are no groundings and, correctly, the "seen examples" from our existing rules
        * should not be increased.
        * */
        //if (exampleCounts.isEmpty) throw new RuntimeException("No example count returned")

        // Normally, only one countGroundings/1 atom should be returned, with the number of
        // target concept groundings as its argument. If we have more than one target concept
        // then we could have more such atoms, but OLED does not handle that.
        if (exampleCounts.length > 1)
          throw new RuntimeException(s"Only one countGroundings/1 atom was expected, got ${exampleCounts.mkString(" ")} instead.")

        // increase the count for seen examples
        //val c = exampleCounts.size

        val c = exampleCounts.head.split("\\(")(1).split("\\)")(0).toInt

        this.clauses foreach { x =>
          x.seenExmplsNum += c //times//*100 // interps
          x.refinements.foreach(y => y.seenExmplsNum += c)
          x.supportSet.clauses.foreach(y => y.seenExmplsNum += c)
        }

        val parse = (atom: String) => {
          val tolit = Literal.parse(atom)
          val (what, hashCode, count) = (tolit.predSymbol, tolit.terms.head.tostring, tolit.terms.tail.head.tostring)
          (what, hashCode, count)
        }

        val updateCounts = (what: String, hashCode: String, count: String) => {
          val clause = markedMap(hashCode)
          what match {
            case "tps" => clause.tps += count.toInt
            case "fps" => clause.fps += count.toInt
            case "fns" => clause.fns += count.toInt
          }
        }

        coverageCounts foreach { x =>
          val (what, hashCode, count) = parse(x)
          updateCounts(what, hashCode, count)
        }
    }
  }

  /*
  * These variables are used for scoring the theory as a whole
  * */

  var _tps: Int = 0
  var _fps: Int = 0
  var _fns: Int = 0
  def precision: Double = _tps.toFloat / (_tps + _fps)
  def recall: Double = _tps.toFloat / (_tps + _fns)
  def fscore: Double = {
    val s = (2 * precision * recall) / (precision + recall)
    if (s.isNaN) 0.0 else s
  }
  def score = fscore

  /* Score this theory as a whole */
  def score(example: Example, globals: Globals, postPruningMode: Boolean = false) = {
    val e = (example.annotationASP ++ example.narrativeASP).mkString("\n")
    val include = globals.INCLUDE_BK(globals.BK_WHOLE_EC)
    val exmplCountRules = globals.EXAMPLE_COUNT_RULE
    val show = globals.SHOW_TPS_ARITY_1 + globals.SHOW_FPS_ARITY_1 + globals.SHOW_FNS_ARITY_1 + globals.SHOW_INTERPRETATIONS_COUNT

    val all =
      e + "\n" + include + "\n" + this.clauses.map(x => x.withTypePreds(globals).tostring).mkString("\n") +
        "\n" + exmplCountRules + globals.TPS_RULES + globals.FPS_RULES + globals.FNS_RULES + "\n" + show
    val f = Utils.getTempFile("isConsistent", ".lp", deleteOnExit = true)
    Utils.writeToFile(f, "append")(p => List(all) foreach p.println)
    val path = f.getCanonicalPath
    val answerSet = ASP.solve(task         = Globals.SCORE_RULES, aspInputFile = new File(path))

    f.delete()

    //val (exampleCounts, tpss, fpss, fnss) = (List[String](), List[String](), List[String](), List[String]())

    answerSet match {
      case Nil => throw new RuntimeException("Got an empty answer set during rule evaluation (at least times count should be returned)")
      case _ =>
        val (exampleCounts, tpss, fpss, fnss) = answerSet.head.atoms.foldLeft(List[String](), List[String](), List[String](), List[String]()){ (x, y) =>
          val exCount = x._1
          val tps = x._2
          val fps = x._3
          val fns = x._4
          if (y.startsWith("tps")) (exCount, tps :+ y, fps, fns)
          else if (y.startsWith("fps")) (exCount, tps, fps :+ y, fns)
          else if (y.startsWith("fns")) (exCount, tps, fps, fns :+ y)
          else if (y.startsWith("exampleGrounding")) (exCount :+ y, tps, fps, fns)
          else throw new RuntimeException(s"Don't know what to do with what the solver" + s" returned.\nExpected tps/2,fps/2,fns/2,exampleGrounding/1 got\n${answerSet.head.atoms}")

        }
        if (exampleCounts.isEmpty) throw new RuntimeException("No example count returned")

        val c = exampleCounts.size
        this.clauses foreach { x =>
          x.seenExmplsNum += c
          x.refinements.foreach(y => y.seenExmplsNum += c)
          x.supportSet.clauses.foreach(y => y.seenExmplsNum += c)
        }

        this._tps += tpss.length
        this._fps += fpss.length
        this._fns += fnss.length
        (exampleCounts.length, tpss.length, fpss.length, fnss.length)
    }
  }

  var meanScore = 0.0

  /*

  def updateMeanScore: Globals = {
    val newScore = this.fscore
    val newMeanScore = ((GlobalValues.TOP_THEORY_SCORE*GlobalValues.TOP_THEORY_SCORE_COUNT) + newScore) / (GlobalValues.TOP_THEORY_SCORE_COUNT + 1)
    GlobalValues.TOP_THEORY_SCORE_COUNT += 1
    GlobalValues.TOP_THEORY_SCORE = newMeanScore
    this.meanScore = GlobalValues.TOP_THEORY_SCORE
  }

  /* Score each candidate refinement, based on the utility of adding the refinement in the theory */
  def scoreRules2(example: Example, jep: Jep, globals: GlobalValues, postPruningMode: Boolean = false) = {
    // First get the score of the current running theory.
    // Note that this also updates the seen example counts for each "top" clause
    // (i.e. each clause in the running theory), for each bottom clause in the
    // support set of each top clause and for each one of the top-clause's candidate
    // refinements. So DO NOT update these counts again in what follows.
    // Sore the running hypothesis:
    this.score(example, jep, globals, postPruningMode)
    // Update the mean observed f-score for the running hypothesis:
    this.updateMeanScore
    // The topTheoryFscore above is new score for each clause C in the running
    // hypothesis (since each such clause belongs in the running hypothesis).
    // Score all refinements for each clause in the current hypothesis
    // Therefore, the new observed mean score for each such C is now (i.e. after the update
    // in the previous line):
    //----------------
    // this.meanScore
    //----------------
    println(s"top theory score: ${this.meanScore}")
    // So, we update the mean observed score for each such "top" clause:
    this.clauses.foreach(x => x.updateScoreLearnWholeTs(this.meanScore))
    // Next, score each refinement, for each clause in the current hypothesis:
    for (rule <- this.clauses) {
      if (!postPruningMode) {
        if (rule.refinements.isEmpty) rule.generateCandidateRefs
        for (ref <- rule.refinements) {
          val T = Theory(this.clauses.filter(x => x!=rule) :+ ref)
          // Score the alternative hypothesis:
          T.score(example,jep,globals,postPruningMode)
          // Update the mean observed score for the current clause.
          // This score is now the mean observed score of the alternative hypothesis T
          // that contains a refinement instead of a parent clause.
          T.updateMeanScore
          ref.updateScoreLearnWholeTs(T.meanScore)
          println(s"ref score: ${ref.meanScoreLearningWholeTheories}")
        }
      }
    }
  }

  */

  def growNewRuleTest(e: Example, target: String, globals: Globals): Boolean = {
    // we already have the target with the input (the target parameter).
    // But the one from the input is used only in case of an empty theory. In
    // other cases we get the target class by looking at the rules' heads,
    // just for some extra safety on whether we're indeed learning separately
    // (check out the exceptions thrown below in case we end with a mixture
    // of initiatedAt and terminated rules in the theory.)
    val targetClass = getTargetClass

      def solve(program: String): List[AnswerSet] = {
        val f = Utils.getTempFile(s"growNewRuleTest-for-$target", ".lp")
        Utils.writeToFile(f, "append")(p => List(program) foreach p.println)
        val path = f.getCanonicalPath
        //ASP.solve(task = Core.INFERENCE, aspInputFile = new File(path), jep=jep)
        ASP.solve(task         = Globals.GROW_NEW_RULE_TEST, aspInputFile = new File(path))
      }

    val (includeBKfile, failedTestDirective, show) = {
      targetClass match {
        // If we are learning the initiatedAt part of the the theory, then we must start growing
        // a new rule if we have FNs, i.e. no initiatedAt rule in the current hypothesis fires,
        // and fluents are not initiated when they should.
        case "initiatedAt" =>
          if (Globals.glvalues("with-inertia").toBoolean) {
            (globals.INCLUDE_BK(globals.INITIATED_ONLY_INERTIA), globals.FNS_RULES, globals.SHOW_FNS_ARITY_1)
          } else {
            (globals.INCLUDE_BK(globals.BK_INITIATED_ONLY), globals.FNS_RULES, globals.SHOW_FNS_ARITY_1)
          }

        // If we are learning the terminatedAt part of the the theory, then we must start growing
        // a new rule if we have FPs, i.e. no terminatedAt rule in the current hypothesis fires,
        // and fluents are not terminated when they should.
        case "terminatedAt" =>
          (globals.INCLUDE_BK(globals.BK_TERMINATED_ONLY), globals.FPS_RULES, globals.SHOW_FPS_ARITY_1)
        // In this case no theory has been generated yet. We therefore check if the current example
        // satisfies the empty theory with the plain isSAT method. To do that, we use the whole set
        // of EC axioms in the BK. Also, coverage directives (normally fps, tns etc) are coverage
        // constraints here, forcing the SOLVER to try to satisfy them and getting back an UNSAT program in case of failure.
        // Note that we use no #show here.
        case "empty" =>
          // the target is taken from the method's input here.
          (if (target == "initiatedAt") globals.INCLUDE_BK(globals.BK_INITIATED_ONLY)
          else globals.INCLUDE_BK(globals.BK_TERMINATED_ONLY),
            if (target == "initiatedAt") globals.CONSTRAINT_COVER_ALL_POSITIVES
            else globals.CONSTRAINT_EXCLUDE_ALL_NEGATIVES, "") // no #show
      }
    }

    val t = (this.withTypePreds(globals) map (_.tostring)).mkString("\n")
    // Getting exmplWithInertia here does not cause problems (in the initiated case). See comments at CaviarUtils.getDataAsChunks
    val ex = (e.annotationASP ++ e.narrativeASP).mkString("\n")

    val program = ex + includeBKfile + t + failedTestDirective + show
    // Fail if either one of the existing rules
    val failure = (atoms: List[String]) =>
      if (targetClass != "empty")
        targetClass match {
          case "initiatedAt" => atoms.exists(p => p.startsWith("fns"))
          case "terminatedAt" => atoms.exists(p => p.startsWith("fps"))
        }
      else atoms.head == globals.UNSAT // then the example does not satisfy the empty theory. No rules are needed.

    //val timeStart = System.nanoTime()

    val answerSet = solve(program)

    //val timeEnd = System.nanoTime()

    //println(s"growNewRuleTest solving time: ${(timeEnd-timeStart)/1000000000.0}")

    answerSet.nonEmpty match {
      case true =>
        val atoms = answerSet.head.atoms
        if (failure(atoms)) true
        else false
      case _ => false
    }
  }

  def growNewRuleTestNoEC(e: Example, globals: Globals): Boolean = {

      def solve(program: String): List[AnswerSet] = {
        val f = Utils.getTempFile(s"growNewRuleTest", ".lp")
        Utils.writeToFile(f, "append")(p => List(program) foreach p.println)
        val path = f.getCanonicalPath
        ASP.solve(task         = Globals.GROW_NEW_RULE_TEST, aspInputFile = new File(path))
      }

    val (failedTestDirective, show) = (globals.FNS_RULES, globals.SHOW_FNS_ARITY_1)

    val t = (this.withTypePreds(globals) map (_.tostring)).mkString("\n")
    val ex = (e.annotationASP ++ e.narrativeASP).mkString("\n")

    val program = ex + t + failedTestDirective + show

    val failure = (atoms: List[String]) => atoms.exists(p => p.startsWith("fns"))

    val answerSet = solve(program)

    answerSet.nonEmpty match {
      case true =>
        val atoms = answerSet.head.atoms
        if (failure(atoms)) true
        else false
      case _ => false
    }
  }

  def growNewRuleTestWholeTheories(e: Example, globals: Globals): Boolean = {

      def solve(program: String): List[AnswerSet] = {
        val f = Utils.getTempFile(s"growNewRuleTest", ".lp", deleteOnExit = true)
        Utils.writeToFile(f, "append")(p => List(program) foreach p.println)
        val path = f.getCanonicalPath
        //ASP.solve(task = Core.INFERENCE, aspInputFile = new File(path), jep=jep)
        ASP.solve(task         = Globals.GROW_NEW_RULE_TEST, aspInputFile = new File(path))
      }

    val (includeBKfile, failedTestDirective, show) = {
      (globals.INCLUDE_BK(globals.BK_WHOLE_EC),
        List(globals.FPS_RULES, globals.FNS_RULES).mkString("\n"),
        List(globals.SHOW_FNS_ARITY_1, globals.SHOW_FPS_ARITY_1).mkString("\n"))
    }

    val t = (this.withTypePreds(globals) map (_.tostring)).mkString("\n")
    val ex = (e.annotationASP ++ e.narrativeASP).mkString("\n")

    val program = ex + includeBKfile + t + failedTestDirective + show
    // Fail if either one of the existing rules
    val failure = (atoms: List[String]) => atoms.exists(p => p.startsWith("fns") || p.startsWith("fps"))

    //val timeStart = System.nanoTime()
    val answerSet = solve(program)
    //val timeEnd = System.nanoTime()

    //println(s"growNewRuleTest solving time: ${(timeEnd-timeStart)/1000000000.0}")

    answerSet.nonEmpty match {
      case true =>
        val atoms = answerSet.head.atoms
        if (failure(atoms)) true
        else false
      case _ => false
    }
  }

  def use_2_split(globals: Globals): (Theory, Map[String, Literal]) = {
    /*
     val t = (for ((c, i) <- this.clauses zip List.range(1, this.clauses.length + 1))
               yield c.use_2_split(i)).map { x => List(x._1, x._2) }.transpose
      val defeasibles = Theory((for (x <- t(0)) yield x.asInstanceOf[Theory].clauses).flatten)
      val use = t(1).asInstanceOf[List[Map[String, Literal]]].reduce((x, y) => x ++ y)
      (defeasibles, use)
      */

    this match {
      case Theory.empty => (Theory(), Map[String, Literal]())
      case _ =>
        val t = (for ((c, i) <- this.clauses zip List.range(1, this.clauses.length + 1))
          yield c.use_2_split(i, globals)).map { x => List(x._1, x._2) }.transpose
        val defeasibles = Theory((for (x <- t(0)) yield x.asInstanceOf[Theory].clauses).flatten)
        val use = t(1).asInstanceOf[List[Map[String, Literal]]].reduce((x, y) => x ++ y)
        (defeasibles, use)
    }
  }

  /*
  def withStrongSupportsOnly = {
    val x = this.strongRules.clauses.map{
      p => Clause(head=p.head,body=p.body,fromWeakExample=p.fromWeakExample,supportSet=p.supportSet.strongRules)
    }
    if (Core.glvalues("withWeaks").toBoolean) Theory(x) else this // don't complicate things in strong-only learning
  }
  */
  def filterSupports(filterWhat: String) = {
    val f = (x: Theory) => filterWhat match {
      case "strongRules" => x.strongRules
      case "weakRules" => {
        val weakRules = x.weakRules
        if (!weakRules.isEmpty) weakRules else x
      }
    }
    val x = this.strongRules.clauses.map{
      p =>
        Clause(
          head            = p.head,
          body            = p.body,
          fromWeakExample = p.fromWeakExample,
          supportSet      = f(p.supportSet))
    }
    if (Globals.glvalues("withWeaks").toBoolean) Theory(x) else this // don't complicate things in strong-only learning
  }

  def strongWeakSplit = {
    val (strongs, weaks) = this.clauses.foldLeft(List[Clause](), List[Clause]()) {
      (x, y) =>
        val (strongRules, weakRules) = (x._1, x._2)
        y.fromWeakExample match {
          case true => (strongRules, weakRules :+ y)
          case false => (strongRules :+ y, weakRules)
        }
    }
    (strongs, weaks)
  }

  /**
    * This is not used anywhere. I generates a defeasible theory from all rules
    * in the prior hypothesis and for each rule, from all the rules in its
    * support set. It's not necessary to search in such a large program,
    * specializations are implemented in a ryle-by-rule fashion.
    */

  def use_3_spilt_one(withSupport: String = "fullSupport", globals: Globals) = {
    if (this != Theory()) {
      val z = this.clauses zip List.range(1, this.clauses.length + 1) map
        (x => x._1.use_3_split_one(x._2, withSupport = withSupport, globals = globals)) map (x => List(x._1, x._2, x._3))
      val t = z.transpose
      val defeasibles = Theory((for (x <- t(0)) yield x.asInstanceOf[Theory].clauses).flatten)
      val use3map = t(1).asInstanceOf[List[Map[String, Literal]]].reduce((x, y) => x ++ y)
      val use3generates = t(2).asInstanceOf[List[String]]
      (defeasibles, use3map, use3generates)
    } else {
      (Theory(), Map[String, Literal](), List[String]())
    }
  }

  /**
    *
    * Same thing as above, but this analyses a rule using its whole support.
    *
    * @todo This needs to be refactored and merged with the one above
    */

  def use_3_split_all(withSupport: String = "fullSupport", globals: Globals) = {
    if (this != Theory()) {
      val z = this.clauses zip List.range(1, this.clauses.length + 1) map
        (x => x._1.use_3_split(x._2, withSupport = withSupport, globals = globals)) map (x => List(x._1, x._2, x._3))
      val t = z.transpose
      val defeasibles = Theory((for (x <- t(0)) yield x.asInstanceOf[Theory].clauses).flatten)
      val use3map = t(1).asInstanceOf[List[Map[String, Literal]]].reduce((x, y) => x ++ y)
      val use3generates = t(2).asInstanceOf[List[String]]
      (defeasibles, use3map, use3generates)
    } else {
      (Theory(), Map[String, Literal](), List[String]())
    }
  }

  def use_3_split(globals: Globals): (Theory, Map[String, Literal]) = {

    val z = this.clauses zip List.range(1, this.clauses.length + 1) map (x => x._1.use_3_split(x._2, globals = globals)) map (x => List(x._1, x._2))
    val t = z.transpose
    val defeasibles = Theory((for (x <- t(0)) yield x.asInstanceOf[Theory].clauses).flatten)
    val use = t(1).asInstanceOf[List[Map[String, Literal]]].reduce((x, y) => x ++ y)
    (defeasibles, use)
  }

  def map(f: (Clause => Any)) = this.clauses map f

  def strongRules = Theory(this.clauses.filter(x => !x.fromWeakExample))
  def weakRules = Theory(this.clauses.filter(x => x.fromWeakExample))

  def extend(that: Theory): Theory = {
    /*
     def getStrongRules(t: Theory) = Theory(this.clauses.filter(x => !x.fromWeakExample))
     def check(t: Theory) = {
       if (getStrongRules(this).clauses.length != getStrongRules(t).clauses.length){
         throw new RuntimeException("Some strong rules got lost!")
       }
     }
     */
    val t = Theory((this.clauses ++ that.clauses).distinct)
    //check(t)
    t
  }

  def extendUnique(that: Theory): Theory = {
    val v = for (x <- that.clauses if !this.containsRule(x)) yield x
    val t = Theory(this.clauses ++ v)
    t
  }

  def compress: Theory = {
    val t = this.clauses.foldLeft(List[Clause]()){
      (p, q) =>
        if (!q.fromWeakExample)
          if (p.exists(x => !x.fromWeakExample && x.thetaSubsumes(q) && q.thetaSubsumes(x))) p
          else p :+ q
        else if (p.exists(x => x.thetaSubsumes(q) && q.thetaSubsumes(x))) p
        else p :+ q
    }
    Theory(t)
  }

  def containsRule(c: Clause) = {
    this.clauses.exists(x => x.thetaSubsumes(c) && c.thetaSubsumes(x))
  }

  def updateSupports(kernel: Theory, fromWeakExample: Boolean = false) = {
    LogicUtils.updateSupport(this, kernel, fromWeakExample = fromWeakExample)
  }

  def clearSupports(e: Example, globals: Globals) = { // it also removes inconsistent weak rules

    // Don't throw away weak rules before trying to refine them

    //val consistents =
    //  this.clauses.filter(x => !x.fromWeakExample) ++ this.clauses.filter(x => x.fromWeakExample).filter(x => x.isConsistent(e))

    val consistents = this.clauses

    /* // This is too slow
    consistents.foreach {
      x =>
        logger.info(s"Checking consistency for weak rules ${this.clauses.indexOf(x)}")
        x.supportSet.clauses.foreach {
        y =>
          if (!y.isConsistent(e)) {
            if (y.fromWeakExample) {
              x.removeFromSupport(y)
              logger.info("Removed inconsistent (weak) support set rule")
            } else {
              logger.error(s"Strong support set rule covers negatives")
              System.exit(-1)
            }
          }
      }
    }
    */

    ///* // This delegates consistency checking for all rules at once to the ASP solver
    if (consistents != Nil) {
      val path = ASP.isConsistent_program_Marked(Theory(consistents), e, globals)
      val answerSet = ASP.solve(task         = Globals.INFERENCE, aspInputFile = new File(path))
      if (answerSet != Nil) {
        val f = (a: String) => {
          val tolit = Literal.parse(a)
          val (i, j) = (tolit.terms.head.tostring.toInt, tolit.terms.tail.head.tostring.toInt)
          (i, j)
        }
        val grouped = answerSet.head.atoms.map(x => f(x)).groupBy{ _._1 }.map{ case (k, v) => k -> v.map(y => y._2).distinct }
        for (x <- grouped.keySet) {
          val rule = this.clauses(x)
          val toBeRemoved = grouped(x) map (p => rule.supportSet.clauses(p))
          for (rm <- toBeRemoved) {
            if (rm.fromWeakExample) {
              rule.removeFromSupport(rm)
              logger.info(s"Removed inconsistent support rule: \n ${rm.tostring} \n")
            } else {
              logger.error(s"Strong support set rule covers" +
                s" negatives: \n ${rm.tostring} \n ${answerSet.head.atoms}")
              throw new RuntimeException(s"Strong support set rule covers" +
                s" negatives: \n ${rm.tostring} \n ${answerSet.head.atoms}")
            }
          }
        }
      }
    }
    //*/

    val removeRule = (x: Clause) => x.supportSet.isEmpty match {
      case false => false
      case _ => x.fromWeakExample match {
        case true => true
        case _ => throw new RuntimeException(s"Strong rule with empty support: \n ${x.tostring} ${x.fromWeakExample}")
      }
    }

    val keep = (x: Theory) => Theory(x.clauses.filter(x => !removeRule(x)))
    keep(Theory(consistents))
  }

}
/*
object PriorTheory {
  def apply(pt: PriorTheory) = {
    new PriorTheory(retainedRules = pt.merge)
  }
}
*/
class PriorTheory(
    val retainedRules: Theory = Theory(),
    val newRules: Theory = Theory(),
    val refinedRules: Theory = Theory()) extends Theory {

  def merge = Theory(
    this.retainedRules.clauses ++
      this.newRules.clauses ++
      this.refinedRules.clauses
  )

  override def updateSupports(kernel: Theory, fromWeakExample: Boolean) = {
    LogicUtils.updateSupport(this.newRules, kernel, fromWeakExample)
    LogicUtils.updateSupport(this.retainedRules, kernel, fromWeakExample)
    LogicUtils.updateSupport(this.refinedRules, kernel, fromWeakExample)
  }

  override def clearSupports(e: Example, globals: Globals) = {
    val news = this.newRules.clearSupports(e, globals)
    val ret = this.retainedRules.clearSupports(e, globals)
    val ref = this.refinedRules.clearSupports(e, globals)
    new PriorTheory(retainedRules = ret, newRules = news, refinedRules = ref)
  }

  override val isEmpty = this.merge.isEmpty

  override val tostring = this.merge.tostring

  /*
  override def compress: PriorTheory = {
    def checkAgainst(currentRules: Theory, allOtherRules: List[Theory]) = {
      val allOthers = allOtherRules.foldLeft(List[Clause]()){(x,y) => x ++ y.clauses}
      val keep = currentRules.clauses.foldLeft(List[Clause]()){
        (p,q) =>
          if (!q.fromWeakExample) p :+ q
            //if ((p++allOthers).exists(x => !x.fromWeakExample && x.thetaSubsumes(q) && q.thetaSubsumes(x))) p
            //else p :+ q
          else
            if ((p++allOthers).exists(x => x.thetaSubsumes(q) && q.thetaSubsumes(x))) p
            else p :+ q
      }
      Theory(keep)
    }
    val news = checkAgainst(this.newRules, List(this.refinedRules,this.retainedRules))
    val retained = checkAgainst(this.retainedRules, List(news,this.refinedRules))
    val refined = checkAgainst(this.refinedRules, List(news, retained))
    new PriorTheory(retainedRules=retained,newRules=news,refinedRules=refined)
  }
  */
}

/*
case class DefeasibleProgram(kernelSet: Theory = Theory(), priorTheory: Theory = Theory()) {

  private val _priorTheory = priorTheory match {
    case x: Theory => x
    case x: PriorTheory => x.merge
  }

  private val splitKernel = new DefeasibleKernel
  private val splitPrior = new DefeasiblePrior

  class DefeasibleKernel {
    private def split = kernelSet.use_2_split
    val isEmpty = this.split == (Theory.empty, Map())
    val defeasibleKS = split._1
    val use2AtomsMap = split._2
  }

  class DefeasiblePrior {
    private def split = _priorTheory.use_3_spilt_one
    val isEmpty = this.split == (Theory.empty, Map(),List())
    val defeasiblePrior = split._1
    val use3AtomsMap = split._2
    val use3generates = split._3
  }



}
*/

package iled.structures

import iled.core.{Iled, Core}
import iled.globalValues.GlobalValues
import iled.parsers.ModesParser
import iled.structures.Examples.Example
import iled.structures.Exceptions._
import iled.utils.{ASP, Utils}
import jep.Jep

import scala.collection.mutable.ListBuffer
import scala.util.Random
import iled.core.ClauseImplicits._

/**
 * Companion object
 */

object Clause extends ModesParser {

  val empty = Clause()

  def apply(lits: List[Literal]) = {
    new Clause(head = lits.head.asPosLiteral, body = lits.drop(1))
  }

  def apply(c: Clause, ss: Theory) = {
    new Clause(head=c.head,body=c.body,supportSet=ss,fromWeakExample=c.fromWeakExample)
  }

  def updateField(c: Clause, fromWeakExample: Boolean) = {
    new Clause(head = c.head, body = c.body, supportSet = c.supportSet, fromWeakExample = fromWeakExample)
  }

  def apply(head: Literal, body: List[Literal]) = {
    new Clause(head = head.asPosLiteral, body = body)
  }

  /* Parses a string into a Clause. */

  def toClause(cl: String): Clause = {
    getParseResult(parse(clause, cl)).asInstanceOf[Clause]
  }


}

case class Clause(head: PosLiteral = PosLiteral(),
                  body: List[Literal] = Nil,
                  fromWeakExample: Boolean = false,
                  var supportSet: Theory = Theory()) extends Expression {


  /**
   * True positives, false positive and false negatives
   * covered by this clause (respectively). These are
   * intended to be used as accumulative scores (in streaming mode).
   * Also there are used in the noise-tolerant setting to identify
   * "good" support clauses to use for refinement.
   */

  var tps: Int = 0
  var fps: Int = 0
  var fns: Int = 0
  def precision: Double = tps.toFloat / (tps + fps)
  def recall: Double = tps.toFloat / (tps + fns)
  //def score: Double = if (Core.glvalues("ruleEvaluationFunction") == "precision" && !precision.isNaN) precision else 0.0

  var refinements = List[Clause]()
  //var refinements = List[Refinement]()

  // The number of examples until the Hoeffding test succeeds
  var seenExmplsNum = 0

  // The number over which the mean of best scores' difference
  // has been computed. When a new mean score difference newDiff arrives,
  // to find the new mean we simply need to calculate
  //
  // newMean = (oldMean*previousCount + newDiff)/(previousCount+1)
  //
  // see also the calculation of meanDiff below.
  var previousCount = 0

  // Stores the previous mean difference observed between
  // the best and second-best specializations. It is used
  // for the calculation of the current mean in meanDiff method.
  var previousMeanDiff = 0.0

  // Very often, the best and second best specializations have the same score. It may be
  // desirable to assess performance if we group specializations by score, and then
  // consider as the best scoring one a representative of the best scoring group and
  // as the second-best, a representative of the second-best scoring group.
  // (this setting is only for experimentation)
  // previousMeanDiff2 keeps track of the mean difference of these quantities.
  var previousMeanDiff2 = 0.0

  // This retunrs
  def meanDiff = {
    // The - sign is to order with decreasing order (default is with increasing)
    val bestTwo = this.refinements.sortBy { x => - x.score }.take(2)
    val (best,secondBest) = (bestTwo.head,bestTwo.tail.head)
    val newDiff = best.score - secondBest.score
    val newMeanDiff = (previousMeanDiff*previousCount + newDiff)/(previousCount+1)
    previousCount += 1 // increase the count
    previousMeanDiff = newMeanDiff

    // Compute also the mean of the quantities in the comment above the previousMeanDiff2
    // variable definition (this is only for experimentation, comment out its calculation)
    // when you're done with it.
    //val grouped = this.refinements.groupBy(_.score)

    (newMeanDiff,best,secondBest)
  }

  def clearStatistics = {
    tps = 0
    fps = 0
    fns = 0
    seenExmplsNum = 0
    refinements = Nil
    previousCount = 0
    previousMeanDiff = 0
    previousMeanDiff2 = 0
  }

  def score: Double =
    if (this.head.functor=="initiatedAt"){
      if (!precision.isNaN) precision else 0.0
    } else {
      if (!recall.isNaN) recall else 0.0
    }

  def showWithStats = {
    val pos = s"tps: $tps"
    val negs = if (this.head.functor=="initiatedAt") s"fps: $fps" else s"fns: $fns"
    s"score (${if (this.head.functor=="initiatedAt") "precision" else "recall"}): $score, tps: $tps, fps: $fps, fns: $fns\n$tostring"
  }

  def mestimate(mparam: Double, totalPositives: Int, totalNegatives: Int): Double = {
    // The Laplace estimate is:
    // l = (positivesCovered + m * relFreq)/totalExamplesCovered + m
    // where
    // positivesCovered = the number of positives covered by the clause
    // relFreq = the relative frequency of positives in the training set (pos/pos+negs)
    // totalExamplesCovered = the total number of examples covered by the clause
    // m = a parameter set according tho the expected amount of noise (larger m for more noise)
    // We'll use the laplace m-estimate where m = 2 and p(+|clause) = 0.5

    //val positivesRelativeFrequency = totalPositives.toFloat / (totalPositives.toFloat + totalNegatives.toFloat)
    //val totalExmplsCovered = tps+fps
    //(tps.toFloat + (mparam * positivesRelativeFrequency)) / (totalExmplsCovered.toFloat + mparam)
    (tps.toFloat + 1) / (tps.toFloat+fps.toFloat+2.0)
  }



  def updateStatistics(tps: Int, fps: Int, fns: Int, meanScoreDiff: Double, meanScoreDiff2: Double, exmplsCount: Int) = {
    this.tps = tps
    this.fps = fps
    this.fns = fns
    this.seenExmplsNum = exmplsCount
  }

  // This variable stores a set of constructed
  // refinements. This is used in case this clause
  // is a support set clause (so it's always an empty
  // list in case this is a regular clause). Also,
  // this variable is used only in the noise-tolerant
  // version. It stores the refinements of a parent rule
  // that have been already generated (and scored) from this
  // support set rule.

  // Haven't decided the final type of this var.
  // I don't know if the Refinement class will be
  // of any use.






  // generates candidate refinements for the Hoeffding test
  def generateCandidateRefs: Unit = {
    val specializationDepth = GlobalValues.glvalues("specializationDepth").toInt
    val candidateList = this.supportSet.clauses.flatMap(_.body).distinct.filter(!this.body.contains(_))
    val refinementsSets =
      (for (x <- 1 to specializationDepth) yield x).foldLeft(List[List[Clause]]()) { (accum, depth) =>
      val z = for (lits <- candidateList.toSet.subsets(depth).toList) yield Clause(this.head, this.body ++ lits)
      accum :+ z
    }
    val flattend = refinementsSets.flatten
    //flattend.foreach(x => println(x.tostring))
    this.refinements = flattend
  }

  def marked = {
    Clause(head=Literal(functor = "marked", terms=List(this.##.toString, this.head)),body=this.withTypePreds().body)
  }

  val isEmpty = this == Clause.empty

  def addToSupport(c: Clause) = {
    this.supportSet = Theory(this.supportSet.clauses :+ c)
  }

  def addToSupport(c: List[Clause]) = {
    this.supportSet = Theory(this.supportSet.clauses ++ c)
  }

  def removeFromSupport(c: Clause) = {
    this.supportSet = Theory(this.supportSet.clauses.filter(x => x!=c))
  }

  // This is used in order to avoid maintaining redundant
  // rules in the support set. In this context, a support set
  // rule is redundant if it subsumes some other rule
  // in the support set. This can happen in cases where e.g.
  // p :- q,r was added to the support set early on and
  // later on p :- q,r,s was added. In this case the first
  // rule is redundant and should be removed. This redundancy
  // checking should be done every time the support set
  // changes with the addition of a rule.

  def compressSupport = {
    val redundants = this.supportSet.clauses filter {
      p => this.supportSet.clauses exists {
        q => !p.equals(q) && (p thetaSubsumes q)
      }
    }
    this.supportSet =
      Theory(this.supportSet.clauses filter (p => !redundants.contains(p)))
  }

  // When a rule is consistent w.r.t.
  // a set of examples w, but there is no rule in its
  // support that entails precisely the examples that
  // this rule entails in w. Then its support needs
  // to be updated by 'encoding' the uncovered examples
  // with a set of new lifted bottom clauses.
  def supportNeedsUpdate(e: Example, jep: Jep): Boolean = {
    val coversThis = ASP.inference(this,e, jep=jep).atoms.toSet // what this covers
    val noUpdate = this.supportSet.clauses.exists(
        p => ASP.inference(p,e, jep=jep).atoms.toSet == coversThis // then no update is needed
      )
    ! noUpdate
  }

  def isConsistent(e: Example, jep: Jep): Boolean = {
    Iled.isSAT(Theory(List(this)),e,ASP.isConsistent_program, jep=jep)
  }

  override def tostringQuote = this.tostring

  override val tostring = this.toStrList match {
    case List() => throw new LogicException("Cannot generate a Clause object for the empty clause")
    case h :: ts =>
      ts.length match {
        case 0 => h + "."
        case 1 => h + " :- \n" + "      " + ts.head + "."
        case _ => h + " :- \n" + (for (x <- ts) yield if (ts.indexOf(x) == ts.length - 1) s"      $x."
        else s"      $x,").mkString("\n")
      }
  }

  /* No new line after each literal */
  def tostring_debug = this.toStrList match {
    case List() => throw new LogicException("Cannot generate a Clause object for the empty clause")
    case h :: ts =>
      ts.length match {
        case 0 => h + "."
        case 1 => h + " :- " + ts.head + "."
        case _ =>
          h + " :- " + (for (x <- ts) yield
            if (ts.indexOf(x) == ts.length - 1) s"$x."
            else s"$x,").mkString("")
      }
  }

  def varbed: Clause = {
    var accum = ListBuffer[Literal]()
    var map = scala.collection.mutable.Map[Expression, Expression]()
    var counter = 0
    for (x <- this.toLiteralList) {
      val (a, _, c, d) = x.variabilize(List(Literal(functor = x.functor, isNAF = x.isNAF)),
        x.terms zip x.modeAtom.args, map, List(), counter)
      val aa = Literal(a.head.functor, a.head.terms, a.head.isNAF, x.modeAtom, a.head.typePreds)
      accum ++= List(aa)
      map ++ c
      counter = d
    }
    val l = accum.toList
    val out = Clause(head = l.head, body = l.tail)
    out
  }

  /**
   * this theta-subsumes that
    *
    * @param that @tparam Clause the (potentially) sumbumed clause
   * @return true if this subsumes that else false
   */

  def thetaSubsumes(that: Clause): Boolean = {
    /*
    this.toLiteralList.forall{ l =>
      that.toLiteralList.exists { l2 =>
        f(l,l2)
      }
    }
    */
    val isVar = (x: String) => try {
      Variable(x); true
    } catch {
      case _: IllegalArgumentException => false
    }
    val (skolemised, skmap) = that.skolemise
    var skolems = (for (y <- skmap.keySet.filter(x => isVar(x))) yield skmap(y)).toList
    val thisVars = this.getVars
    while (thisVars.length > skolems.length) {
      skolems = skolems ::: skolems
    }
    for (x <- skolems.permutations) {
      val trySubstitution = (thisVars zip x).map { x => (x._1, Constant(x._2)) }.toMap
      val repl = this.toLiteralList.map { x => x.replaceAll(trySubstitution) }.map { x => x.tostring }
      if (Utils.isSubset(repl.toSet, skolemised.toStrList.toSet)) return true
    }
    false
  }

  def thetaSubsumes(t: Theory): Boolean = {
    t.clauses.forall(x => this.thetaSubsumes(x))
  }

  /**
   * Same as above, but returns a List[String].
   */

  def toStrList: List[String] = List(head.tostring) ++ (for (x <- body) yield x.tostring)

  /**
   * Get the variables from this clause
   */

  def getVars = {
    val vars = this.head.asLiteral.getVars
    for (x <- this.body) vars ++= x.getVars.filter { x => !vars.contains(x) }
    vars.toList
  }

  /**
   * Replaces all variables with a new constant symbol 'skolem0', 'skolem1' etc. Same variables correspond to the
   * same constant symbol. Constants remain intact, i.e. they are used as skolem constants themselves. Example:
   *
   * a(X,Y,Z) :-
   * p(x,q(Y,const1,2),Z),
   * not r(A,B,C).
   *
   * is turned into:
   *
   * a(skolem0,skolem1,skolem2) :-
   * p(skolem0,q(skolem1,const1,2),skolem2),
   * not r(skolem3,skolem4,skolem5).
   *
   * Returns the skolemised clause and the 'vars -> skolems' map
   *
   */

  def skolemise: (Clause, Map[String, String]) = {
    val l = this.toLiteralList
    val skmap = this.getSkolemConsts
    var temp = new ListBuffer[Literal]
    for (x <- l) {
      val m = x.skolemize(skmap).toList
      val toLit = Literal(functor = x.functor, terms = m, isNAF = x.isNAF)
      temp += toLit
    }
    val fl = temp.toList
    val sk = Clause(head = fl.head,
      body = for (x <- fl; if fl.indexOf(x) != 0 ) yield x)
    (sk, skmap)
  }

  /**
   * Generates skolem constants from the variables and the constants of the clause. It returns a map of the form
   * Map('X -> skolem0', 'Y -> skolem1', 'const -> const', .... ) (we use the constants as skolem constants)
   */

  private def getSkolemConsts: Map[String, String] = {
    val l = this.toLiteralList
    //print(l)
    var skolems = new ListBuffer[(String, String)]
    var counter = 0
    for (x <- l) {
      val m = x.getSkolemConsts(skolems, counter);
      skolems = m._1; counter = m._2
    }
    skolems.toMap
  }

  def use_2_split(i: Int): (Theory, Map[String, Literal]) = {
    val temp = for (
      (lit, j) <- this.toLiteralList zip List.range(0, this.toLiteralList.length);
      vars = lit.variables;
      _try = Literal(functor = "try",
        terms = List(Constant(s"$i"), Constant(s"$j"),
        Literal(functor = "vars",
                terms = for (x <- vars) yield Variable(x.name, _type = x._type))),
                typePreds = for (x <- vars) yield s"${x._type}(${x.name})");
      tryLit = j match {
        case 0 => Literal(functor = "use2", terms = List(Constant(s"$i"), Constant("0")))
        case _ => _try
      };
      useMap = j match {
        case 0 => s"use2($i,0)" -> this.head.asLiteral
        case _ => s"use2($i,$j)" -> Literal(functor = lit.functor, terms = lit.terms,
          isNAF = lit.isNAF, typePreds = _try.typePreds, modeAtom = lit.modeAtom)
      };

      tryClause1 = if (j > 0)
        Clause(head = _try,
          body = List(Literal(functor = "use2",
            terms = List(Constant(s"$i"), Constant(s"$j")) ), lit))
      else None;
      tryClause2 = if (j > 0)
        Clause(head = _try, body = List(Literal(functor = "use2",
          terms = List(Constant(s"$i"), Constant(s"$j")), isNAF = true)) :::
          (for (x <- _try.typePreds) yield Literal(x) ))
      else None

    ) yield (tryLit, tryClause1, tryClause2, useMap)
    val ls = temp.map { x => List(x._1, x._2, x._3, x._4) }.transpose
    val defeasible = Theory(
      (List(Clause(head = this.head, body = ls.head.map(x => x.asInstanceOf[Literal]))) :::
        ls(1).filter { x => x != None }.map(x => x.asInstanceOf[Clause]) :::
        ls(2).filter { x => x != None }.map(x => x.asInstanceOf[Clause])).map { x => x.withTypePreds() })
    //val useMap = ls(3).asInstanceOf[List[(String,Literal)]].groupBy(_._1).map { case (k,v) => (k,v.map(_._2))}
    val useMap = ls(3).asInstanceOf[List[(String, Literal)]].groupBy(_._1).map { case (k, v) => v.head }
    (defeasible, useMap)
  }

  /**
   * Generates a defeasible theory from a single support set rule. This rule
   * may either be chosen at random from the support set, or it can be set
   * to a particular one (given with the input). This is used in order to identify
   * inconsistent rules from the prior hypothesis w.r.t. a new example window.
   *
   */

  def use_3_split_one(i: Int, ssClause: Clause = Clause.empty, withSupport: String="fullSupport") = {
    if (this != Clause.empty) {
      val SSRule = ssClause match {
        case Clause.empty =>
          if (withSupport == "fullSupport")
            Random.shuffle(this.supportSet.clauses).head // select an arbitrary rule
          else
            Random.shuffle(this.supportSet.strongRules.clauses).head // select a strong rule
        case _ => ssClause
      }
      val j = this.supportSet.clauses.indexOf(SSRule) + 1
      val (defeasible, useMap) = this.f(SSRule, i, j)
      (Theory(defeasible), useMap, s"{use3($i,$j,1..${SSRule.body.length})}.")
    } else {
      (Theory(), Map[String, Literal](), "")
    }
  }


  /**
    * generates a defeasible theory from this clause, using every rule in its
    * support set. Each defeasible theory resulting from each support set rule
    * is merged with the others.
    */

  def use_3_split(i: Int, withSupport: String = "fullSupport") = {
    //val support = if(withSupport=="fullSupport") this.supportSet else this.supportSet.strongRules
    val e = if(withSupport=="fullSupport") { // analyze the whole support set
      for ((x, j) <- this.supportSet.clauses zip List.range(1, this.supportSet.clauses.length + 1)) yield f(x, i, j)
    } else { // analyze only the strong support rules, by skipping the weak ones. Indexing remains consistent with the actual ordering in the support
      for ((x, j) <- this.supportSet.clauses zip List.range(1, this.supportSet.clauses.length + 1) if !this.supportSet.clauses(j-1).fromWeakExample) yield f(x, i, j)
    }
    val g = e.foldLeft(Tuple2(List[Clause](), Map[String, Literal]()))((x, y) => (x._1 ++ y._1, x._2 ++ y._2))
    val generates = (this.supportSet.clauses zip List.range(1, this.supportSet.clauses.length + 1)).map(p => s"{use3($i,${p._2},1..${p._1.body.length})}.").mkString("\n")
    (Theory(g._1), g._2,generates)
  }


  def f(c2: Clause, i: Int, j: Int)  = {
    val usedPart = c2.body filter (x => !this.body.contains(x))
    val (iconst, jconst) = (Constant(s"$i"), Constant(s"$j"))
    val vars = this.head.asLiteral.variables
    val varlit = Literal(functor = "vars", terms = vars map (x => Variable(x.name, _type = x._type)))
    val exceptionLit = Literal(functor = "exception",
      terms = List(iconst, jconst, varlit), isNAF = true)
    val res = (
      for (
        (x, k) <- usedPart zip List.range(1, usedPart.length + 1);
        use3lit = Literal(functor = "use3", terms = List(Constant(s"$i"), Constant(s"$j"), Constant(s"$k")))
      ) yield (Clause(head = exceptionLit.nonNegated, body = List(use3lit, x.negateThis)),
        use3lit.tostring -> x)).map { z => List(z._1, z._2) }.transpose
    val defeasible =
      (List(Clause(this.toLiteralList :+ exceptionLit)) ::: res.head.
        map { x => x.asInstanceOf[Clause] }).
        map { x => x.withTypePreds(extraTypePreds = this.head.asLiteral.getTypePredicates) }
    val useMap = res(1).asInstanceOf[List[(String, Literal)]].groupBy(_._1).map { case (k, v) => v.head }
    (defeasible, useMap)
  }



  /**
   * Helper method that converts a clause to a List[Literal] with the head of the clause as the first element.
   */

  def toLiteralList = List(head.asLiteral) ++ (for (x <- body) yield x)

  /**
   * Returns this clause with type predicates in the body, for each variable that appears in the
   * clause. The optional input parameter is for adding extra type
   * predicates that cannot be be inferred from the head of the rule.
   * Examples of the latter are transformation rules where the heads consist
   * of auxiliary, fresh predicates, not included in the target
   * language and thus not described by the mode declarations.
   *
   */

  def withTypePreds(extraTypePreds: List[String] = List()): Clause = {
    val types = (for (x <- this.toLiteralList)
      yield x.getTypePredicates).filter { z => z != Nil }.
      flatten.++(extraTypePreds).distinct.
      map { y => Literal.toLiteral(y) }
    Clause(head = this.head, body = this.body ::: types)
  }






}

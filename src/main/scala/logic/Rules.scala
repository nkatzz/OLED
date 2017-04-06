package logic

import app.Globals
import com.typesafe.scalalogging.LazyLogging
import logic.Examples.Example
import utils.ASP
import utils.Utils
import jep.Jep

import scala.collection.mutable.ListBuffer
import scala.util.control.Breaks


/**
 *
 * Utilities for rule handling.
 *
 */


object Rules extends LazyLogging{


  /**
   *
   * @param answerSet a set of use2 or use3 atoms
   * @param useAtomsMap a map o the use atoms to actual literals
   * @return a theory formed by the use2/use3 atoms and the literals
   */

  def getNewRules(answerSet: List[String],
                  useAtomsMap: Map[String, Literal], fromWeakExample: Boolean = false): Theory = {

    // Group atoms of the same rule.
    // Doing this with mat/flatMap etc does not work.
    // For comprehensions work as below:

    val group = for (Use2HeadAtom(x) <- answerSet;
           val z = for (Use2BodyAtom(y) <- answerSet if y._2._1 == x._2) yield y._1)
      yield (x._1, z)

    val rules = group match {
      case Nil => List[Clause]()
      case _ => group map (x =>
        Clause(head = useAtomsMap(x._1).asPosLiteral,
          body = x._2 map (x => useAtomsMap(x)),
          fromWeakExample = fromWeakExample))
    }
    Theory(rules)
  }

  def getInconsistentRules(answerSet: List[String],
                           priorTheory: Theory,
                           use3AtomsMap: Map[String, Literal]
                          ): List[InconstistentRule] = {

    // Group literal from same support set clause. example:
    // Answer set: List("use2(1,0)","use2(1,4)","use2(2,4)","use3(1,2,3)",
    //             "use3(1,2,5)","use3(2,3,5)","use3(2,3,7)","use3(6,3,8)")
    // group = Map((2,3) -> List(use3(2,3,5), use3(2,3,7)),
    //             (6,3) -> List(use3(6,3,8)), (1,2) -> List(use3(1,2,3), use3(1,2,5)))
    val group = answerSet.groupBy{
      x => x match {
        case Use3Atom(y) => (y._2._1,y._2._2)
        case _ => None
      }
    }.filter{case (k,v) => k!=None}.
      asInstanceOf[Map[(Int,Int), List[String]]]

    val inconstistentRules = (group map { case (k, v) => InconstistentRule(priorTheory, use3AtomsMap, (k, v)) }).toList
    inconstistentRules

  }




  /**
    * Represents an inconsistent rule. All the work is done in the companion object.
    *
    * @param rule The actual rule that needs to be specialized.
    * @param priorTheory The theory in which rule belongs.
    * @param use3Map A mapping o use3 atoms to literals from a
    *                rule from this rule's support set.
    * @param index a tuple (i,atoms) where i is the position of
    *              the inconsistent rule
    * @param initialRefinement An initial refinement of this rule.
    *                          If it subsumes rule's support set,
    *                          then this refinement will replace rule in
    *                          prior theory, else we'll search some more
    *                          for an appropriate refinement.
    * @param finalRefinement The final refinement (one subsuming the whole
    *                        support set) for this rule.
    */

  class InconstistentRule private (val rule: Clause = Clause(), val priorTheory: Theory = Theory(),
                          val use3Map: Map[String, Literal] = Map(), val index: (Int,List[String]) = (0,List()),
                          val initialRefinement: Clause = Clause(), val finalRefinement: Theory = Theory())


  object InconstistentRule {

    def apply(priorTheory: Theory,use3Map: Map[String, Literal],index: ((Int,Int),List[String])) = {
      require(!priorTheory.isEmpty)
      require(use3Map.nonEmpty)

      val rule = priorTheory.clauses(index._1._1 - 1)
      val SSRule = rule.supportSet.clauses(index._1._2 - 1)
      val initialRefinement =
        Clause(
          head = rule.head,
          body = rule.body ++ index._2.map(x => use3Map(x)),
          fromWeakExample = rule.fromWeakExample || SSRule.fromWeakExample
        )
      initialRefinement.addToSupport(rule.supportSet.clauses filter (x => initialRefinement.thetaSubsumes(x)))
      new InconstistentRule(rule = rule, initialRefinement = initialRefinement)
    }

    def apply(rule: Clause, finalRefinement: Theory = Theory()) = {
      //Initialize the support set for the final refinement
      val _finalRefinement = finalRefinement.isEmpty match {
        case true => Theory()
        case false =>
          finalRefinement.clauses.foreach(
            x => x.addToSupport(
              rule.supportSet.clauses filter ( y => x thetaSubsumes y ) )
          )
          finalRefinement
          //Theory(finalRefinement.clauses.map(x => Clause.updateField(x, fromWeakExample = rule.fromWeakExample)))
      }
      new InconstistentRule(rule = rule, finalRefinement = _finalRefinement)
    }

  }



  def subsetsSearch(incRule: InconstistentRule, accum: List[List[InconstistentRule]], examples: Example) = {
    logger.info("Minimal refinement search method: Full search")
    var done = false ; var k = 1 ; var finalRefinement = Theory()
    val loop = new Breaks
    loop.breakable {
      while(!done){
        for (x <- accum.flatten.toSet.subsets(k)){
          logger.info(s"Example ${examples.time}: Searching support subsets of size" +
            s" $k for support of size ${incRule.rule.supportSet.clauses.length}")
          val t = Theory((x map (y => y.initialRefinement)).toList) // the initialRefinement field carries one of the tried refs here
          finalRefinement = t
          if (t.thetaSubsumes(incRule.rule.supportSet)) {
            done = true
            loop.break
          }
        }
        k = k+1
      }
    }
    InconstistentRule(incRule.rule, finalRefinement)
  }

  def setCoverSearch(incRule: InconstistentRule, accum: List[List[InconstistentRule]], examples: Example) = {
    logger.info("Minimal refinement search method: Set cover search")
    //Find the support rules that are subsumed by each candiadate refinement
    val eachSubsumes = (c: InconstistentRule) => (c,incRule.rule.supportSet.clauses.filter(x => c.initialRefinement.thetaSubsumes(x)))
    // startWith contains a list of 2-tuples (x,y) where x is a specialization and y is the list
    // of support rules that are subsumed by x, sorted by the size of y
    val startWith = accum.flatten.map(x => eachSubsumes(x)).sortBy(y => y._2.length)
    //for each support rule c, select a specialization find a (x,y) from startWith such that y contains c. Add
    // x to a growing set of specializations that will result in the final refinement.
    var refinement = List[Clause]()
    for (c <- incRule.rule.supportSet.clauses) {
      val loop = new Breaks
      loop.breakable {
        for (s <- startWith) {
          if (s._2.contains(c)){
            refinement = (refinement :+ s._1.initialRefinement).distinct
            loop.break
          }
        }
      }
    }
    // do a final check to verify that the whole support is subsumed
    if (!Theory(refinement).thetaSubsumes(incRule.rule.supportSet)) {
      logger.error("Search for minimal refinement (set cover search) returned a" +
        " program that does not subsume the whole support set")
      System.exit(-1)
    }
    InconstistentRule(incRule.rule, Theory(refinement))
  }



  def getRefinedProgram(incRule: InconstistentRule,
                        retainedRules: Theory,
                        newRules: Theory, examples: Example, jep: Jep, globals: Globals): InconstistentRule = {
    // Search for a refined program that subsumes the support set
    def search: InconstistentRule = {
      val accum = new ListBuffer[List[InconstistentRule]]
      for (ssc <- incRule.rule.supportSet.clauses){
        val file = Utils.getTempFile("search",".lp")
        val (_,_,_,_,defeasibleRule,use3Map) =
          ASP.inductionASPProgram( retained = retainedRules.extendUnique(newRules),
            findAllRefs = (incRule.rule,ssc), examples = examples.toMapASP, aspInputFile = file, globals = globals)
        val answerSets = ASP.solve(Globals.FIND_ALL_REFMS, use3Map, file, examples.toMapASP, jep=jep)
        if (answerSets.head != AnswerSet.UNSAT){
          val inc = (answerSets map (x => getInconsistentRules(x.atoms, Theory(incRule.rule), use3Map))).flatten
          accum += inc
        } else {
          ssc.fromWeakExample match {
            case true => incRule.rule.removeFromSupport(ssc)
            case _ =>
              logger.error(s"\n ...While searching minimal refinements:" +
                s" Rule \n ${incRule.rule.tostring} cannot be refined using support rule \n ${ssc.tostring} \n")
              System.exit(-1)
          }
        }
      }
      Globals.glvalues("refinementSearch") match {
        case "setCover" => setCoverSearch(incRule,accum.toList,examples)
        case "fullSearch" => subsetsSearch(incRule,accum.toList,examples)
      }

    }

    // Use this for the new search for minimal refinement (the one that gives the whole support set)
    // Here prior theory is an inconsistent rule as a single-clause theory.
    def tryToRefine(priorTheory: Theory, retained: Theory, examples: Example, jep: Jep, globals: Globals): List[AnswerSet] = {
      val file = Utils.getTempFile("search",".lp")
      val (_,_,defeasiblePrior, use3Map,_,_) =
        ASP.inductionASPProgram(priorTheory = priorTheory, retained = retained,
          examples = examples.toMapASP, aspInputFile = file, globals=globals)
      val answerSets = ASP.solve(Globals.FIND_ALL_REFMS, use3Map, file, examples.toMapASP, jep=jep)
      val inc = (answerSets map (x => getInconsistentRules(x.atoms, priorTheory, use3Map))).flatten
      answerSets
    }

    val refinement: InconstistentRule =
      // Avoid redundant search if the initial refinement is ok
      //if (incRule.initialRefinement.thetaSubsumes(incRule.rule.supportSet)){
      if (retainedRules.extend(newRules).extend(Theory(incRule.initialRefinement)).thetaSubsumes(incRule.rule.supportSet)){
        // finalRefinement stays unpopulated, so the initial refinement is be used
        incRule
      } else {
        logger.info(s"Example ${examples.time}: Searching for minimal refinement")
        //search

        val answerSets = tryToRefine(priorTheory = Theory(incRule.rule), retained = retainedRules.extendUnique(newRules), examples = examples, jep=jep, globals=globals)
        //InconstistentRule(incRule.rule, Theory(refinement))
        incRule // Just in order for the code to compile for debugging.
      }

    refinement
  }



  // Use this for the new search for minimal refinement (the one that gives the whole support set)
  // Here prior theory is an inconsistent rule as a single-clause theory.
  def refineRule(incRule: InconstistentRule, retained: Theory,
                 e: Example, withSupport:String = "fullSupport", jep: Jep, globals: Globals): Either[String,List[InconstistentRule]] = {
    val file = Utils.getTempFile("search",".lp")
    val (_,_,_,use3Map,_,_) = ASP.inductionASPProgram(
      priorTheory = Theory(incRule.rule), retained = retained,
      examples = e.toMapASP, aspInputFile = file, use3WithWholeSupport = true, withSupport = withSupport, globals=globals)
    val answerSets = ASP.solve(Globals.FIND_ALL_REFMS, use3Map, file, e.toMapASP, jep=jep)
    val refined =
    answerSets match {
      case Nil =>
        Left("The support set is not subsumed as a whole, but I cannot get a refinement from the solver. Does this ever happen? What do we do in this case???")
      case _ =>
        answerSets.head match {
          case x: AnswerSet if x == AnswerSet.UNSAT =>
            incRule.rule.fromWeakExample match { // then the rule must be discarded
              case true =>
                Right(List[InconstistentRule]())
              case _ =>
                Left(s"\n Searching for minimal refinements at example ${e.time}:" + s" Rule \n ${incRule.rule.tostring} cannot be refined!")
            }
          case _ =>
            val refs = (answerSets map (x => getInconsistentRules(x.atoms, Theory(incRule.rule), use3Map))).flatten
            Right(refs)
        }
    }
    refined
  }


  def getRefined(incs: List[InconstistentRule], retainedRules: Theory,
                 newRules: Theory, e: Example, withSupport:String = "fullSupport", jep: Jep, globals: Globals) = {
    val retained = retainedRules.extendUnique(newRules)
    var refined = List[Theory]()

    def searchMore(p: InconstistentRule, extra: Theory) = withSupport match {
      case "fullSupport" => !retained.extendUnique(extra).thetaSubsumes(p.rule.supportSet)
      case "strongOnly" =>  !retained.extendUnique(extra).thetaSubsumes(p.rule.supportSet.strongRules)
    }
    //retained.extend(Theory(p.initialRefinement))
    for (incRule <- incs) {
      if (searchMore(incRule,Theory(incRule.initialRefinement))){
        val r = refineRule(incRule,retained,e,withSupport=withSupport, jep=jep, globals=globals)
        val z = r match {
          case Left(x) =>
            logger.error(x)
            if (Globals.glvalues("perfect-fit") == "true") {
              throw new RuntimeException(x)
            } else {
              List[InconstistentRule]()
            }
          case Right(x) => x
        }
        val ref = z.asInstanceOf[List[InconstistentRule]] map (x => x.initialRefinement)
        if (searchMore(incRule,Theory(ref))) {
          logger.error("we're fucked")
          if (Globals.glvalues("perfect-fit") == "true") {
            throw new RuntimeException("we're fucked")
          }
        }
        refined = refined :+ Theory(ref).compress
      }else{
        refined = refined :+ Theory(incRule.initialRefinement)
      }
    }
    refined
  }


  object Use2HeadAtom {
    val use2headPattern = "use2\\(([1-9]+)\\,0\\)".r

    def unapply(atom: String): Option[(String, Int)] = atom match {
      case use2headPattern(i) => Some(atom, i.toInt)
      case _ => None
    }
  }



  object Use2BodyAtom {
    //val use2bodyPattern = "use2\\(([0-9]+)\\,([1-9]+)\\)".r
    val use2bodyPattern = "use2\\(([0-9]+)\\,([1-9]\\d*)\\)".r
//^[1-9]\d*$
    def unapply(atom: String): Option[(String, (Int, Int))] = atom match {
      case use2bodyPattern(i, j) => Some(atom, (i.toInt, j.toInt))
      case _ => None
    }
  }

  object Use3Atom {
    val use3Pattern = "use3\\(([0-9]+)\\,([0-9]+)\\,([0-9]+)\\)".r

    def unapply(atom: String): Option[(String, (Int, Int, Int))] = atom match {
      case use3Pattern(i, j, k) => Some(atom, (i.toInt, j.toInt, k.toInt))
      case _ => None
    }
  }


}
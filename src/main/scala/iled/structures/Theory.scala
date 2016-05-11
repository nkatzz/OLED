package iled.structures

import java.io.File

import com.typesafe.scalalogging.LazyLogging
import iled.core.{Core, Iled}
import iled.globalValues.GlobalValues
import iled.structures.Examples.Example
import iled.utils.{Utils, Exmpl, ASP}
import jep.Jep

import scala.collection.mutable.ListBuffer

object Theory  {

  val empty = Theory()

  def apply(c: Clause) = new Theory(List(c))

  def mergeTheories(T: List[Theory]) = Theory(T flatMap (x => x.clauses))

}

case class Theory(clauses: List[Clause] = List()) extends Expression with LazyLogging {

  val tps = new ListBuffer[String]
  val fps = new ListBuffer[String]
  var fns = new ListBuffer[String]

  def stats = {
    val tps = this.tps.distinct.length.toFloat
    val fps = this.fps.distinct.length.toFloat
    val fns = this.fns.distinct.length.toFloat
    val precision = tps/(tps+fps)
    val recall = tps/(tps+fns)
    val fscore = 2*precision*recall/(precision+recall)
    (tps.toInt,fps.toInt,fns.toInt,precision,recall,fscore)
  }


  def showWithStats = (this.clauses map (_.showWithStats)).mkString("\n")


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

   def withTypePreds = clauses.map(_.withTypePreds())


  /**
    * Returns "initiated" or "terminated". This is used by the streaming version
    */

  def getTargetClass = {
    val what = this.clauses.map(x => x.head.functor).toSet
    if(what.size > 1) throw new RuntimeException("I'm learning both initiated and terminated rules in the same process!")
    if(what.size > 0) what.head else "empty"
  }


  /**
    *
    * @return The marked rules and the marked rule preds (e.g. rule(234234)) as a single string ready for ASP use.
    *         Also a map of ruleId --> rule
    */
  def marked: (String, Map[String, Clause]) = {
    val allRefinements = this.clauses flatMap(_.refinements)
    val allRules = this.clauses ++ allRefinements
    val markedTheory = this.clauses map (_.marked)
    val markedRefinements = allRefinements map (_.marked)
    val allRulesMarked = markedTheory ++ markedRefinements
    val hashCodesClausesMap = (allRules map (x => x.##.toString -> x)).toMap
    val rulePredicates = hashCodesClausesMap.keySet.map(x => s"rule($x). ").mkString("\n")
    (allRulesMarked.map(_.tostring).mkString("\n")+rulePredicates, hashCodesClausesMap)
  }


  def scoreRules(example: Example, jep: Jep, globals: GlobalValues, postPruningMode: Boolean = false): Unit = {

    val targetClass = getTargetClass
    // If a rule has just been expanded its refinements are empty, so generate new
    if (!postPruningMode) {
      this.clauses foreach (rule => if (rule.refinements.isEmpty) rule.generateCandidateRefs)
    }
    // Proceed to scoring
    val e = (example.annotationASP ++ example.narrativeASP).mkString("\n")
    val _marked = marked
    val markedProgram = _marked._1
    val markedMap = _marked._2
    val countRules = globals.TIMES_COUNT_RULE


    val show = globals.SHOW_TPS_ARITY_2 + globals.SHOW_FPS_ARITY_2 + globals.SHOW_FNS_ARITY_2 + globals.SHOW_TIME + globals.SHOW_INTERPRETATIONS_COUNT
    val include = {
      targetClass match {
        case "initiatedAt" => globals.INCLUDE_BK(globals.BK_INITIATED_ONLY_MARKDED)
        case "terminatedAt" => globals.INCLUDE_BK(globals.BK_TERMINATED_ONLY_MARKDED)
      }
    }
    val all = e + include + countRules + markedProgram + show
    val f = Utils.getTempFile("isConsistent",".lp",deleteOnExit = true)
    Utils.writeToFile(f, "append")(p => List(all) foreach p.println)
    val path = f.getCanonicalPath
    val answerSet = ASP.solve(task = Core.INFERENCE, aspInputFile = new File(path), jep=jep)

    answerSet match {
      case Nil =>
        throw new RuntimeException("Got an empty answer set during rule evaluation (at least times count should be returned)")
      case _ =>
        val (timeCounts, interpretationsCounts, coverageCounts) = answerSet.head.atoms.foldLeft(List[String](), List[String](), List[String]()){ (x,y) =>
          val timesCount = x._1
          val interpretationsCount = x._2
          val coverageCounts = x._3
          if(y.startsWith("tps") || y.startsWith("fps") || y.startsWith("fns")) {
            (timesCount,interpretationsCount,coverageCounts :+ y)
          } else if (y.startsWith("times")) {
            (timesCount :+ y, interpretationsCount, coverageCounts)
          } else if (y.startsWith("examplesCount")) {
            (timesCount, interpretationsCount :+ y, coverageCounts)
          } else {
            throw new RuntimeException(s"Don't know what to do with what the solver" +
              s" returned.\nExpected tps/2,fps/2,fns/2,times/1 counts, examplesCount/1 got\n${answerSet.head.atoms}")
          }
        }

        if (timeCounts.isEmpty) throw new RuntimeException("No times count returned")
        if (timeCounts.size > 1) throw new RuntimeException(s"Don't know what to do with these times counts:\n$timeCounts")
        val times = Literal.toLiteral(timeCounts.head).terms.head.tostring.toInt


        //if (interpretationsCounts.isEmpty) throw new RuntimeException("No interpretations count returned")
        //if (interpretationsCounts.size > 1) throw new RuntimeException(s"Don't know what to do with these interpretations counts:\n$timeCounts")
        //val interps = Literal.toLiteral(interpretationsCounts.head).terms.head.tostring.toInt

        /*=====================================
         We need to get the count of different
         interpretations. This is application-
         specific. For instance in CAVIAR it is
         equal to the the 2-combinations of
         time points by persons.

         Need to find a generic way to handle
         this, although in the general case we
         may assume that the interpretations
         are passed in correctly (e.g. each
         interpretation has only two persons
         in the CAVIAR case) and we can simply
         count them. For now I'll use the patch
         below to get the persons and calculate
         the combinations.
         =====================================*/

        // increase the count for seen examples

        this.clauses foreach { x =>
          x.seenExmplsNum +=  times // interps
          x.refinements.foreach(y => y.seenExmplsNum += times)
          x.supportSet.clauses.foreach(y => y.seenExmplsNum += times)
        }

        val parse = (atom: String) => {
          val tolit = Literal.toLiteral(atom)
          val (what, hashCode, count) = (tolit.functor, tolit.terms.head.tostring, tolit.terms.tail.head.tostring)
          (what, hashCode, count)
        }
        val updateCounts = (what: String, hashCode: String, count: String) => {
          val clause = markedMap(hashCode)

          if(what == "fns" && count.toInt > 0) {
            val stop = "stop"
          }

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

  def growNewRuleTest(e: Exmpl, jep: Jep, target: String, globals: GlobalValues): Boolean = {



    // we already have it the target with the input (the target parameter).
    // But the one from the input is used only in case of an empty theory. In
    // other cases with get the target class by looking at the rules' heads,
    // just for some extra safety on the fact that we are indeed learning separately
    // (check out the exceptions thrown below in case we end but with a mixture
    // of initiatedAt and terminated rules in the theory._
    val targetClass = getTargetClass

    /*
    val parse = (atom: String) => {
      println(atom)
      val tolit = Literal.toLiteral(atom)
      val count = tolit.terms.tail.head.tostring
      count
    }
    */

    def solve(program: String, jep: Jep): List[AnswerSet] = {
      val f = Utils.getTempFile(s"growNewRuleTest-for-$target",".lp",deleteOnExit = true)
      Utils.writeToFile(f, "append")(p => List(program) foreach p.println)
      val path = f.getCanonicalPath
      ASP.solve(task = Core.INFERENCE, aspInputFile = new File(path), jep=jep)
    }

    val (includeBKfile,failedTestDirective,show) = {
      targetClass match {
        // If we are learning the initiatedAt part of the the theory, then we must start growing
        // a new rule if we have FNs, i.e. no initiatedAt rule in the current hypothesis fires,
        // and fluents are not initiated when they should.
        case "initiatedAt" => (globals.INCLUDE_BK(globals.BK_INITIATED_ONLY), globals.FNS_RULES,globals.SHOW_FNS_ARITY_1)
        // If we are learning the terminatedAt part of the the theory, then we must start growing
        // a new rule if we have FPs, i.e. no terminatedAt rule in the current hypothesis fires,
        // and fluents are not terminated when they should.
        case "terminatedAt" => (globals.INCLUDE_BK(globals.BK_TERMINATED_ONLY), globals.FPS_RULES,globals.SHOW_FPS_ARITY_1)
        // In this case no theory has been generated yet. We therefore check if the current example
        // satisfies the empty theory with the plain isSAT method. To do that, we use the whole set
        // of EC axioms in the BK. Also, coverage directives (normally fps, tns etc) are coverage
        // constraints here, forcing the SOLVER to try to satisfy them and getting back an UNSAT program in case of failure.
        // Note that we use no #show here.
        case "empty" =>
          // the target is taken from the method's input here.
          (if (target=="initiatedAt") globals.INCLUDE_BK(globals.BK_INITIATED_ONLY)
          else globals.INCLUDE_BK(globals.BK_TERMINATED_ONLY),
            if (target=="initiatedAt") globals.CONSTRAINT_COVER_ALL_POSITIVES
            else globals.CONSTRAINT_EXCLUDE_ALL_NEGATIVES,"") // no #show
      }
    }

    val t = (this.withTypePreds map (_.tostring)).mkString("\n")
    // Getting exmplWithInertia here does not cause problems (in the initiated case). See comments at CaviarUtils.getDataAsChunks
    val ex = (e.exmplWithInertia.annotationASP ++ e.exmplWithInertia.narrativeASP).mkString("\n")

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

    val answerSet = solve(program,jep)

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







  def use_2_split: (Theory, Map[String, Literal]) = {
     /*
     val t = (for ((c, i) <- this.clauses zip List.range(1, this.clauses.length + 1))
               yield c.use_2_split(i)).map { x => List(x._1, x._2) }.transpose
      val defeasibles = Theory((for (x <- t(0)) yield x.asInstanceOf[Theory].clauses).flatten)
      val use = t(1).asInstanceOf[List[Map[String, Literal]]].reduce((x, y) => x ++ y)
      (defeasibles, use)
      */

     this match {
       case Theory.empty => (Theory(),Map[String,Literal]())
       case _ =>
         val t = (for ((c, i) <- this.clauses zip List.range(1, this.clauses.length + 1))
           yield c.use_2_split(i)).map { x => List(x._1, x._2) }.transpose
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
      p => Clause(
        head=p.head,
        body=p.body,
        fromWeakExample=p.fromWeakExample,
        supportSet=f(p.supportSet))
    }
    if (Core.glvalues("withWeaks").toBoolean) Theory(x) else this // don't complicate things in strong-only learning
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

   def use_3_spilt_one(withSupport:String = "fullSupport") = {
      if (this != Theory()) {
         val z = this.clauses zip List.range(1, this.clauses.length + 1) map
           (x => x._1.use_3_split_one(x._2,withSupport=withSupport)) map (x => List(x._1, x._2, x._3))
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

  def use_3_split_all(withSupport:String = "fullSupport") = {
    if (this != Theory()) {
      val z = this.clauses zip List.range(1, this.clauses.length + 1) map
        (x => x._1.use_3_split(x._2,withSupport = withSupport)) map (x => List(x._1, x._2, x._3))
      val t = z.transpose
      val defeasibles = Theory((for (x <- t(0)) yield x.asInstanceOf[Theory].clauses).flatten)
      val use3map = t(1).asInstanceOf[List[Map[String, Literal]]].reduce((x, y) => x ++ y)
      val use3generates = t(2).asInstanceOf[List[String]]
      (defeasibles, use3map, use3generates)
    } else {
      (Theory(), Map[String, Literal](), List[String]())
    }
  }

   def use_3_split: (Theory, Map[String, Literal]) = {
      
      val z = this.clauses zip List.range(1, this.clauses.length + 1) map (x => x._1.use_3_split(x._2)) map (x => List(x._1,x._2))
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
      (p,q) =>
        if (!q.fromWeakExample)
          if (p.exists(x => !x.fromWeakExample && x.thetaSubsumes(q) && q.thetaSubsumes(x))) p
          else p :+ q
        else
          if (p.exists(x => x.thetaSubsumes(q) && q.thetaSubsumes(x))) p
          else p :+ q
    }
    Theory(t)
  }

   def containsRule(c: Clause) = {
     this.clauses.exists(x => x.thetaSubsumes(c) && c.thetaSubsumes(x))
   }

  def updateSupports(kernel: Theory, fromWeakExample: Boolean = false) = {
    Iled.updateSupport(this, kernel, fromWeakExample = fromWeakExample)
  }

  def clearSupports(e: Example, jep: Jep) = { // it also removes inconsistent weak rules

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
      val path = ASP.isConsistent_program_Marked(Theory(consistents), e)
      val answerSet = ASP.solve(task = Core.INFERENCE, aspInputFile = new File(path), jep=jep)
      if(answerSet != Nil) {
        val f = (a: String) => {
          val tolit = Literal.toLiteral(a)
          val (i, j) = (tolit.terms.head.tostring.toInt, tolit.terms.tail.head.tostring.toInt)
          (i,j)
        }
        val grouped = answerSet.head.atoms.map(x => f(x)).groupBy{_._1}.map{case (k,v) => k -> v.map(y => y._2).distinct}
        for ( x <- grouped.keySet ){
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
class PriorTheory(val retainedRules: Theory = Theory(),
                  val newRules: Theory = Theory(),
                  val refinedRules: Theory = Theory()) extends Theory {

  def merge = Theory(
    this.retainedRules.clauses ++
      this.newRules.clauses ++
      this.refinedRules.clauses
  )

  override def updateSupports(kernel: Theory, fromWeakExample: Boolean) = {
    Iled.updateSupport(this.newRules,kernel,fromWeakExample)
    Iled.updateSupport(this.retainedRules,kernel,fromWeakExample)
    Iled.updateSupport(this.refinedRules,kernel,fromWeakExample)
  }

  override def clearSupports(e: Example, jep: Jep) = {
    val news = this.newRules.clearSupports(e,jep)
    val ret = this.retainedRules.clearSupports(e,jep)
    val ref = this.refinedRules.clearSupports(e,jep)
    new PriorTheory(retainedRules=ret,newRules=news,refinedRules=ref)
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

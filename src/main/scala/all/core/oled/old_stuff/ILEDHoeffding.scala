package all.core.oled.old_stuff

import java.io.File

import com.mongodb.casbah.Imports._
import com.mongodb.casbah.commons.MongoDBObject
import all.core.Iled._
import all.core.Implicits._
import all.core.{Core, IledStreaming}
import all.structures.Examples.Example
import all.structures.{AnswerSet, Clause, Literal, Theory}
import all.utils.{ASP, Database, Exmpl, Utils}
import jep.Jep

import scala.math._


/**
  * Created by nkatz on 20/2/2016.
  */

object ILEDHoeffding {

  val maxBottomTheorySize = 15

  val startWithEmpty = false
  def main(args: Array[String]): Unit = {
    val db = new Database(Core.fromDB, "examples")
    val jep = new Jep()
    // best threshold so far: δ = 0.0005, tiesThreshold = 0.05
    run(db, withWeaks=true, jep=jep, delta=0.0005,breakTiesThreshold=0.05)
    //createPairsDB
  }

  def run(DB: Database, withWeaks: Boolean, jep: Jep, delta: Double,breakTiesThreshold: Double): Unit = {
    //val isRedundant = (bottomClause: Clause, bottomTheory: List[Clause]) =>
    //  bottomTheory exists (x => bottomClause.thetaSubsumes(x))

    val dataIterator = DB.collection.find().sort(MongoDBObject("exampleId" -> 1))
    val finalTheory = dataIterator.foldLeft(Theory()){ (topTheory, newExample) =>
      val e = new Exmpl(newExample)
      println(e.id)
      val t = processExample(topTheory, e, jep, delta, breakTiesThreshold)

      val tt = clearSupports(t)
      tt
    }
    finalTheory.clauses.foreach{ x =>
      x.tps = x.tps + x.supportSet.clauses.map(_.tps).sum
      x.fps = x.fps + x.supportSet.clauses.map(_.fps).sum
      println(s"tps: ${x.tps}, fps: ${x.fps}, score: ${x.score}, evaluated on ${x.seenExmplsNum} examples")
      println(x.tostring, x.tps, x.fps, x.score)
    }
    crossVal(finalTheory,DB, jep)
  }

  def crossVal(t: Theory, DB: Database, jep: Jep) = {
    val dataIterator = DB.collection.find().sort(MongoDBObject("exampleId" -> 1))
    while(dataIterator.hasNext) {
      val e = dataIterator.next()
      val ee = new Exmpl(e)
      println(ee.id)
      evaluateTheory(t,ee,withInertia = true,jep)
    }
    //println(s"tps: ${t.tps}, fps: ${t.fps}, fns: ${t.fns}\nprecision: ${t.precision}\nrecall: ${t.recall}\nfscore: ${t.fscore}")
    //println(s"tps: ${t.tpsString.distinct}, fps: ${t.fpsString.distinct}, fns: ${t.fnsString.distinct}\nprecision: ${t.precision}\nrecall: ${t.recall}\nfscore: ${t.fscore}")
  }

  def clearSupports(topTheory: Theory): Theory = {
    /*
    val newTheory = new ListBuffer[Clause]
    for (rule <- topTheory.clauses) {
      val (keep,remove) = rule.supportSet.clauses.foldLeft(List[Clause](),List[Clause]()){ (x,supportRule) =>
        val removeTest = (supportRule.score < 0.6 || supportRule.tps.toFloat / supportRule.seenExmplsNum <= 0.01) &&
          !supportRule.head.tostring.contains("terminatedAt")
        if (removeTest) (x._1, x._2 :+ supportRule)
        else (x._1 :+ supportRule, x._2)
      }
      remove.foreach(z => rule.removeFromSupport(z))
      if (rule.supportSet.clauses.nonEmpty) newTheory :+ rule
    }
    */


    for (x <- topTheory.clauses) {
      for (y <- x.supportSet.clauses) {
        //println(y.tps,y.fps,y.score)
        //(y.score < 0.6 || y.tps.toFloat / y.seenExmplsNum <= 0.001) && !y.head.tostring.contains("terminatedAt")
        if ( y.score < 0.6 && y.seenExmplsNum > 1000 && !y.head.tostring.contains("terminatedAt")){
          x.removeFromSupport(y)
          println(s"removed support rule. Score: ${y.score} tps: ${y.tps} fps: ${y.fps} evaluated on ${y.seenExmplsNum} examples")
          println(y.tostring)
        }
      }
    }
    topTheory.clauses.filter(x => x.supportSet.clauses.nonEmpty)
  }

  def generateCandidateRefs(c: Clause): Unit = {
    val candidateList = c.supportSet.clauses.flatMap(_.body).distinct.filter(!c.body.contains(_))
    val candidateRefinements = for (lit <- candidateList) yield Clause(c.head, c.body :+ lit)
    c.refinements = candidateRefinements
  }

  /*

  Generates 2-step refinements, I wrote it for debugging

  def generateCandidateRefs1(c: Clause): Unit = {
    val candidateList = c.supportSet.clauses.flatMap(_.body).distinct.filter(!c.body.contains(_))
    val kReks_size_1 = candidateList
    val kReks_size_2 = candidateList.combinations(2)
    val candidateRefinements = for (lit <- kReks_size_1 ) yield Clause(c.head, c.body :+ lit)
    val candidateRefinements_2 = for (litList <- kReks_size_2 ) yield Clause(c.head, c.body ++ litList)
    c.refinements = candidateRefinements ++ candidateRefinements_2
  }
*/



  def processExample(topTheory: Theory, e: Exmpl, jep: Jep, delta: Double, breakTiesThreshold: Double): Theory = {
    //val isRedundant=(bottomClause:Clause,bottomTheory:List[Clause])=>bottomTheory exists (x => bottomClause.thetaSubsumes(x))
    var newTopTheory = topTheory
    val bottomTheory = topTheory.clauses flatMap (_.supportSet.clauses)

    // If the bottom theory is not satisfied by the collection of bottom clauses generated so far,
    // the current example has to be compressed into a new bottom clause. Note that here we should
    // always use the no-inertia version of the example for reasoning
    if (!isSAT(bottomTheory, e.exmplNoInertia, ASP.check_SAT_Program, jep = jep)) {
      val (_, varKernel) = Utils.generateKernel(e.exmplNoInertia.toMapASP, jep = jep)
      if(bottomTheory.length < maxBottomTheorySize) { // try to prevent the bottom theory from growing too much
        updateSupport(topTheory, varKernel)
        // each bottom rule r that has NOT been added to a support set,
        // should initiate a new rule added to the top hypothesis:
        val leftOvers = varKernel.filter(x => !topTheory.clauses.flatMap (_.supportSet.clauses).contains(x))
        val newRules = leftOvers map { x =>
          val c = Clause(head=x.head, body = List())
          c.addToSupport(x)
          c
        }
        newTopTheory = topTheory.clauses ++ newRules
      }
    }
    scoreRules(newTopTheory, e, jep)
    expandRules(newTopTheory, delta, breakTiesThreshold)
  }

  def expandRules(topTheory: Theory, delta: Double, breakTiesThreshold: Double): Theory = {

    def isRedundant(r: Clause, t: Theory) = {t.clauses.exists(p => p.thetaSubsumes(r) && r.thetaSubsumes(p))}
    val out = topTheory.clauses flatMap { parentRule =>
      // - sign is to order with decreasing order (default is with increasing)
      val best2 = parentRule.refinements.sortBy { x => -x.score }.take(2)
      val epsilon = sqrt(scala.math.log(1.0 / delta) / (2 * parentRule.seenExmplsNum))
      val observedDiff = best2.head.score - best2(1).score
      val passesTest = if (epsilon < observedDiff) true else false

      //println("epsilon:", epsilon, "best ref:", best2.head.score, "second best ref:", best2(1).score, observedDiff)

      val tie = if (epsilon <= breakTiesThreshold) true else false
      val couldExpand = passesTest || tie
      couldExpand match {
        case true =>
          best2.head.score > parentRule.score match {
            case true =>
              val refinedRule = best2.head
              if(! isRedundant(refinedRule, topTheory)) { //expand only if the refinement does not exist it the top theory
                IledStreaming.showInfo(parentRule, best2.head, best2(1), epsilon, observedDiff, parentRule.seenExmplsNum)
                refinedRule.seenExmplsNum = 0 // zero the counter
                val (subsumedSSRules, notSubsumedSSRules) =
                  parentRule.supportSet.clauses.foldLeft(List[Clause](), List[Clause]()) { (a, supportRule) =>
                    val subsumed = a._1
                    val notSubsumed = a._2
                    if (refinedRule.thetaSubsumes(supportRule)) (subsumed :+ supportRule, notSubsumed)
                    else (subsumed, notSubsumed :+ supportRule)
                  }
                refinedRule.supportSet = subsumedSSRules
                // If there are are support set rules that are not subsumed by the best refinement,
                // then if these rules are good enough (have to decide what will this mean), keep
                // growing the parent rule with this support set rules
                val goodEnoughSupports = notSubsumedSSRules.filter(isSupportRuleGoodEnough)
                if (goodEnoughSupports.nonEmpty) {
                  val tempTheory = topTheory.clauses.filter(!_.equals(parentRule))
                  val isRedundant = tempTheory.clauses.exists(q => q.thetaSubsumes(parentRule) && parentRule.thetaSubsumes(q))
                  if (!isRedundant) {
                    parentRule.supportSet = goodEnoughSupports
                    List(parentRule, refinedRule)
                  } else {
                    List(refinedRule)
                  }
                } else {
                  List(refinedRule)
                }
              } else {
                List(parentRule) // here we delay the expansion of the rule until necessary
              }
            case _ => List(parentRule)
          }
        case _ => List(parentRule)
      }
    }
    Theory(out)
  }


  def isSupportRuleGoodEnough(rule: Clause) = true


  def scoreRules(topTheory: Theory, e: Exmpl, jep: Jep): Unit = {
    // First we need to take care of the candidate refinements. The refinements buffer is emptied
    // each time the Hoeffding test succeeds, so if this is the case, at the next step we
    // need to generate new candidate refinements:
    topTheory.clauses foreach (rule => if (rule.refinements.isEmpty) rule.generateCandidateRefs)

    // Seperate initiatedAt from terminatedAt rules, they have to be treated differently.
    // In particular, inertia in difused in case of initiatedAt rules, to allow for a positive
    // example to be explicitly covered. In contrast, inertia is preserved during training time
    // in case of terminatedAt rules, in order to learn the correct terminating conditions.
    val (initRules, termRules) =
      topTheory.clauses.foldLeft(List[Clause](),List[Clause]()){ (x,rule) =>
        val inits = x._1 ; val terms = x._2
        if (rule.head.tostring.contains("initiatedAt")) (inits :+ rule, terms) else (inits, terms :+ rule)
      }
    RuleScoring.scoreRules(initRules,e.exmplNoInertia,jep)
    RuleScoring.scoreRules(termRules,e.exmplWithInertia,jep)
    // increase the seen examples count for each rule in the top theory
    topTheory.clauses.foreach{
      x =>
        x.seenExmplsNum += 1
        x.supportSet.clauses.foreach(y => y.seenExmplsNum += 1)
    }
  }


  def evaluateTheory(theory: Theory, e: Exmpl, withInertia: Boolean = true, jep: Jep): Unit ={
    val varbedExmplPatterns = for (x <- Core.examplePatterns) yield x.varbed.tostring
    val coverageConstr =
      varbedExmplPatterns flatMap{
        x => List(s"\nfns($x) :- example($x), not $x.", s"\nfps($x):- $x, not example($x).", s"\ntps($x):- $x, example($x).\n")
      }
    val t = theory.clauses.map(x => x.withTypePreds().tostring).mkString("\n")
    val show = "\n#show tps/1.\n#show fps/1.\n#show fns/1.\n"

    val ex = if(withInertia) e.exmplWithInertia else e.exmplNoInertia
    val exx = (ex.annotationASP ++ ex.narrativeASP).mkString("\n")

    val all = exx + s"\n\n#include " + "\""+Core.bkFile+"\".\n" + t + coverageConstr.mkString("\n") + show

    val f = Utils.getTempFile("isConsistent",".lp",deleteOnExit = true)
    Utils.writeToFile(f, "append")(p => List(all) foreach p.println)
    val path = f.getCanonicalPath
    val answerSet = ASP.solve(task = Core.INFERENCE, aspInputFile = new File(path), jep=jep)
    println(answerSet)
    if (answerSet.nonEmpty) {
      val atoms = answerSet.head.atoms
      //theory.tps = theory.tps + atoms.count(p => p.startsWith("tps")) // I think this counts duplicates
      //theory.fps = theory.fps + atoms.count(p => p.startsWith("fps"))
      //theory.fns = theory.fns + atoms.count(p => p.startsWith("fns"))

      atoms.foreach { a=>
        val lit = Literal.toLiteral(a)
        val inner = lit.terms.head
        if (lit.functor == "tps"){
          //theory.tpsString :+ inner.tostring
        }
        if (lit.functor == "fps"){
          //theory.fpsString :+ inner.tostring
        }
        if (lit.functor == "fns"){
          //theory.fnsString :+ inner.tostring
        }
      }
    }
    //theory
  }

  def isSAT(theory: Theory, example: Example, F: (Theory, Example) => String, jep: Jep): Boolean = {
    val f = F(theory, example)
    val out = ASP.solve(Core.CHECKSAT, Map(), new java.io.File(f), example.toMapASP, jep = jep)
    if (out != Nil && out.head == AnswerSet.UNSAT) false else true
  }







}

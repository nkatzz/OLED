package iled.core.noisyILED.old_stuff

import java.io.File

import akka.actor.Actor
import com.mongodb.casbah.commons.MongoDBObject
import com.typesafe.scalalogging.LazyLogging
import iled.core.Core
import iled.core.Implicits._
import iled.globalValues.GlobalValues
import iled.structures.Examples.Example
import iled.structures.{AnswerSet, Clause, Literal, Theory}
import iled.utils.{ASP, Database, Exmpl, Utils}
import jep.Jep

import scala.math._

/**
  * Created by nkatz on 2/24/16.
  *
  * Learns initiated and terminated rules separately and concurently
  *
  */
object ILEDHoeffdingNoSupportTermInit extends GlobalValues with LazyLogging {

  val delta = 0.05
  val breakTiesThreshold = 0.05
  val postPruningThreshold = 0.5
  val minSeenExmpls = 0 // break ties for a rule only after it has been evaluated on minSeenExmpls
  val repeatFor = 1     // re-see the data repeatFor
  val chunkSize = 10    // learn from chunks instead of pairs (to speed things up)

  val DB = new Database(Core.fromDB, "examples")
  val initiated = "initiated"
  val terminated = "terminated"
  val withInertia = true
  val noInertia = false


  val initorterm = "init"
  val trainigsize = 10000000
  def getData = iled.utils.CaviarUtils.getDataAsChunks(DB, chunkSize, initorterm, withInertia).take(trainigsize)
  //def getData = DB.collection.find().sort(MongoDBObject("exampleId" -> 1))

  def main(args: Array[String]): Unit = {
    val jep = new Jep()
    // best threshold so far: δ = 0.0005, tiesThreshold = 0.05
    // repeatFor re-sees the whole dataset
    // minSeenExmpls is the minimum number of seen examples before tie breaking is allowed
    //run(db, withWeaks=true, jep=jep, delta=0.05,breakTiesThreshold=0.05, repeatFor = 1)

    //DevUtils.accumulateKernels(db,jep)

    runWithDataChunks(DB, jep=jep, initiated, noInertia)

  }


  class Learner(val DB: Database, val jep: Jep, targetClass: String, withInertia: Boolean) extends Actor {
    def receive = { case "go" => sender ! runWithDataChunks(DB,jep,targetClass, withInertia) }
  }



  def runWithDataChunks(DB: Database, jep: Jep, targetClass: String, withInertia: Boolean): Theory = {
    val trainingSetSize = 1000 // set 1000000 to take all
    def runOnce(inTheory: Theory): Theory = {
      //val dataChunks = scala.util.Random.shuffle(iled.utils.CaviarUtils.getDataAsChunks(DB, chunkSize)) // do not shuffle to see what happens
      val data = iled.utils.CaviarUtils.getDataAsChunks(DB, chunkSize, initorterm, withInertia).take(trainigsize)
      //val data = getData
      data.foldLeft(inTheory){ (topTheory, newExample) =>
        println(newExample.id)
        processExample(topTheory, newExample, jep, notCoversPositives, targetClass)
      }
    }
    val _finalTheory = Utils.time{ (1 to repeatFor).foldLeft(Theory())( (t,_) =>  runOnce(t)) }
    val (finalTheory,time) = (_finalTheory._1,_finalTheory._2)
    reScore(DB,finalTheory,chunkSize,jep,trainingSetSize,targetClass, withInertia)
    val pruned = finalTheory.clauses.filter(x => x.score > postPruningThreshold)
    pruned.foreach { p => p.clearStatistics }
    pruned
  }

  /*
     val (tps,fps,fns,precision,recall,fscore) = crossVal(pruned,DB, jep, trainingSetSize*chunkSize)
    logger.info(s"tps: $tps, fps: $fps, fns: $fns\nprecision: $precision\nrecall: $recall\nf-score: $fscore\ntraining time:" +
      s" $time\ntheory size: ${pruned.clauses.foldLeft(0)((x,y) => x + y.body.length + 1)}\n${pruned.tostring}")
   */

  /**
    *
    * Quick and dirty solution for post-pruning. Go through the training set again and score rules.
    * "what" is either initiated or terminated and it is used to specify which BK file to use
    *
    * @param DB
    * @param theory
    * @param chunkSize
    * @param jep
    * @param trainingSetSize
    * @param what
    *
    */
  def reScore(DB: Database, theory: Theory, chunkSize: Int, jep: Jep, trainingSetSize: Int, what: String, withInertia: Boolean) = {
    val dataChunks = getData
    theory.clauses foreach (p => p.clearStatistics)
    for (x <- dataChunks) {
      //println(x.id)
      scoreRules(theory, x.exmplWithInertia, jep, what)
    }
    logger.info( theory.clauses map { p => s"score: ${p.score}, tps: ${p.tps}, fps: ${p.fps}, fns: ${p.fns}\np.tostring" } mkString("\n") )
  }



  def processExample(topTheory: Theory, e: Exmpl, jep: Jep, failedTest: (Theory,Exmpl,Jep,String) => Boolean, targetClass: String): Theory = {
    var newTopTheory = topTheory
    if (failedTest(topTheory, e, jep, targetClass)) {
      val newRules = generateNewRules(topTheory, e, jep, targetClass)
      newTopTheory = topTheory.clauses ++ newRules
    }
    scoreRules(newTopTheory, e, jep, targetClass)
    expandRules(newTopTheory, delta, breakTiesThreshold, minSeenExmpls)
  }


  def generateNewRules(topTheory: Theory, e: Exmpl, jep: Jep, targetClass: String) = {
    val (_, varKernel) = Utils.generateKernel(e.exmplWithInertia.toMapASP, jep = jep)
    val bottomTheory = topTheory.clauses flatMap(x => x.supportSet.clauses)
    val goodKernelRules = varKernel.filter(newBottomRule => !bottomTheory.exists(supportRule => newBottomRule.thetaSubsumes(supportRule)))
    goodKernelRules map { x =>
      val c = Clause(head=x.head, body = List())
      logger.info(s"Started growing new rule: \n ${c.tostring} from bottom clause: \n ${x.tostring}")
      c.addToSupport(x)
      c
    }
  }

  def scoreRules(topTheory: Theory, e: Exmpl, jep: Jep, targetClass: String): Unit = {
    topTheory.clauses foreach (rule => if (rule.refinements.isEmpty) rule.generateCandidateRefs)
    // increase the seen examples count for each rule in the top theory
    // to do that we need to get the number of examples in the current chunk.
    // We call the solver once again to get counts for the time/1 predicate,
    // since we have one training example per time point
    val counts = getExmplsNumInChunk(e,jep)
    val (times,fps) = (counts._1,counts._2)
    scoreRules(topTheory.clauses, e.exmplWithInertia, jep, targetClass)
    topTheory.clauses.foreach{
      x =>
        x.seenExmplsNum += times
        x.refinements.foreach(y => y.seenExmplsNum += times)
        x.supportSet.clauses.foreach(y => y.seenExmplsNum += times)
    }
  }


  def expandRules(topTheory: Theory, delta: Double, breakTiesThreshold: Double, minSeenExmpls: Int): Theory = {
    val out = topTheory.clauses flatMap { parentRule =>
      // - sign is to order with decreasing order (default is with increasing)
      val best2 = parentRule.refinements.sortBy { x => - x.score }.take(2)
      val epsilon = sqrt(scala.math.log(1.0 / delta) / (2 * parentRule.seenExmplsNum))
      val observedDiff = best2.head.score - best2(1).score
      val passesTest = if (epsilon < observedDiff) true else false

      //println("epsilon:", epsilon, "best ref:", best2.head.score, "second best ref:", best2(1).score, observedDiff)

      val tie = if (epsilon <= breakTiesThreshold && parentRule.seenExmplsNum >= minSeenExmpls) true else false
      val couldExpand = passesTest || tie
      couldExpand match {
        case true =>
          best2.head.score > parentRule.score match {
            case true =>
              val refinedRule = best2.head
              /*
              for (z <- parentRule.refinements) {
                println(s"score: ${z.score}, tps: ${z.tps}, fps: ${z.fps}")
                println(z.tostring)
              }
              */
              showInfo(parentRule, best2.head, best2(1), epsilon, observedDiff, parentRule.seenExmplsNum)
              refinedRule.seenExmplsNum = 0 // zero the counter
              refinedRule.supportSet = parentRule.supportSet // only one clause here
              List(refinedRule)
            case _ => List(parentRule)
          }
        case _ => List(parentRule)
      }
    }
    Theory(out)
  }



  /**
    *
    * @param e
    * @param jep
    * @return
    *
    * Returns the total number of time points in the chunk (we use them to update the rules' counts for seen examples)
    * as well as the total number of tps. The latter is used along with the tps counts to update a global value of
    * the total number of examples, used in the m-estimate scoring function
    */
  def getExmplsNumInChunk(e: Exmpl, jep: Jep): (Int,Int) = {
    val example = (e.exmplWithInertia.annotationASP ++ e.exmplWithInertia.narrativeASP).mkString("\n")
    val totalFps = varbedExmplPatterns flatMap(x => List(s"\nfps($x):- $x, not example($x)."))
    val show = "\n#show time/1.\n#show fps/1."
    val program = example + s"\n\n#include " + "\""+Core.bkFile+"\".\n" + totalFps.mkString("\n") + show
    val answerSet = solve(program, jep)
    if (answerSet.nonEmpty){
      val (times,fps) = answerSet.head.atoms.foldLeft(0,0){ (x,atom) =>
        val _times = x._1
        val _fps = x._2
        if (atom.startsWith("time")) (_times+1,_fps)
        else (_times,_fps+1)
      }
      (times,fps)
    }
    else throw new RuntimeException("Got an empty answer set while asking for number of examples (time points) in chunk")
  }

  //def ruleHashCode(x: Clause) = x.## //Math.abs(x.##)
  def markAll(theory: Theory, targetClass: String) = {
    val directives = Core.markRulesDirectives
    //--------------------------------------------------
    //val programPart1 = markInit + markTerm + directives
    val programPart1 = directives
    //--------------------------------------------------
    val allRefinements = theory.clauses flatMap(_.refinements)
    val allRules = theory.clauses ++ allRefinements
    // generate marked versions for all:
    val markedTheory = theory.clauses map (_.marked)
    val markedRefinements = allRefinements map (_.marked)
    val allRulesMarked = markedTheory ++ markedRefinements
    // create a hashmap of hashcodes and the corresponding clauses
    val hashCodesClausesMap = (allRules map (x => x.##.toString -> x)).toMap
    val rulePredicates = hashCodesClausesMap.keySet.map(x => s"rule($x). ").mkString("\n")
    val show = "\n#show tps/2.\n#show fps/2.\n#show fns/2.\n"
    //------------------------------------------------------------------------------------------------
    val programPart2 = allRulesMarked.map(x => x.tostring+"\n").mkString("\n") + rulePredicates + show
    //------------------------------------------------------------------------------------------------
    (programPart1+programPart2,hashCodesClausesMap)
  }

  def scoreRules(theory: Theory, example: Example, jep: Jep, targetClass: String): Unit = {
    val e = (example.annotationASP ++ example.narrativeASP).mkString("\n")
    val markedProgram = markAll(theory, targetClass)
    val all = e + s"\n#include " + "\""+Core.bkMarkedFile+"\".\n" + markedProgram._1
    val f = Utils.getTempFile("isConsistent",".lp",deleteOnExit = true)
    Utils.writeToFile(f, "append")(p => List(all) foreach p.println)
    val path = f.getCanonicalPath
    val answerSet = ASP.solve(task = Core.INFERENCE, aspInputFile = new File(path), jep=jep)
    answerSet match {
      case Nil =>
      case _ =>
        val f = (atom: String) => {
          val tolit = Literal.toLiteral(atom)
          val (hash, example) = (tolit.terms.head.tostring, tolit.terms.tail.head.tostring)
          (tolit,hash,example)
        }
        val g = (hashCode: String, what: String) =>{
          val clause = markedProgram._2(hashCode)
          what match {
            case "tps" => clause.tps += 1
            case "fps" => clause.fps += 1
            case "fns" => clause.fns += 1
          }
        }
        val groupped = answerSet.head.atoms.par.foldLeft(List[String](),List[String](),List[String]()){
          (x,y) =>
            val (tps,fps,fns) = (x._1,x._2,x._3)
            val z = f(y)
            z._1.functor match {
              case "tps" => (tps :+ z._2,fps,fns)
              case "fps" => (tps,fps :+ z._2,fns)
              case "fns" => (tps,fps,fns :+ z._2 )
            }
        }
        val (tps,fps,fns) = (groupped._1,groupped._2,groupped._3)
        // increment the counts for each rule
        tps foreach (x => g(x,"tps"))
        fps foreach (x => g(x,"fps"))
      //fns foreach (x => g(x,"fns")) // uncomment this to get fns counts
    }

  }



  /*
  def scoreRules(theory: Theory, example: Example, jep: Jep): Unit = {
    def scoreRule(rule: Clause, e: String, jep: Jep) = {
      val typed = typedClause(rule)
      val bkFile =
        if(typed.head.functor == "initiatedAt") {
          Core.bkInitiatedOnly
        } else {
          Core.bkTerminatedOnly
        }
      val program = e + s"\n\n#include " + "\""+bkFile+"\".\n" + typed.tostring + tpsFpsFnsDefs.mkString("\n") + show
      val answerSet = solve(program,jep)
      if(answerSet.nonEmpty) {
        val atoms = answerSet.head.atoms
        atoms foreach { a =>
          if (a.startsWith("tps")) {
            rule.tps += 1
          }
          if (a.startsWith("fps")) {
            rule.fps += 1
          }
          if (a.startsWith("fns")) {
            rule.fns += 1
          }
        }
      }
    }

    val e = (example.annotationASP ++ example.narrativeASP).mkString("\n")

    for (rule <- theory.clauses) {
      //println("scoring")
      scoreRule(rule, e, jep)
      for (ref <- rule.refinements) {
        //println("scoring")
        scoreRule(ref, e, jep)
      }
    }
  }
  */

  val varbedExmplPatterns = for (x <- Core.examplePatterns) yield x.varbed.tostring
  val tpsFpsFnsDefs = varbedExmplPatterns flatMap(x => List(s"\nfns($x) :- example($x), not $x.", s"\nfps($x):- $x, not example($x).", s"\ntps($x):- $x, example($x).\n"))
  val show = "\n#show tps/1.\n#show fps/1.\n#show fns/1.\n"
  def typed(t: Theory) = t.clauses.map(x => x.withTypePreds().tostring).mkString("\n")
  def typedClause(c: Clause): Clause = c.withTypePreds()
  def exmpl(e: Exmpl) = (e.exmplWithInertia.annotationASP ++ e.exmplWithInertia.narrativeASP).mkString("\n")
  def solve(program: String, jep: Jep): List[AnswerSet] = {
    val f = Utils.getTempFile("isConsistent",".lp",deleteOnExit = true)
    Utils.writeToFile(f, "append")(p => List(program) foreach p.println)
    val path = f.getCanonicalPath
    ASP.solve(task = Core.INFERENCE, aspInputFile = new File(path), jep=jep)
  }

  def notCoversPositives(theory: Theory, e: Exmpl, jep: Jep, what: String) = {
    val t = (theory.withTypePreds map (_.tostring)).mkString("\n")
    val ex = exmpl(e)
    val program = ex + s"\n\n#include " + "\""+Core.bkInitiatedOnly+"\".\n" + t + tpsFpsFnsDefs.mkString("\n") + show
    val failure = (atoms: List[String]) => atoms.exists(p => p.startsWith("fns")) // positives left uncovered. This is used as failure test for initiatedAt Rules
    val answerSet = solve(program,jep)
    if (answerSet.nonEmpty) {
      val atoms = answerSet.head.atoms
      if (failure(atoms)) true
      else false
    } else {
      false
    }
  }

  def coversNegatives(theory: Theory, e: Exmpl, jep: Jep, what: String) = {
    val t = typed(theory)
    val ex = exmpl(e)
    val program = ex + s"\n\n#include " + "\""+Core.bkTerminatedOnly+"\".\n" + t + tpsFpsFnsDefs.mkString("\n") + show
    val failure = (atoms: List[String]) => atoms.exists(p => p.startsWith("fps")) //negatives covered. This is used as failure test for terminatedAt Rules
    val answerSet = solve(program,jep)
    if (answerSet.nonEmpty) {
      val atoms = answerSet.head.atoms
      if (failure(atoms)) {
        true
      } else {
        false
      }
    } else {
      false
    }
  }


  def showInfo(c: Clause, c1: Clause, c2: Clause, hoeffding: Double, observedDiff: Double, n: Int) = {
    logger.info("\n========================================" +
      s"\nHoeffding bound : $hoeffding" +
      s"\nobserved mean difference: $observedDiff" +
      s"\nNumber of examples used: $n\n" +
      "------------------------------------------------\n" +
      "Current rule (" + c.score + "): \n" +
      c.tostring + "\n" +
      "------------------------------------------------\n" +
      "Best refinement (" + c1.score + "): \n" +
      c1.tostring + "\n" +
      "------------------------------------------------\n" +
      "second best refinement (" + c2.score + "): \n" +
      c2.tostring + "\n" +
      "============================================")
  }


  /**
    *
    * Evaluate a hypothesis on the testing data
    *
    * @param t
    * @param DB
    * @param jep
    * @param testingSetSize this is used to drop the slice of the data on the which we didn't train
    *                       (so use them for testing). This works in conjunction with trainingSetSize
    *                       (see runWithDataChunks). E.g. if DB.size = 2000 and trainingSetSize = 1000
    *                       then runWithDataChunks will take the first 1000 examples for training. So,
    *                       here, we'll drop the first 1000 and use the rest for testing.
    * @param testData This list contains exmplIds for the examples that should be used for testing. This is
    *                 used in case we train on particular dataset and test on another (that's the normal way to
    *                 do it, stuff with testingSetSize/trainingSetSize & take/drop are used only for developement/debugging)
    */
  def crossVal(t: Theory, DB: Database, jep: Jep, testingSetSize: Int, testData: List[Int] = Nil) = {
    val dataIterator = DB.collection.find().sort(MongoDBObject("exampleId" -> 1)).drop(testingSetSize)
    while(dataIterator.hasNext) {
      val e = dataIterator.next()
      val ee = new Exmpl(e)
      println(ee.id)
      evaluateTheory(t, ee, withInertia=true, jep)
    }
    val stats = t.stats
    (stats._1, stats._2, stats._3, stats._4, stats._5, stats._6)
  }

  def evaluateTheory(theory: Theory, e: Exmpl, withInertia: Boolean = true, jep: Jep): Unit ={
    val varbedExmplPatterns = for (x <- Core.examplePatterns) yield x.varbed.tostring
    val coverageConstr = s"$TPS_RULES\n$FPS_RULES\n$FNS_RULES"
    val t = theory.clauses.map(x => x.withTypePreds().tostring).mkString("\n")
    val show = SHOW_TPS_ARITY_1 + SHOW_FPS_ARITY_1 + SHOW_FNS_ARITY_1
    val ex = if(withInertia) e.exmplWithInertia.tostring else e.exmplNoInertia.tostring
    val program = ex + INCLUDE_BK(Core.bkFile) + t + coverageConstr + show
    val f = Utils.getTempFile("isConsistent",".lp",deleteOnExit = true)
    Utils.writeLine(program, f, "overwrite")
    val answerSet = ASP.solve(task = Core.INFERENCE, aspInputFile = f, jep=jep)
    if (answerSet.nonEmpty) {
      val atoms = answerSet.head.atoms
      atoms.foreach { a=>
        val lit = Literal.toLiteral(a)
        val inner = lit.terms.head
        lit.functor match {
          case "tps" => theory.tps += inner.tostring
          case "fps" => theory.fps += inner.tostring
          case "fns" => theory.fns += inner.tostring
        }
      }
    }
  }


}

/**
  *
  * Various utilities for development
  *
  */

object DevUtils {

  /**
    *
    * @param DB
    * @param jep
    *
    * Iterate over the whole DB of pairs and create kernel sets.
    * IMPORTANT: For this to work you need to make sure that the full EC
    * is available. (in general, do not comment-out axioms in the main bk
    * file, but if you do, make sure you un-comment them before running this).
    *
    */
  def accumulateKernels(DB: Database, jep: Jep) = {
    val dataIterator = DB.collection.find().sort(MongoDBObject("exampleId" -> 1))
    while (dataIterator.hasNext) {
      val e = new Exmpl(dataIterator.next())
      val tt = iled.utils.Utils.generateKernel(e.exmplNoInertia.toMapASP,fromWeakExmpl = false, jep)
      val varKernel = tt._2

      val distMoreAndLess = (a: Literal, b: List[Literal]) => a.functor == "distLessThan" && b.view.exists(p => p.functor == "distMoreThan" && p.terms(2) == a.terms(2))
      def removeOne[A](elem: A, list: List[A]) = list.view.filter(_ != elem)
      val hasDistanceProblem = (a: List[Literal]) => true//a.exists( p => distMoreAndLess(p, removeOne(p, a)))
      if (varKernel.exists(p => hasDistanceProblem(p.body))) {
        println(e.id)
        println(Theory(varKernel).tostring)
      }
        //println(Theory(tt._2).tostring)
      //val test = List(1,2,3).find(_ > 3)
    }
  }

  /**
    * Stuff used for learning with no bottom clauses.
    */
  /*
val lits = List("happensAt(walking(X0),X2)","happensAt(walking(X1),X2)","happensAt(active(X0),X2)","happensAt(active(X1),X2)","happensAt(inactive(X0),X2)",
  "happensAt(inactive(X1),X2)","happensAt(running(X0),X2)","happensAt(running(X1),X2)","happensAt(abrupt(X0),X2)","happensAt(abrupt(X1),X2)","distLessThan(X0,X1,27,X2)",
  "distLessThan(X1,X0,27,X2)","distLessThan(X0,X1,25,X2)","distLessThan(X1,X0,25,X2)","distLessThan(X0,X1,24,X2)","distLessThan(X1,X0,24,X2)","distLessThan(X0,X1,34,X2)",
  "distLessThan(X1,X0,34,X2)","distLessThan(X0,X1,40,X2)","distLessThan(X1,X0,40,X2)","distMoreThan(X0,X1,27,X2)","distMoreThan(X1,X0,27,X2)","distMoreThan(X0,X1,25,X2)",
  "distMoreThan(X1,X0,25,X2)","distMoreThan(X0,X1,24,X2)","distMoreThan(X1,X0,24,X2)","distMoreThan(X0,X1,34,X2)","distMoreThan(X1,X0,34,X2)","distMoreThan(X0,X1,40,X2)",
  "distMoreThan(X1,X0,40,X2)","orientClose(X0,X1,45,X2)","orientClose(X1,X0,45,X2)","orientFar(X0,X1,45,X2)","orientFar(X1,X0,45,X2)","holdsAt(visible(X0),X2)",
  "holdsAt(visible(X1),X2)","happensAt(appear(X0),X2)","happensAt(appear(X1),X2)","happensAt(disappear(X0),X2)","happensAt(disappear(X1),X2)")
val bodyList = lits map (Literal.toLiteral(_))
val initBottomRule = Clause(head=Literal.toLiteral("initiatedAt(moving(X0,X1),X2)"),body=bodyList)
val termBottomRule = Clause(head=Literal.toLiteral("terminatedAt(moving(X0,X1),X2)"),body=bodyList)
var totalPosInData = 0 // this is updated at the scoreRules method below
var totalNegsInData = 0 // this is updated at the scoreRules method below
val mparam = 5.0 // m parameter for the m-estimate calculation

def mestimate(c: Clause) = {
  c.mestimate(mparam = mparam, totalPositives = totalPosInData, totalNegatives = totalNegsInData)
}


 def processExampleNoBottomRules(topTheory: Theory, e: Exmpl, jep: Jep, delta: Double, breakTiesThreshold: Double, minSeenExmpls: Int): Theory = {
    var newTopTheory = topTheory
    if (notCoversPositives(topTheory, e, withInertia = true, jep = jep)) {
      val newRules = List(Clause(head=universalBottomRule.head, body = List()))
      newRules.foreach(x => x.addToSupport(universalBottomRule))
      logger.info(s"Started growing new rule: \n ${newRules.head.head.tostring}")
      /*
      val newRules = varKernel map { x =>
        val c = Clause(head=x.head, body = List())
        logger.info(s"Started growing new rule: \n ${c.tostring} from bottom clause: \n ${x.tostring}")
        c.addToSupport(x)
        c
      }
      */
      newTopTheory = topTheory.clauses ++ newRules
    }
    scoreRules(newTopTheory, e, jep)
    expandRules(newTopTheory, delta, breakTiesThreshold, minSeenExmpls)
  }


*/

}

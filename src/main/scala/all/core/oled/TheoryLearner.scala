package all.core.oled

import akka.actor.Actor
import com.typesafe.scalalogging.LazyLogging
import all.core.Core
import all.globalValues.GlobalValues
import all.structures.{Clause, Theory}
import all.utils.{CaviarUtils, Utils, Exmpl, Database}
import jep.Jep
import all.core.Implicits._
import scala.collection.mutable.ListBuffer
import scala.math._


/**
  * Created by nkatz on 27/2/2016.
  *
  */

/**
  *
  * @param DB The database of examples
  * @param delta Hoeffding delta
  * @param breakTiesThreshold
  * @param postPruningThreshold All rules with score < this are pruned
  * @param minSeenExmpls Minimun number of examples on which a rule must be evaluated before ties may be broken
  * @param repeatFor Re-sees the data this-many times
  * @param chunkSize Instead of getting pairs from the DB, get chunks to speed things up
  * @param targetClass initiated or terminated
  * @param withInertia This is used for experiments with learning from weak/strong examples. If false, learning
  *                    is performed without inertia (for the initiated part of the theory). This parameter is
  *                    only used for getting the examples from the DB (see also CaviarUtils.getDataAsChunks)
  */

class TheoryLearner(val DB: Database, val delta: Double, val breakTiesThreshold: Double,
                    val postPruningThreshold: Double, val minSeenExmpls: Double, trainingSetSize: Int,
                    val repeatFor: Int, val chunkSize: Int, targetClass: String,
                    withInertia: Boolean, withPostPruning: Boolean, trainingData: TrainingSet = TrainingSet(),
                    val HLE: String, val learningInitWithInertia: Boolean = false) extends GlobalValues with LazyLogging with Actor {

  /**
    * utils and methods for learning an initiatedAt or terminatedAt theory (separately)
    */

  // These are vars used to compute runtime statistics
  // (running averages over the stream), like mean
  // processing time per example and mean size of the
  // ground program per example
  var timeRunningMean = 0.0
  var groundRunningMean = 0.0
  var exmplCounter = 0

  def receive = {
    case "go" => sender ! run
    //context.stop(self)
  }

  val jep = new Jep()
  //jep.runScript(GlobalValues.ASPHandler)
  val globals = new GlobalValues()

  val specialBKfile = if(targetClass=="initiated") BK_INITIATED_ONLY else BK_TERMINATED_ONLY

  //val specialBKfile = if(targetClass=="initiated") BK_WHOLE_EC else BK_TERMINATED_ONLY

  /*
  val specialBKfile =
    if (learningInitWithInertia) {
      if(targetClass=="initiated") BK_OLED_WITH_INERTIA else BK_TERMINATED_ONLY
    } else {
      if(targetClass=="initiated") BK_INITIATED_ONLY else BK_TERMINATED_ONLY
    }
   */

  //val specialBKfile =  BK_TERMINATED_ONLY

  val initorterm = if(targetClass=="initiated") "initiatedAt" else "terminatedAt"
  //val wholeECBKFile = BK_WHOLE_EC
  //val trainingSetSize = 10000000 // only for development, won't be used at experiments
  def getData = all.utils.CaviarUtils.getDataAsChunks(DB, chunkSize, targetClass, withInertia).take(trainingSetSize)

  def getTrainingData: Iterator[Exmpl] = {
    if (trainingData.asInstanceOf[TrainingSet].trainingSet == Nil) {
      trainingData.asInstanceOf[all.core.oled.experimentsMLNdata.MLNDataHandler.TrainingSet].trainingData.toIterator
    } else {
      if (this.trainingData == TrainingSet()) getData else CaviarUtils.getDataFromIntervals(DB,HLE,this.trainingData.trainingSet,chunkSize)
    }
  }

  def run: (Theory,Double) = {

    def runOnce(inTheory: Theory): Theory = {
      //val dataChunks = scala.util.Random.shuffle(iled.utils.CaviarUtils.getDataAsChunks(DB, chunkSize)) // do not shuffle to see what happens

      // helper method to compute the running mean for various statistics
      def getRunningMean(what: String, newValue: Double) = {
        // The running average can be computed by
        // ((prevAvg*n) + newValue)/(n+1)
        // where n is the number of seen data points
        def runningMean(prevAvg: Double, newVal: Double, n: Int) = ((prevAvg*n) + newValue)/(n+1)
        what match {
          case "time" => runningMean(this.timeRunningMean, newValue, this.exmplCounter)
          case "groundPrg" => runningMean(this.groundRunningMean, newValue, this.exmplCounter)
        }
      }

      val data = getTrainingData

      data.foldLeft(inTheory){ (topTheory, newExample) =>
        //println(newExample.time)
        val res = Utils.time {
          processExample(topTheory, newExample, learningInitWithInertia)
        }
        //----------------------------------------------------------------
        // Getting some runtime stats
        val lastProcTime = res._2
        val newMeanProcTime = getRunningMean("time",lastProcTime)
        val newMeanGrndSize = getRunningMean("groundPrg",GlobalValues.grnd.toDouble)
        logger.debug(s"\nExmpl ${this.exmplCounter} Mean processing time so far: $newMeanProcTime sec\nMean grnd prg size: $newMeanGrndSize")
        // Updating global vars for the next computation of running means
        this.timeRunningMean = newMeanProcTime
        this.groundRunningMean = newMeanGrndSize
        this.exmplCounter += 1
        //----------------------------------------------------------------
        res._1
      }
    }

    logger.info(s"Starting learning for $targetClass")

    val _finalTheory = Utils.time{ (1 to repeatFor).foldLeft(Theory())( (t,_) =>  runOnce(t)) }
    val (finalTheory,time) = (_finalTheory._1,_finalTheory._2)
    logger.info(s"\nTraining time for $targetClass: $time")
    val output = withPostPruning match {
      case true =>
        //logger.info(s"Starting post-pruning for $targetClass")
        //logger.info(s"Rescoring $targetClass theory")
        reScore(DB,finalTheory,chunkSize,this.jep,trainingSetSize,targetClass, withInertia)
        logger.debug(s"\nLearned hypothesis:\n${finalTheory.showWithStats}")
        val pruned = finalTheory.clauses.filter(x => x.score > postPruningThreshold)
        logger.debug(s"\nPruned hypothesis:\n${pruned.showWithStats}")
        //pruned.foreach { p => p.clearStatistics } // No need. I want to display the stats and also they cause no problem since the theory is evaluated as a whole
        Theory(pruned)
      case _ => finalTheory
    }
    logger.debug(s"\n$targetClass theory found:\n${output.tostring}")
    this.jep.close()

    (output,time)
  }

  def processExample(topTheory: Theory, e: Exmpl, learningInitWithInertia: Boolean): Theory = {
    var newTopTheory = topTheory
    val startNew = newTopTheory.growNewRuleTest(e, this.jep, initorterm, globals)
    if (startNew) {
      val newRules = generateNewRules(topTheory, e, learningInitWithInertia)
      newTopTheory = topTheory.clauses ++ newRules
    }
    if (newTopTheory.clauses.nonEmpty) {
      //val t0 = System.nanoTime()
      newTopTheory.scoreRules(e.exmplWithInertia, this.jep, globals)
      //val t1 = System.nanoTime()
      //println(s"scoreRules time: ${(t1-t0)/1000000000.0}")

      try {
        expandRules(newTopTheory)
      } catch {
        case z: IndexOutOfBoundsException =>
          println(s"top theory:\n ${topTheory.tostring}")
          println(e.id)
          Theory()
      }

    } else {
      newTopTheory
    }
  }

  def generateNewRules(topTheory: Theory, e: Exmpl, learningInitWithInertia: Boolean = false) = {
    val terminatedOnly = if(initorterm=="terminatedAt") true else false
    val (_, varKernel) =
      Utils.generateKernel(e.exmplWithInertia.toMapASP, jep = this.jep, bk = specialBKfile,
        learningTerminatedOnly = terminatedOnly, oledLearningInitWithInertia = learningInitWithInertia)
    val bottomTheory = topTheory.clauses flatMap(x => x.supportSet.clauses)
    val goodKernelRules =
      varKernel.filter(newBottomRule => !bottomTheory.exists(supportRule => newBottomRule.thetaSubsumes(supportRule)))
    goodKernelRules map { x =>
      val c = Clause(head=x.head, body = List())
      logger.debug(s"Started growing new rule: \n ${c.tostring} from bottom clause: \n ${x.tostring}")
      c.addToSupport(x)
      c
    }
  }

  def wrongWay(parentRule: Clause) = {
    val best2 = parentRule.refinements.sortBy { x => - x.score }.take(2)
    val best = best2.head
    val secondBest = best2(1)
    val epsilon = sqrt(scala.math.log(1.0 / delta) / (2 * parentRule.seenExmplsNum))
    val observedDiff = best2.head.score - best2(1).score
    val passesTest = if (epsilon < observedDiff) true else false
    val tie = if (epsilon <= breakTiesThreshold && parentRule.seenExmplsNum >= minSeenExmpls) true else false
    val couldExpand = passesTest || tie
    (couldExpand,epsilon,observedDiff,best,secondBest)
  }

  def rightWay(parentRule: Clause) = {
    val (observedDiff,best,secondBest) = parentRule.meanDiff
    val epsilon = sqrt(scala.math.log(1.0 / delta) / (2 * parentRule.seenExmplsNum))
    val passesTest = if (epsilon < observedDiff) true else false
    val tie = if (epsilon <= breakTiesThreshold && parentRule.seenExmplsNum >= minSeenExmpls) true else false
    val couldExpand = passesTest || tie
    (couldExpand,epsilon,observedDiff,best,secondBest)
  }

  def expandRules(topTheory: Theory): Theory = {

    //val t0 = System.nanoTime()

    val out = topTheory.clauses flatMap { parentRule =>
      //val best2 = parentRule.refinements.sortBy { x => - x.score }.take(2)
      /*
      // - sign is to order with decreasing order (default is with increasing)
      val best2 = parentRule.refinements.sortBy { x => - x.score }.take(2)
      val epsilon = sqrt(scala.math.log(1.0 / delta) / (2 * parentRule.seenExmplsNum))
      val observedDiff = best2.head.score - best2(1).score
      val passesTest = if (epsilon < observedDiff) true else false
      val tie = if (epsilon <= breakTiesThreshold && parentRule.seenExmplsNum >= minSeenExmpls) true else false
      val couldExpand = passesTest || tie
      */

      val (couldExpand,epsilon,observedDiff,best,secondBest) = rightWay(parentRule)
      couldExpand match {
        case true =>
          best.score > parentRule.score match {
            case true =>
              val refinedRule = best
              logger.debug(showInfo(parentRule, best, secondBest, epsilon, observedDiff, parentRule.seenExmplsNum))
              refinedRule.seenExmplsNum = 0 // zero the counter
              refinedRule.supportSet = parentRule.supportSet // only one clause here
              List(refinedRule)
            case _ => List(parentRule)
          }
        case _ => List(parentRule)
      }
    }

    //val t1 = System.nanoTime()
    //println(s"expandRules time: ${(t1-t0)/1000000000.0}")

    Theory(out)
  }


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
  def reScore(DB: Database, theory: Theory,
              chunkSize: Int, jep: Jep,
              trainingSetSize: Int, what: String, withInertia: Boolean) = {
    val dataChunks = getTrainingData
    theory.clauses foreach (p => p.clearStatistics) // zero all counters before re-scoring
    for (x <- dataChunks) {
      //println(x.id)
      theory.scoreRules(x.exmplWithInertia,jep,globals, postPruningMode = true)
    }
    logger.debug( theory.clauses map { p => s"score: ${p.score}, tps: ${p.tps}, fps: ${p.fps}, fns: ${p.fns}\n${p.tostring}" } mkString("\n") )
  }


  def showInfo(c: Clause, c1: Clause, c2: Clause, hoeffding: Double, observedDiff: Double, n: Int) = {
    "\n========================================" +
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
      "============================================"
  }




}

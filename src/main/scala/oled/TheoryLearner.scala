package oled

import akka.actor.Actor
import com.typesafe.scalalogging.LazyLogging
import app.Globals
import logic.{LogicUtils, Clause, Theory}
import utils.DataUtils.{DataAsExamples, DataAsIntervals, TrainingSet}
import utils.{CaviarUtils, Utils, Exmpl, Database}
import jep.Jep
import utils.Database

import utils.Implicits._

/**
  * Created by nkatz on 27/2/2016.
  *
  */

/**
  *
  * @param DB The database of examples
  * @param delta Hoeffding delta
  * @param breakTiesThreshold
  * @param pruningThreshold All rules with score < this are pruned
  * @param minSeenExmpls Minimun number of examples on which a rule must be evaluated before ties may be broken
  * @param repeatFor Re-sees the data this-many times
  * @param chunkSize Instead of getting pairs from the DB, get chunks to speed things up
  * @param targetClass initiated or terminated
  * @param withInertia This is used for experiments with learning from weak/strong examples. If false, learning
  *                    is performed without inertia (for the initiated part of the theory). This parameter is
  *                    only used for getting the examples from the DB (see also CaviarUtils.getDataAsChunks)
  */



class TheoryLearner(val DB: Database,
                    val delta: Double,
                    val breakTiesThreshold: Double,
                    val pruningThreshold: Double,
                    val minSeenExmpls: Double,
                    trainingSetSize: Int,
                    val repeatFor: Int,
                    val chunkSize: Int,
                    targetClass: String,
                    withInertia: Boolean,
                    withPostPruning: Boolean,
                    onlinePruning: Boolean,
                    data: TrainingSet,
                    val HLE: String,
                    val learningInitWithInertia: Boolean = false,
                    kernelSet: Theory = Theory(),
                    globals: Globals) extends LazyLogging with Actor {



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

  val bottomClauses =
    if (kernelSet.clauses.nonEmpty) {
      if (this.targetClass == "initiated") {
        Theory(kernelSet.clauses.filter(x => x.head.tostring.contains("initiatedAt")))
      } else {
        Theory(kernelSet.clauses.filter(x => x.head.tostring.contains("terminatedAt")))
      }
    } else {
      kernelSet
    }

  def receive = {
    case "go" => sender ! run
  }

  val jep = new Jep()
  //jep.runScript(GlobalValues.ASPHandler)

  val specialBKfile = if(targetClass=="initiated") globals.BK_INITIATED_ONLY else globals.BK_TERMINATED_ONLY

  val initorterm = if(targetClass=="initiated") "initiatedAt" else "terminatedAt"

  def getData = utils.CaviarUtils.getDataAsChunks(DB, chunkSize, targetClass, withInertia).take(trainingSetSize)

  def getTrainingData: Iterator[Exmpl] = {
    data match {
      case x: DataAsIntervals =>
        if (data.isEmpty)
          getData
        else
          CaviarUtils.getDataFromIntervals(DB, HLE, data.asInstanceOf[DataAsIntervals].trainingSet, chunkSize)
      case x: DataAsExamples =>
        data.asInstanceOf[DataAsExamples].trainingSet.toIterator
      case _ =>
        throw new RuntimeException(s"${data.getClass}: Don't know what to do with this data container!")
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
        //if (newExample.time == "507120") {
        //  val stop = "stop"
        //}
        val res = Utils.time {
          processExample(topTheory, newExample, learningInitWithInertia)
        }
        //----------------------------------------------------------------
        // Getting some runtime stats
        //val lastProcTime = res._2
        //val newMeanProcTime = getRunningMean("time",lastProcTime)
        //val newMeanGrndSize = getRunningMean("groundPrg",GlobalValues.grnd.toDouble)
        //logger.debug(s"\nExmpl ${this.exmplCounter} Mean processing time so far: $newMeanProcTime sec\nMean grnd prg size: $newMeanGrndSize")
        // Updating global vars for the next computation of running means
        //this.timeRunningMean = newMeanProcTime
        //this.groundRunningMean = newMeanGrndSize
        //this.exmplCounter += 1
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
        logger.info(s"Starting post-pruning for $targetClass")
        logger.info(s"Rescoring $targetClass theory")
        reScore(DB,finalTheory,chunkSize,this.jep,trainingSetSize,targetClass, withInertia)
        logger.info(s"\nLearned hypothesis (before pruning):\n${finalTheory.showWithStats}")
        val pruned = finalTheory.clauses.filter(x => x.score > pruningThreshold)
        logger.debug(s"\nPruned hypothesis:\n${pruned.showWithStats}")
        Theory(pruned)
      case _ =>
        // no re-scoring
        val pruned = finalTheory.clauses.filter(x => x.score > pruningThreshold)
        logger.info(s"\nLearnt hypothesis (non-pruned):\n${finalTheory.showWithStats}")
        Theory(pruned)
      //finalTheory
    }
    logger.debug(s"\n$targetClass theory found:\n${output.tostring}")
    this.jep.close()

    (output,time)
  }

  def processExample(topTheory: Theory, e: Exmpl, learningInitWithInertia: Boolean): Theory = {
    var newTopTheory = topTheory
    if (this.bottomClauses.clauses.isEmpty){ // in the opposite case we've collected the BCs in a first pass over the data.
    val startNew = newTopTheory.growNewRuleTest(e, this.jep, initorterm, globals)
      if (startNew) {
        val newRules = generateNewRules(topTheory, e, learningInitWithInertia)
        newTopTheory = topTheory.clauses ++ newRules
        //-------------------------------------------------------------
        // updateGlobals Used in a test to see if we can learn better
        // theories by down-scoring candidates that are
        // very similar to existing clauses in the current
        // hypothesis. Until I come up with a cleaner solution,
        // I simply store the current version of the the running
        // hypothesis in the globals, so that the Haussdorff
        // distance of each candidate from each existing clause
        //-----------------------
        //updateGlobals(newRules)
        //-----------------------
        //-------------------------------------------------------------
      }
    }
    if (newTopTheory.clauses.nonEmpty) {
      //val t0 = System.nanoTime()
      newTopTheory.scoreRules(e.exmplWithInertia, this.jep, globals)
      //val t1 = System.nanoTime()
      //println(s"scoreRules time: ${(t1-t0)/1000000000.0}")

      try {
        val expanded = expandRules(newTopTheory)
        if (onlinePruning) {
          pruneRules(expanded)
        } else {
          expanded
        }
      } catch {
        case z: IndexOutOfBoundsException =>
          println(s"top theory:\n ${topTheory.tostring}")
          println(e.id)
          Theory()
      }
    } else {
      if (this.bottomClauses.clauses.isEmpty) {
        newTopTheory
      } else {
        // generate a top theory from the already constructed bottom clauses
        val top = this.bottomClauses.clauses map { x =>
          val c = Clause(head=x.head, body = List())
          logger.debug(s"Started growing new rule: \n ${c.tostring} from bottom clause: \n ${x.tostring}")
          c.addToSupport(x)
          c
        }
        Theory(top)
      }
    }
  }

  def pruneRules(topTheory: Theory) = {
    val pruned = topTheory.clauses.foldLeft(List[Clause]()){ (keep, clause) =>
      val epsilon = Utils.hoeffding(delta, clause.seenExmplsNum)
      val meanPruningScore = clause.meanScorePruning(this.pruningThreshold)
      //if (this.pruningThreshold - meanScore > epsilon && clause.seenExmplsNum > minSeenExmpls) {
      if (meanPruningScore > epsilon && clause.seenExmplsNum > minSeenExmpls*10) {
        logger.info(s"\nPruned clause:\n${clause.tostring}\nMean score" +
          s" so far: ${clause.meanScorePruning(this.pruningThreshold)} | tps: ${clause.tps} fps: ${clause.fps}, fns: ${clause.fns}")
        keep
      } else {
        keep :+ clause
      }
    }
    Theory(pruned)
  }

  /*
  def updateGlobals(newRules: List[Clause]) = {
    if (this.targetClass=="initiated") GlobalValues.CurrentTheoryInitiated =  newRules
    else GlobalValues.CurrentTheoryTerminated =  newRules
  }
  */

  def generateNewRules(topTheory: Theory, e: Exmpl, learningInitWithInertia: Boolean = false) = {
    val terminatedOnly = if(initorterm=="terminatedAt") true else false
    val (_, varKernel) =
      LogicUtils.generateKernel(e.exmplWithInertia.toMapASP, jep = this.jep, learningTerminatedOnly = terminatedOnly,
                                oledLearningInitWithInertia = learningInitWithInertia, bkFile = specialBKfile, globals=globals)
    val bottomTheory = topTheory.clauses flatMap(x => x.supportSet.clauses)
    val goodKernelRules = varKernel.filter(newBottomRule => !bottomTheory.exists(supportRule => newBottomRule.thetaSubsumes(supportRule)))
    ///*
    goodKernelRules map { x =>
      val c = Clause(head=x.head, body = List())
      logger.debug(s"Started growing new rule: \n ${c.tostring} from bottom clause: \n ${x.tostring}")
      //println(x.tostring)
      c.addToSupport(x)
      c
    }
    //*/
    // Just a test to use all available mode literals at a bottom clause
    /*
    val bc1 = if (this.targetClass=="initiated") this.globals.initBC1 else this.globals.termBC1
    val bc2 = if (this.targetClass=="initiated") this.globals.initBC2 else this.globals.termBC2
    val c1 = Clause(head=bc1.head, body = List())
    val c2 = Clause(head=bc2.head, body = List())
    c1.addToSupport(bc1)
    c2.addToSupport(bc2)
    //logger.info(s"Started growing new rule: \n ${c.tostring} from bottom clause: \n ${bc.tostring}")
    List(c1,c2)
    */
  }

  def wrongWay(parentRule: Clause) = {
    val best2 = parentRule.refinements.sortBy { x => - x.score }.take(2)
    val best = best2.head
    val secondBest = best2(1)
    val epsilon = Utils.hoeffding(delta, parentRule.seenExmplsNum)
    val observedDiff = best2.head.score - best2(1).score
    val passesTest = if (epsilon < observedDiff) true else false
    val tie = if (epsilon <= breakTiesThreshold && parentRule.seenExmplsNum >= minSeenExmpls) true else false
    val couldExpand = passesTest || tie
    (couldExpand,epsilon,observedDiff,best,secondBest)
  }

  def rightWay(parentRule: Clause) = {
    val (observedDiff,best,secondBest) = parentRule.meanDiff
    val epsilon = Utils.hoeffding(delta, parentRule.seenExmplsNum)
    val passesTest = if (epsilon < observedDiff) true else false
    //val tie = if (epsilon <= breakTiesThreshold && parentRule.seenExmplsNum >= minSeenExmpls) true else false
    val tie = if (observedDiff < epsilon  && epsilon < breakTiesThreshold && parentRule.seenExmplsNum >= minSeenExmpls) true else false

    val couldExpand = passesTest || tie
    (couldExpand,epsilon,observedDiff,best,secondBest)
  }

  def expandRules(topTheory: Theory): Theory = {

    //val t0 = System.nanoTime()

    val out = topTheory.clauses flatMap { parentRule =>
      val (couldExpand,epsilon,observedDiff,best,secondBest) = rightWay(parentRule)
      couldExpand match {
        case true =>
          ///*
          // This is the extra test that I added at Feedzai
          val extraTest =
            if(secondBest != parentRule) (best.score > parentRule.score) && (best.score - parentRule.score > epsilon)
            else best.score > parentRule.score
          extraTest match { //&& (1.0/best.body.size+1 > 1.0/parentRule.body.size+1) match {
            case true =>
              val refinedRule = best
              logger.info(showInfo(parentRule, best, secondBest, epsilon, observedDiff, parentRule.seenExmplsNum))
              refinedRule.seenExmplsNum = 0 // zero the counter
              refinedRule.supportSet = parentRule.supportSet // only one clause here
              List(refinedRule)
            case _ => List(parentRule)
          }
        //*/
        //val refinedRule = best
        //logger.info(showInfo(parentRule, best, secondBest, epsilon, observedDiff, parentRule.seenExmplsNum))
        //refinedRule.seenExmplsNum = 0 // zero the counter
        //refinedRule.supportSet = parentRule.supportSet // only one clause here
        //List(refinedRule)
        case _ => List(parentRule)
      }
    }

    //val t1 = System.nanoTime()
    //println(s"expandRules time: ${(t1-t0)/1000000000.0}")

    //-------------------------------------------------------------
    // updateGlobals Used in a test to see if we can learn better
    // theories by down-scoring candidates that are
    // very similar to existing clauses in the current
    // hypothesis. Until I come up with a cleaner solution,
    // I simply store the current version of the the running
    // hypothesis in the globals, so that the Haussdorff
    // distance of each candidate from each existing clause
    //updateGlobals(out)
    //-------------------------------------------------------------

    Theory(out)
  }

  //def debugExpandRules()


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

    /*
    val similarity = (x: Clause) => {
      if (this.targetClass=="initiated"){
        //println(Theory(GlobalValues.CurrentTheoryInitiated).tostring)
        Utils.similarity(x,GlobalValues.CurrentTheoryInitiated)
      } else {
        //println(Theory(GlobalValues.CurrentTheoryTerminated).tostring)
        Utils.similarity(x,GlobalValues.CurrentTheoryTerminated)
      }
    }
    */
    s"\n===========================================================\n" +
      s"\nClause (score: ${c.score} | tps: ${c.tps} fps: ${c.fps} fns: ${c.fns})\n\n${c.tostring}\n\nwas refined to" +
      s" (new score: ${c1.score} | tps: ${c1.tps} fps: ${c1.fps} fns: ${c1.fns})\n\n${c1.tostring}\n\nε: $hoeffding, ΔG: $observedDiff, examples used: $n" +
      //s"\nall refs: \n\n ${c.refinements.sortBy(z => -z.score).map(x => x.tostring+" "+" | score "+x.score+" | similarity "+similarity(x)).mkString("\n")}" +
      s"\nall refs: \n\n ${c.refinements.sortBy(z => (-z.score,z.body.length+1)).map(x => x.tostring+" | score "+x.score+" (tps|fps|fns): "+(x.tps,x.fps,x.fns)).mkString("\n")}" +
      s"\n===========================================================\n"

    /*
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
    */
  }




}

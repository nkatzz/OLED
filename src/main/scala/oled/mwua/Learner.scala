package oled.mwua

import java.io.File

import akka.actor.{Actor, ActorRef, PoisonPill, Props}
import app.runutils.IOHandling.Source
import app.runutils.{Globals, RunningOptions}
import logic.Examples.Example
import logic._
import oled.mwua.MessageTypes.{FinishedBatchMsg, ProcessBatchMsg}
import org.slf4j.LoggerFactory
import oled.functions.SingleCoreOLEDFunctions.{crossVal, eval}

import scala.collection.mutable.{ListBuffer, Map}
import AuxFuncs._
import utils.{ASP, Utils}
import utils.Implicits._

import scala.util.control.Breaks._
import scala.reflect.internal.Trees
import java.util.Random

import scala.util.matching.Regex


/**
  * Created by nkatz at 26/10/2018
  */

/*
*
* I ran this on the normal CAVIAR ordering as follows:
* --inpath=/home/nkatz/dev/OLED-BK/BKExamples/BK-various-taks/DevTest/caviar-bk --delta=0.00001 --prune=0.8
* --train=caviar --repfor=4 --chunksize=50 --try-more-rules=true --scorefun=default --onlineprune=true
*
* */

class Learner[T <: Source](val inps: RunningOptions,
                           val trainingDataOptions: T,
                           val testingDataOptions: T,
                           val trainingDataFunction: T => Iterator[Example],
                           val testingDataFunction: T => Iterator[Example],
                           val writeExprmtResultsTo: String = "") extends Actor {

  startTime = System.nanoTime()

  /*
  private var totalTPs = 0
  private var totalFPs = 0
  private var totalFNs = 0
  private var totalTNs = 0
  */

  //--------------------------
  val normalizeWeights = true
  //--------------------------

  private var totalTPs = Set[String]()
  private var totalFPs = Set[String]()
  private var totalFNs = Set[String]()
  private var totalTNs = Set[String]()

  private var totalBatchProcessingTime = 0.0
  private var totalRuleScoringTime = 0.0
  private var totalNewRuleTestTime = 0.0
  private var totalCompressRulesTime = 0.0
  private var totalExpandRulesTime = 0.0
  private var totalNewRuleGenerationTime = 0.0

  private var totalWeightsUpdateTime = 0.0
  private var totalgroundingsTime = 0.0
  private var totalPredictionTime = 0.0

  private val logger = LoggerFactory.getLogger(self.path.name)

  private val withec = Globals.glvalues("with-ec").toBoolean

  private var bestTheoryFoundSoFar = Theory()

  // This map cantanins all fluents that were true previously,
  // (i.e. at the time point prior to the one that is currently being processed)
  // along with their weights. The weights are updated properly at each time point
  // and new atoms are added if we predict that they start holding, and
  // existing atoms are removed if we predict that they're terminated.
  // The key values are string representations of fluents, not holdsAt/2 atoms.
  // So, we're storing "meeting(id1,id2)", not "holdsAt(meeting(id1,id2), 10)".
  private var inertiaExpert = scala.collection.mutable.Map[String, Double]()

  def getInertiaExpertPrediction(fluent: String) = {
    if (inertiaExpert.keySet.contains(fluent)) inertiaExpert(fluent) else 0.0
  }

  val learningRate = 1.0

  //----------------------------------------------------------------------
  // If true, the firing/non-firing initiation rules are not taken
  // into account when making a prediction about a fluent that persists
  // by inertia.
  // Setting this to false is the default for learning/reasoning with
  // weakly initiated fluents, but setting it to true is necessary for
  // strongly initiated settings, in order to allow for previously
  // recognized fluents to persist.
  private val isStrongInertia = false
  //----------------------------------------------------------------------


  /* All these are for presenting analytics/results after a run. */
  private val initWeightSums = new ListBuffer[Double]
  private val nonInitWeightSums = new ListBuffer[Double]
  private val TermWeightSums = new ListBuffer[Double]
  private val monTermWeightSums = new ListBuffer[Double]
  private val predictInitWeightSums = new ListBuffer[Double]
  private val predictTermWeightSums = new ListBuffer[Double]
  private val inertWeightSums = new ListBuffer[Double]
  private val prodictHoldsWeightSums = new ListBuffer[Double]
  // For each query atom encountered during a run, 0.0 or 1.0 is stored in this buffer (true false)
  private val trueLabels = new ListBuffer[Double]
  // Keep weights only for this
  val keepStatsForFluent = "meeting(id4,id5)"

  // Control learning iterations over the data
  private var repeatFor = inps.repeatFor

  // Used to count examples for holdout evaluation
  private var exampleCounter = 0

  // Local data variable. Cleared at each iteration (in case repfor > 1).
  private var data = Iterator[Example]()

  // This is optional. A testing set (for holdout evaluation) may not be provided.
  private var testingData = Iterator[Example]()

  // Counts the number of precessed batches. Used to determine when to
  // perform holdout evaluation on the test set. Incremented whenever a
  // new batch is fetched (see the getNextBatch() method)
  private var batchCounter = 0

  // Stores the error from the prequential evaluation at each batch.
  private var prequentialError = Vector[Double]()

  // Current prequential error (for logging only, updated as a string message containing the actual error).
  private var currentError = ""

  // Stores the F1-scores from holdout evaluation
  private var holdoutScores = Vector[Double]()

  // Evolving theory. If we're learning with the Event Calculus the head of the
  // list is the initiation part of the theory and the tail is the termination.
  // If not, the list has a single element (the current version of the theory).
  private var theory = if (withec) List(Theory(), Theory()) else List(Theory())

  private var startTime = System.nanoTime()

  private var endTime = System.nanoTime()

  // Get the training data from the current inout source
  private def getTrainData = trainingDataFunction(trainingDataOptions)

  private def getTestingData = testingDataFunction(testingDataOptions)

  private def getNextBatch(lleNoise: Boolean = false) = {
    this.batchCounter += 1
    if (data.isEmpty) {
      Example()
    } else {
      if (!lleNoise) {
        data.next()
      } else {
        val currentBatch = data.next()
        val noisyNarrative = {
          currentBatch.narrative map { x =>
            x.replaceAll("active", "active_1")
          }
        }
        Example(annot = currentBatch.annotation, nar = noisyNarrative, _time=currentBatch.time)
      }
    }
  }


  val workers: List[ActorRef] = {

    // Two workers for initiated and terminated rules respectively.
    if (withec) {
      val worker1 = context.actorOf(Props( new Worker(inps) ), name = "worker-1")
      val worker2 = context.actorOf(Props( new Worker(inps) ), name = "worker-2")
      List(worker1, worker2)
    } else {
      val worker = context.actorOf(Props( new Worker(inps) ), name = "worker")
      List(worker)
    }
  }

  // Use this variable to count the responses received from worker actors while processing a new batch.
  private var responseCounter = workers.length

  // Keep response messages from workers in here until all workers are done.
  private val responses = Map[String, FinishedBatchMsg]()

  def receive = {

    case "start" => {
      this.repeatFor -= 1
      this.data = getTrainData
      if (inps.test != "None") this.testingData = getTestingData
      if (this.data.isEmpty) {
        logger.error(s"Input source ${inps.train} is empty.")
        System.exit(-1)
      }
      processNext()
    }

    case "eval" => {
      // Prequential evaluation of a given theory
      ///*
      logger.info(s"Performing prequential Evaluation of theory from ${inps.evalth}")
      (1 to repeatFor) foreach { _ =>
        this.data = getTrainData
        while (data.hasNext) {
          evaluate(data.next(), inps.evalth)
          logger.info(currentError)
        }
      }
      logger.info(s"Prequential error vector:\n${prequentialError.map(x => x.toDouble)}")
      logger.info(s"Prequential error vector (Accumulated Error):\n${prequentialError.scanLeft(0.0)(_ + _).tail}")
      //*/

      // This is evaluation on a test set, just comment-out prequential, uncomment this.
      /*
      val testData = testingDataFunction(testingDataOptions)

      val (tps,fps,fns,precision,recall,fscore) = crossVal(Theory(), data=testData, handCraftedTheoryFile = inps.evalth, globals = inps.globals, inps = inps)

      logger.info(s"\ntps: $tps\nfps: $fps\nfns: " + s"$fns\nprecision: $precision\nrecall: $recall\nf-score: $fscore)")
      */
      context.system.terminate()
    }

    // Use a hand-crafted theory for sequential prediction. This updates the rule weights after each round,
    // but it does not mess with the structure of the rules.
    case "predict" => {
      def matches(p: Regex, str: String) = p.pattern.matcher(str).matches
      val rules = scala.io.Source.fromFile(inps.evalth).getLines.toList.filter(line => !matches( """""".r, line) && !line.startsWith("%"))
      val rulesParsed = rules.map(r => Clause.parse(r))
      println(rulesParsed)
      (1 to repeatFor) foreach { _ =>

        this.data = getTrainData
        while (data.hasNext) {

          val batch = getNextBatch(lleNoise = false)

          logger.info(s"Prosessing $batchCounter")

          evaluateTest_NEW(batch, "", false, true, Theory(rulesParsed))
        }
      }

      logger.info(s"Prequential error vector:\n${prequentialError.map(x => x.toDouble)}")
      logger.info(s"\nPrequential error vector (Accumulated Error):\n${prequentialError.scanLeft(0.0)(_ + _).tail}")
      /*
      logger.info(s"\nTrue labels:\n$trueLabels")
      logger.info(s"\nInitiation Weight Sums:\n$initWeightSums")
      logger.info(s"\nNo Initiation Weight Sums:\n$nonInitWeightSums")
      logger.info(s"\nTermination Weight Sums:\n$TermWeightSums")
      logger.info(s"\nNon Termination Weight Sums:\n$monTermWeightSums")
      logger.info(s"\nPredict Initiation Weight Sums:\n$predictInitWeightSums")
      logger.info(s"\nPredict Termination Weight Sums:\n$predictTermWeightSums")
      logger.info(s"\nInertia Weight Sums:\n$inertWeightSums")
      logger.info(s"\nHolds Weight Sums:\n$prodictHoldsWeightSums")
      */

      //logger.info(s"\nTrue labels:\n$trueLabels")

      ///*
      utils.plotting.PlotTest2.plotResults("/home/nkatz/Desktop/", "results",
        trueLabels.toVector, initWeightSums.toVector, nonInitWeightSums.toVector, TermWeightSums.toVector,
        monTermWeightSums.toVector, predictInitWeightSums.toVector, predictTermWeightSums.toVector,
        inertWeightSums.toVector, prodictHoldsWeightSums.toVector)
      //*/

      context.system.terminate()
    }

    case p: FinishedBatchMsg => {
      responseCounter -= 1
      if (p.targetClass == "") responses += ("theory-no-ec" -> p) else responses += (p.targetClass -> p)
      if (responseCounter == 0) {

        processedBatches += 1

        // General case first (no event calculus)
        if (responses.keySet.size == 1) {
          val r = responses("theory-no-ec")
          this.theory = List(r.theory)
          this.totalBatchProcessingTime += r.BatchProcessingTime
          this.totalCompressRulesTime += r.compressRulesTime
          this.totalExpandRulesTime += r.expandRulesTime
          this.totalNewRuleGenerationTime += r.newRuleGenerationTime
          this.totalNewRuleTestTime += r.newRuleTestTime
          this.totalRuleScoringTime += r.ruleScoringTime
        } else {
          val ir = responses("initiated")
          val tr = responses("terminated")
          val newInitTheory = ir.theory
          val newTermTheory = tr.theory
          this.theory = List(newInitTheory, newTermTheory)
          this.totalBatchProcessingTime += math.max(ir.BatchProcessingTime, tr.BatchProcessingTime)
          this.totalCompressRulesTime += math.max(ir.compressRulesTime, tr.compressRulesTime)
          this.totalExpandRulesTime += math.max(ir.expandRulesTime, tr.expandRulesTime)
          this.totalNewRuleGenerationTime += math.max(ir.newRuleGenerationTime, tr.newRuleGenerationTime)
          this.totalNewRuleTestTime += math.max(ir.newRuleTestTime, tr.newRuleTestTime)
          this.totalRuleScoringTime += math.max(ir.ruleScoringTime, tr.ruleScoringTime)
        }
        //logger.info(currentError)
        // reset these before processing a new batch
        responseCounter = workers.length
        responses.clear()
        processNext()
      }
    }
  }


  var processedBatches = 0

  /*
  * Performs online evaluation and sends the next batch to the worker(s) for processing.
  *
  * */
  private def processNext() = {

    val nextBatch = getNextBatch(lleNoise = false)

    logger.info(s"Processing batch $batchCounter")

    exampleCounter += inps.chunkSize

    if (nextBatch.isEmpty) {
      logger.info(s"Finished the data.")
      if (this.repeatFor > 0) {
        logger.info(s"Starting new iteration.")
        self ! "start"
      } else if (this.repeatFor == 0) {

        endTime = System.nanoTime()
        logger.info("Done.")
        workers foreach(w => w ! PoisonPill)
        wrapUp()
        context.system.terminate()

      } else {
        throw new RuntimeException("This should never have happened (repeatfor is now negative?)")
      }
    } else {

      evaluate(nextBatch)

      //evaluateTest(nextBatch)

      //evaluateTest_NEW(nextBatch)

      //evaluateTest_NEW_EXPAND_WHEN_NEEDED(nextBatch)

      if (this.workers.length > 1) { // we're learning with the Event Calculus.
        val msg1 = new ProcessBatchMsg(theory.head, nextBatch, "initiated")
        val msg2 = new ProcessBatchMsg(theory.tail.head, nextBatch, "terminated")
        workers.head ! msg1
        workers.tail.head ! msg2
      } else { // We're learning without the Event Calculus.
        workers.head ! new ProcessBatchMsg(theory.head, nextBatch)
      }
    }
  }

  /* Finished. Just show results and shut down */
  def wrapUp(): Unit = {
    val merged = {
      if (theory.length == 1) {
        theory.head
      } else {
        Theory(theory.head.clauses ++ theory.tail.head.clauses)
      }
    }

    val theorySize = merged.clauses.foldLeft(0)((x,y) => x + y.body.length + 1)
    val totalRunningTime = (endTime - startTime)/1000000000.0
    val totalTrainingTime = totalBatchProcessingTime

    logger.info(s"\nAll rules found (non-pruned, non-compressed):\n ${merged.showWithStats}")

    val pruned = Theory(merged.clauses.filter(_.score >= inps.pruneThreshold))

    /* THIS MAY TAKE TOO LONG FOR LARGE AND COMPLEX THEORIES!! */
    logger.info("Compressing theory...")
    val pruned_ = Theory(LogicUtils.compressTheory(pruned.clauses))

    logger.info(s"\nFinal Pruned theory found:\n ${pruned_.showWithStats}")
    logger.info(s"Theory size: $theorySize")
    logger.info(s"Total running time: $totalTrainingTime")
    logger.info(s"Total batch processing time: $totalRunningTime")
    logger.info(s"Total rule scoring time: $totalRuleScoringTime")
    logger.info(s"Total rule expansion time: $totalExpandRulesTime")
    logger.info(s"Total rule compression time: $totalCompressRulesTime")
    logger.info(s"Total testing for new rule generation time: $totalNewRuleTestTime")
    logger.info(s"Total new rule generation  time: $totalNewRuleGenerationTime")
    logger.info(s"Total prediction & weights update time: $totalWeightsUpdateTime")
    logger.info(s"Total groundings computation time: $totalgroundingsTime")

    logger.info(s"Prequential error vector:\n${prequentialError.map(x => x.toDouble)}")
    logger.info(s"Prequential error vector (Accumulated Error):\n${prequentialError.scanLeft(0.0)(_ + _).tail}")
    logger.info(s"Prequential F1-score:\n$runningF1Score")
    logger.info(s"Total TPs: $TPs, total FPs: $FPs, total FNs: $FNs")

    if (this.writeExprmtResultsTo != "") {
      // Just for quick and dirty experiments
      val x = prequentialError.scanLeft(0.0)(_ + _).tail.toString()
      Utils.writeToFile(new File(this.writeExprmtResultsTo), "append") { p => List(x).foreach(p.println) }
    }

    //logger.info(s"Total TPs: ${totalTPs.size}, Total FPs: ${totalFPs.size}, Total FNs: ${totalFNs.size}")

    if (trainingDataOptions != testingDataOptions) {

      //logger.info("Evaluating on the test set")

      val testData = testingDataFunction(testingDataOptions)

      // Prequential eval on the test set (without weights update at each step).
      logger.info("Evaluating on the test set with the theory found so far (no weights update at each step, no structure updates).")
      prequentialError = Vector[Double]()
      totalTPs = Set[String]()
      totalFPs = Set[String]()
      totalFNs = Set[String]()

      // This includes the refinements in the final theory
      // Comment it out to test with the final theory
      ///*
      val predictWith = getFinalTheory(theory, useAvgWeights = true, logger)

      val newInit = predictWith._1
      val newTerm = predictWith._2

      theory = List(Theory(newInit), Theory(newTerm))
      //*/

      testData foreach { batch =>
        evaluateTest_NEW(batch, testOnly = true)
      }

      logger.info(s"Prequential error on test set:\n${prequentialError.mkString(",")}")
      logger.info(s"Prequential error vector on test set (Accumulated Error):\n${prequentialError.scanLeft(0.0)(_ + _).tail}")
      logger.info(s"Evaluation on the test set\ntps: ${totalTPs.size}\nfps: ${totalFPs.size}\nfns: ${totalFNs.size}")

      // just for quick and dirty experiments
      if (this.writeExprmtResultsTo != "") {
        val x = s"tps: ${totalTPs.size}\nfps: ${totalFPs.size}\nfns: ${totalFNs.size}\n\n"
        Utils.writeToFile(new File(this.writeExprmtResultsTo), "append") { p => List(x).foreach(p.println) }
      }

      logger.info(s"Total prediction & weights update time: $totalWeightsUpdateTime")
      logger.info(s"Total groundings computation time: $totalgroundingsTime")
      logger.info(s"Total per-rule prediction time (combining rule's sub-experts' predictions): $totalPredictionTime")
    }

    //val (tps,fps,fns,precision,recall,fscore) = crossVal(pruned_, data=testData, globals = inps.globals, inps = inps)

    //logger.info(s"\ntps: $tps\nfps: $fps\nfns: " + s"$fns\nprecision: $precision\nrecall: $recall\nf-score: $fscore)")
  }



  var TPs = 0
  var FPs = 0
  var FNs = 0

  var runningF1Score = Vector.empty[Double]

  def evaluate(batch: Example, inputTheoryFile: String = ""): Unit = {

    if (inps.prequential) {
      if (withec) {
        val (init, term) = (theory.head, theory.tail.head)

        //val merged = Theory( (init.clauses ++ term.clauses).filter(p => p.body.length >= 1 && p.seenExmplsNum > 5000 && p.score > 0.7) )

        //val merged = Theory( (init.clauses ++ term.clauses).filter(p => p.body.length >= 1 && p.score > 0.9) )

        val merged = Theory( init.clauses.filter(p => p.precision >= inps.pruneThreshold) ++ term.clauses.filter(p => p.recall >= inps.pruneThreshold) )

        val (tps, fps, fns, precision, recall, fscore) = eval(merged, batch, inps)

        // I think this is wrong, the correct error is the number of mistakes (fps+fns)
        //currentError = s"TPs: $tps, FPs: $fps, FNs: $fns, error (|true state| - |inferred state|): ${math.abs(batch.annotation.toSet.size - (tps+fps))}"

        val error = (fps+fns).toDouble

        TPs += tps
        FPs += fps
        FNs += fns

        val currentPrecision = TPs.toDouble/(TPs+FPs)
        val currentRecall = TPs.toDouble/(TPs+FNs)
        val _currentF1Score = 2*currentPrecision*currentRecall/(currentPrecision+currentRecall)
        val currentF1Score = if (_currentF1Score.isNaN) 0.0 else _currentF1Score
        runningF1Score = runningF1Score :+ currentF1Score

        currentError = s"Number of mistakes (FPs+FNs) "
        this.prequentialError = this.prequentialError :+ error

        println(s"time, scoring theory size, error: ${batch.time}, ${merged.size}, $error")
        println(this.prequentialError)

      }
    }

    // TODO :
    // Implement holdout evaluation.

    if (inps.holdout != 0) {

    }
  }




  private def getMergedTheory(testOnly: Boolean) = {
    if (withec) {
      val (init, term) = (theory.head, theory.tail.head)
      val _merged = Theory(init.clauses ++ term.clauses)

      if (testOnly) {
        _merged
      } else {
        _merged.clauses foreach (rule => if (rule.refinements.isEmpty) rule.generateCandidateRefs(inps.globals))

        // Do we want to also filter(p => p.score > inps.pruneThreshold) here?
        // Do we want to compress here? Theory(LogicUtils.compressTheory(_merged.clauses))

        val mergedWithRefs = Theory(_merged.clauses ++ _merged.clauses.flatMap(_.refinements))
        //val merged = _merged
        val merged = mergedWithRefs
        merged
      }

    } else {
      Theory() /* TODO */
    }
  }

  /* This is called whenever a new rule is added due to a mistake. */
  private def addRuleAndUpdate(r: Clause, testOnly: Boolean = false) = {

    // Update the current theory
    if (withec) {
      if (r.head.functor.contains("initiated")) {
        theory = List(Theory(theory.head.clauses :+ r), theory.tail.head)
      } else if (r.head.functor.contains("terminated")) {
        theory = List(theory.head, Theory(theory.tail.head.clauses :+ r))
      } else {
        throw new RuntimeException("Error while updating current theory.")
      }
    } else {
      /* TODO */
    }

    // Update merged theory and marked-up stuff.
    val mergedNew = getMergedTheory(testOnly)
    val markedNew = marked(mergedNew.clauses.toVector, inps.globals)
    val markedProgramNew = markedNew._1
    val markedMapNew = markedNew._2
    (mergedNew, markedProgramNew, markedMapNew)
  }

  /* This is called whenever we're specializing a rule due to a mistake */
  private def specializeRuleAndUpdate(topRule: Clause,
                                      refinement: Clause, testOnly: Boolean = false) = {

    val filter = (p: List[Clause]) => {
      p.foldLeft(List[Clause]()) { (x, y) =>
        if (!topRule.equals(y)) {
          x :+ y
        } else {
          x
        }
      }
    }

    // Update the current theory
    val oldInit = theory.head.clauses
    val oldTerm = theory.tail.head.clauses
    if (withec) {
      if (topRule.head.functor.contains("initiated")) {
        val newInit = filter(oldInit) :+ refinement
        theory = List(Theory(newInit), Theory(oldTerm))
        showInfo(topRule, refinement)
      } else if (topRule.head.functor.contains("terminated")) {
        val newTerm = filter(oldTerm) :+ refinement
        theory = List(Theory(oldInit), Theory(newTerm))
        showInfo(topRule, refinement)
      } else {
        throw new RuntimeException("Error while updating current theory.")
      }
    } else {
      /* TODO */
    }
    // Update merged theory and marked-up stuff.
    val mergedNew = getMergedTheory(testOnly)
    val markedNew = marked(mergedNew.clauses.toVector, inps.globals)
    val markedProgramNew = markedNew._1
    val markedMapNew = markedNew._2
    (mergedNew, markedProgramNew, markedMapNew)
  }

  private def showInfo(parent: Clause, child: Clause) = {

    logger.info(s"\nRule (id: ${parent.##} | score: ${parent.score} | tps: ${parent.tps} fps: ${parent.fps} " +
      s"fns: ${parent.fns} | ExpertWeight: ${parent.w_pos} " +
      s"AvgExpertWeight: ${parent.avgWeight})\n${parent.tostring}\nwas refined to" +
      s"(id: ${child.##} | score: ${child.score} | tps: ${child.tps} fps: ${child.fps} fns: ${child.fns} | " +
      s"ExpertWeight: ${child.w_pos} AvgExpertWeight: ${child.avgWeight})\n${child.tostring}")

  }






  def evaluateTest_NEW(batch: Example, inputTheoryFile: String = "",
                       testOnly: Boolean = false, weightsOnly: Boolean = false, inputTheory: Theory = Theory()) = {

    if (withec) {

      var merged = if (inputTheory == Theory()) getMergedTheory(testOnly) else inputTheory

      //logger.info(s"Predicting with ${merged.tostring}")

      // just for debugging
      val weightsBefore = merged.clauses.map(x => x.w_pos)
      // just for debugging
      val inertiaBefore = inertiaExpert.map(x => x)

      val _marked = marked(merged.clauses.toVector, inps.globals)
      var markedProgram = _marked._1
      var markedMap = _marked._2
      val e = (batch.annotationASP ++ batch.narrativeASP).mkString("\n")

      val trueAtoms = batch.annotation.toSet

      var inferredAtoms = (Set[String](), Set[String](), Set[String]())

      // this is to be set to the time the previous iteration stopped at.
      // It was supposed to be used for removing already seen stuff from the batch
      // whenever we make a mistake and start computing groundings all over again, but
      // I haven't done that yet.
      var processedUntil = 0
      var finishedBatch = false

      var alreadyProcessedAtoms = Set.empty[String]

      while(!finishedBatch) {

        val groundingsMapTimed = Utils.time{
          computeRuleGroundings(inps, markedProgram, markedMap, e, trueAtoms)
        }

        val groundingsMap = groundingsMapTimed._1._1
        val times = groundingsMapTimed._1._2
        val groundingsTime = groundingsMapTimed._2

        totalgroundingsTime += groundingsTime

        // We sort the groundings map by the time-stamp of each inferred holdsAt atom in ascending order.
        // For each holdsAt atom we calculate if it should actually be inferred, based on the weights
        // of the rules that initiate or terminate it. In this process, the weights of the rules are
        // updated based on whether the atom is mistakenly/correctly predicted and whether each individual
        // rule mistakenly/correctly predicts it. Sorting the inferred atoms and iterating over them is necessary
        // so as to promote/penalize the rule weights correctly after each mistake.
        val sorted = groundingsMap.map { entry =>
          val parsed = Literal.parse(entry._1)
          val time = parsed.terms.tail.head.name.toInt
          ((entry._1, time), entry._2)
        }.toVector.sortBy(x => x._1._2) // sort by time

        val predictAndUpdateTimed = Utils.time {
          breakable {
            sorted foreach { y =>
              val (currentAtom, currentTime) = (y._1._1, y._1._2)

              if (!alreadyProcessedAtoms.contains(currentAtom)) {
                val parsed = Literal.parse(currentAtom)
                val currentFluent = parsed.terms.head.tostring
                val (initiatedBy, terminatedBy) = (y._2._1, y._2._2)

                val initWeightSum = if (initiatedBy.nonEmpty) initiatedBy.map(x => markedMap(x).w_pos).sum else 0.0
                val termWeightSum = if (terminatedBy.nonEmpty) terminatedBy.map(x => markedMap(x).w_pos).sum else 0.0

                // only updates weights when we're not running in test mode.
                val prediction =
                  predictAndUpdate(currentAtom, currentFluent,
                    initiatedBy, terminatedBy, markedMap, testOnly, trueAtoms, batch)

                //val prediction = _prediction._1

                prediction match {
                  case "TP" => inferredAtoms = (inferredAtoms._1 + currentAtom, inferredAtoms._2, inferredAtoms._3)
                  case "FP" => inferredAtoms = (inferredAtoms._1, inferredAtoms._2 + currentAtom, inferredAtoms._3)
                  case "FN" => inferredAtoms = (inferredAtoms._1, inferredAtoms._2, inferredAtoms._3 + currentAtom)
                  case "TN" => // do nothing
                  case _ => throw new RuntimeException("Unexpected response from predictAndUpdate")
                }

                if (! testOnly && ! weightsOnly) {

                  if (prediction == "FP" && terminatedBy.isEmpty) {

                    // Let's try adding a new termination expert only when there is no other termination expert that fires.
                    // Else, let it fix the mistakes in future rounds by increasing the weights of firing terminating experts.

                    // Generate a new termination rule from the point where we currently err.
                    // This rule will be used for fixing the mistake in the next round.

                    // This most probably results in over-training. It increases weights too much and the new rule dominates.
                    //val totalWeight = inertiaExpert(currentFluent) + initWeightSum

                    val totalWeight = 1.0

                    val newTerminationRule = generateNewExpert(batch, currentAtom, inps.globals, "terminatedAt", totalWeight)

                    if (!newTerminationRule.equals(Clause.empty)) {

                      logger.info(s"Generated new termination rule in response to FP atom: $currentAtom")
                      // Since neither the new termination rule (empty-bodied), nor its refinements fire,
                      // therefore, they do not contribute to the FP, increase their weights further
                      increaseWeights(newTerminationRule.refinements :+ newTerminationRule, learningRate)

                      // Finally, add the new termination rule to the current theory.
                      val update = addRuleAndUpdate(newTerminationRule)
                      merged = update._1
                      markedProgram = update._2
                      markedMap = update._3
                      // do it here, cause it won't be set otherwise due to the break.
                      alreadyProcessedAtoms = alreadyProcessedAtoms + currentAtom
                      break
                    } else {
                      logger.info(s"At batch $batchCounter: Failed to generate bottom rule from FP mistake with atom: $currentAtom")
                    }
                  }

                  //if (prediction == "FN" && initiatedBy.isEmpty && getInertiaExpertPrediction(currentFluent) == 0.0) {
                  if (prediction == "FN" && initiatedBy.isEmpty) {
                    //val newInitiationRule = generateNewExpert(batch, currentAtom, inps.globals, "initiatedAt", termWeightSum)
                    // Don't give the total weight of the termination part. It's dangerous
                    // (e.g. if the termination part new rules get the total weight of 0.0, and the TN is never fixed!)
                    // and it's also wrong. You just over-train to get rid of a few mistakes!
                    val newInitiationRule = generateNewExpert(batch, currentAtom, inps.globals, "initiatedAt", 1.0)
                    if (!newInitiationRule.equals(Clause.empty)) {
                      logger.info(s"Generated new initiation rule in response to FN atom: $currentAtom")
                      val update = addRuleAndUpdate(newInitiationRule)
                      merged = update._1
                      markedProgram = update._2
                      markedMap = update._3
                      // do it here, cause it won't be set otherwise due to the break.
                      alreadyProcessedAtoms = alreadyProcessedAtoms + currentAtom
                      break
                    } else {
                      logger.info(s"At batch $batchCounter: Failed to generate bottom rule from FN mistake with atom: $currentAtom")
                    }
                  }
                }
              }
              alreadyProcessedAtoms = alreadyProcessedAtoms + currentAtom
            }
            finishedBatch = true
          }
        }
        totalWeightsUpdateTime += predictAndUpdateTimed._2
      }

      val (tps, fps, fns) = (inferredAtoms._1, inferredAtoms._2, inferredAtoms._3)

      val (fpsNumber, currentFNsNumber) = (fps.size, fns.size)

      // All atoms in tps are certainly true positives.
      // But we need to account for the real true atoms which are not in there.
      val restFNs = trueAtoms.diff(tps).filter(!fns.contains(_))

      if (restFNs.nonEmpty) throw new RuntimeException("FUUUUUUUUUUUUCK!!!!!")

      val restFNsNumber = restFNs.size

      var trueFNsNumber = currentFNsNumber + restFNsNumber

      // Ugly (AND DANGEROUS) hack to avoid counting as mistakes the holdsAt/2 atoms at the first time point of an interval
      if (trueFNsNumber == 2) trueFNsNumber = 0

      // We have gathered the FNs that have not been inferred, but we need to add the rest of them in the global counter
      totalFNs = totalFNs ++ restFNs

      // Just for debugging.
      val weightsAfter = merged.clauses.map(x => x.w_pos)
      //just for debugging
      val inertiaAfter = inertiaExpert.map(x => x)

      prequentialError = prequentialError :+ (fpsNumber + trueFNsNumber).toDouble

      if (fpsNumber + trueFNsNumber > 0) {
        logger.info(s"\nMade mistakes: FPs: $fpsNumber, " +
          s"FNs: $trueFNsNumber.\nWeights before: $weightsBefore\nWeights after: $weightsAfter\nInertia Before: " +
          s"$inertiaBefore\nInertia after: $inertiaAfter")//\nPredicted with:\n${merged.showWithStats}")
      }
    } else { // No Event Calculus. We'll see what we'll do with that.

    }

  }







  def evaluateTest_NEW_EXPAND_WHEN_NEEDED(batch: Example, inputTheoryFile: String = "",
                       testOnly: Boolean = false, weightsOnly: Boolean = false, inputTheory: Theory = Theory()) = {

    if (withec) {

      var merged = if (inputTheory == Theory()) getMergedTheory(testOnly) else inputTheory

      // just for debugging
      val weightsBefore = merged.clauses.map(x => x.w_pos)
      // just for debugging
      val inertiaBefore = inertiaExpert.map(x => x)

      val _marked = marked(merged.clauses.toVector, inps.globals)
      var markedProgram = _marked._1
      var markedMap = _marked._2
      val e = (batch.annotationASP ++ batch.narrativeASP).mkString("\n")

      val trueAtoms = batch.annotation.toSet

      var inferredAtoms = (Set[String](), Set[String](), Set[String]())

      // this is to be set to the time the previous iteration stopped at.
      // It was supposed to be used for removing already seen stuff from the batch
      // whenever we make a mistake and start computing groundings all over again, but
      // I haven't done that yet.
      var processedUntil = 0
      var finishedBatch = false

      var alreadyProcessedAtoms = Set.empty[String]



      while(!finishedBatch) {

        val groundingsMapTimed = Utils.time{
          computeRuleGroundings(inps, markedProgram, markedMap, e, trueAtoms)
        }

        val groundingsMap = groundingsMapTimed._1._1
        val times = groundingsMapTimed._1._2
        val groundingsTime = groundingsMapTimed._2

        totalgroundingsTime += groundingsTime

        // We sort the groundings map by the time-stamp of each inferred holdsAt atom in ascending order.
        // For each holdsAt atom we calculate if it should actually be inferred, based on the weights
        // of the rules that initiate or terminate it. In this process, the weights of the rules are
        // updated based on whether the atom is mistakenly/correctly predicted and whether each individual
        // rule mistakenly/correctly predicts it. Sorting the inferred atoms and iterating over them is necessary
        // so as to promote/penalize the rule weights correctly after each mistake.
        val sorted = groundingsMap.map { entry =>
          val parsed = Literal.parse(entry._1)
          val time = parsed.terms.tail.head.name.toInt
          ((entry._1, time), entry._2)
        }.toVector.sortBy(x => x._1._2) // sort by time

        val predictAndUpdateTimed = Utils.time {
          breakable {
            sorted foreach { y =>
              val (currentAtom, currentTime) = (y._1._1, y._1._2)

              if (!alreadyProcessedAtoms.contains(currentAtom)) {
                val parsed = Literal.parse(currentAtom)
                val currentFluent = parsed.terms.head.tostring

                val (initiatedBy, terminatedBy) = (y._2._1, y._2._2)

                // This is also calculated at predictAndUpdate, we need to factor it out.
                // Calculate it here (because it is needed here) and pass it to predictAndUpdate
                // to avoid doing it twice.
                ///*
                val nonFiringInitRules =
                  markedMap.filter(x =>
                    x._2.head.functor.contains("initiated") && !initiatedBy.contains(x._1))
                //*/

                // This is also calculated at predictAndUpdate, we need to factor it out.
                // Calculate it here (because it is needed here) and pass it to predictAndUpdate
                // to avoid doing it twice.
                ///*
                val nonFiringTermRules =
                  markedMap.filter(x =>
                    x._2.head.functor.contains("terminated") && !terminatedBy.toSet.contains(x._1))
                //*/

                val initWeightSum = if (initiatedBy.nonEmpty) initiatedBy.map(x => markedMap(x).w_pos).sum else 0.0
                val termWeightSum = if (terminatedBy.nonEmpty) terminatedBy.map(x => markedMap(x).w_pos).sum else 0.0

                // only updates weights when we're not running in test mode.
                val prediction =
                  predictAndUpdate(currentAtom, currentFluent,
                    initiatedBy, terminatedBy, markedMap, testOnly, trueAtoms, batch)

                prediction match {
                  case "TP" => inferredAtoms = (inferredAtoms._1 + currentAtom, inferredAtoms._2, inferredAtoms._3)
                  case "FP" => inferredAtoms = (inferredAtoms._1, inferredAtoms._2 + currentAtom, inferredAtoms._3)
                  case "FN" => inferredAtoms = (inferredAtoms._1, inferredAtoms._2, inferredAtoms._3 + currentAtom)
                  case "TN" => // do nothing
                  case _ => throw new RuntimeException("Unexpected response from predictAndUpdate")
                }

                if (! testOnly && ! weightsOnly) {

                  if (prediction == "FP") {

                    if (terminatedBy.isEmpty) {
                      // Let's try adding a new termination expert only when there is no other termination expert that fires.
                      // Else, let it fix the mistakes in future rounds by increasing the weights of firing terminating experts.

                      // Generate a new termination rule from the point where we currently err.
                      // This rule will be used for fixing the mistake in the next round.

                      // This most probably results in over-training. It increases weights too much and the new rule dominates.
                      //val totalWeight = inertiaExpert(currentFluent) + initWeightSum

                      val totalWeight = 1.0

                      val newTerminationRule = generateNewExpert(batch, currentAtom, inps.globals, "terminatedAt", totalWeight)

                      if (!newTerminationRule.equals(Clause.empty)) {

                        logger.info(s"Generated new termination rule in response to FP atom: $currentAtom")
                        // Since neither the new termination rule (empty-bodied), nor its refinements fire,
                        // therefore, they do not contribute to the FP, increase their weights further
                        // NO, WE DO NOT INCREASE WEIGHTS OF NON-FIRING RULES!!!
                        //increaseWeights(newTerminationRule.refinements :+ newTerminationRule, learningRate)

                        // Finally, add the new termination rule to the current theory.
                        val update = addRuleAndUpdate(newTerminationRule)
                        merged = update._1
                        markedProgram = update._2
                        markedMap = update._3
                        // do it here, cause it won't be set otherwise due to the break.
                        alreadyProcessedAtoms = alreadyProcessedAtoms + currentAtom
                        break
                      } else {
                        logger.info(s"At batch $batchCounter: Failed to generate bottom rule from FP mistake with atom: $currentAtom")
                      }

                    } else { // We do have firing termination rules
                      // Specialize a firing initiation rule. If no firing initiation rule exists,
                      // therefore the FP is due to inertia, just let the inertia weight degrade, until
                      // the termination rules take over the majority (note that we do have firing termination rules here,
                      // so there are reasons to believe that we'll have such rules in the up-coming rounds).
                      if (initiatedBy.nonEmpty) {
                        // Note that we'll most certainly have a top-rule that fires: for every
                        // refinement that fires, its parent rule must fire as well. Therefore, if
                        // initiatedBy is non empty, at least some of the rules in there must be top rules.

                        val rulesToSpecialize =
                          // This is the first minor difference with the piece of code
                          // for specializing termination rules (below). Here We select the
                          // rules from the initiation part of the theory, below from the termination
                          theory.head.clauses.
                            filter(x => initiatedBy.toSet.contains(x.##.toString))

                        var performedSpecialization = false

                        rulesToSpecialize foreach { ruleToSpecialize =>
                          // Find suitable refinements, i.e refinements that DO NOT fire
                          // w.r.t. the current FP atom.
                          val suitableRefs =
                            // Here is the second difference. We use nonFiringInitRules here.
                            // It's really stupid to have this code duplicated like that.
                            // Fuck your quick & dirty bullshit.
                            ruleToSpecialize.refinements.
                              filter(r => nonFiringInitRules.keySet.contains(r.##.toString)).
                              filter(s => s.score > ruleToSpecialize.score).
                              filter(r => !theory.head.clauses.exists(r1 => r1.thetaSubsumes(r) && r.thetaSubsumes(r1))).
                              sortBy { x => (- x.w_pos, - x.score, x.body.length+1) }

                          if (suitableRefs.nonEmpty) {
                            performedSpecialization = true
                            val bestRefinement = suitableRefs.head
                            if (bestRefinement.refinements.isEmpty) bestRefinement.generateCandidateRefs(inps.globals)
                            val update = specializeRuleAndUpdate(ruleToSpecialize, bestRefinement)
                            merged = update._1
                            markedProgram = update._2
                            markedMap = update._3
                            // do it here, cause it won't be set otherwise due to the break.
                            alreadyProcessedAtoms = alreadyProcessedAtoms + currentAtom
                            break
                          }
                        }

                        if (performedSpecialization) break


                      }
                    }

                  }

                  if (prediction == "FN") {
                    //if (initiatedBy.isEmpty || (nonFiringInitRules.values.map(_.w).sum > initWeightSum) ) {
                    if (initiatedBy.isEmpty) {
                      //val newInitiationRule = generateNewExpert(batch, currentAtom, inps.globals, "initiatedAt", termWeightSum)
                      // Don't give the total weight of the termination part. It's dangerous
                      // (e.g. if the termination part new rules get the total weight of 0.0, and the TN is never fixed!)
                      // and it's also wrong. You just over-train to get rid of a few mistakes!
                      val newInitiationRule = generateNewExpert(batch, currentAtom, inps.globals, "initiatedAt", 1.0)
                      if (!newInitiationRule.equals(Clause.empty)) {
                        logger.info(s"Generated new initiation rule in response to FN atom: $currentAtom")
                        val update = addRuleAndUpdate(newInitiationRule)
                        merged = update._1
                        markedProgram = update._2
                        markedMap = update._3
                        // do it here, cause it won't be set otherwise due to the break.
                        alreadyProcessedAtoms = alreadyProcessedAtoms + currentAtom
                        break
                      } else {
                        logger.info(s"At batch $batchCounter: Failed to generate bottom rule from FN mistake with atom: $currentAtom")
                      }

                      /* THE CODE BELOW IS THE SAME AS ABOVE. FACTOR IT OUT TO A FUNCTION. */

                    } else {
                      // Them the FN is due to over-weighted firing termination rules. Specialize one.
                      if (terminatedBy.nonEmpty) {

                        val termRulesToSpecialize =
                          theory.tail.head.clauses.
                            filter(x => terminatedBy.toSet.contains(x.##.toString))

                        var performedSpecialization = false

                        termRulesToSpecialize foreach { ruleToSpecialize =>
                          // Find suitable refinements, i.e refinements that DO NOT fire
                          // w.r.t. the current FN atom.
                          val suitableRefs =
                            ruleToSpecialize.refinements.
                              filter(r => nonFiringTermRules.keySet.contains(r.##.toString)).
                              filter(s => s.score > ruleToSpecialize.score).
                              filter(r => !theory.tail.head.clauses.exists(r1 => r1.thetaSubsumes(r) && r.thetaSubsumes(r1))).
                              sortBy { x => (- x.w_pos, - x.score, x.body.length+1) }

                          if (suitableRefs.nonEmpty) {
                            performedSpecialization = true
                            val bestRefinement = suitableRefs.head
                            if (bestRefinement.refinements.isEmpty) bestRefinement.generateCandidateRefs(inps.globals)
                            val update = specializeRuleAndUpdate(ruleToSpecialize, bestRefinement)
                            merged = update._1
                            markedProgram = update._2
                            markedMap = update._3
                            // do it here, cause it won't be set otherwise due to the break.
                            alreadyProcessedAtoms = alreadyProcessedAtoms + currentAtom
                            break
                          }
                        }
                        if (performedSpecialization) break
                      } else {
                        // This would be a problem, certainly something that is worth looking into.
                        // We have an FN, with firing initiation rules, but not firing termination ones.
                        // UPDATE: It's ok, it can happen because the non-firing weight is greater then the firing weight.
                        /*
                        throw new RuntimeException(s"We have an FN atom, which is " +
                          s"initiated by some rules and terminated by NO rules. It's worth finding out how this happens!\nBatch" +
                          s" cointer: $batchCounter, atom: $currentAtom")
                        */
                      }
                    }
                  }
                }
              }
              alreadyProcessedAtoms = alreadyProcessedAtoms + currentAtom
            }
            finishedBatch = true
          }
        }
        totalWeightsUpdateTime += predictAndUpdateTimed._2
      }

      val (tps, fps, fns) = (inferredAtoms._1, inferredAtoms._2, inferredAtoms._3)

      val (fpsNumber, currentFNsNumber) = (fps.size, fns.size)

      // All atoms in tps are certainly true positives.
      // But we need to account for the real true atoms which are not in there.
      val restFNs = trueAtoms.diff(tps).filter(!fns.contains(_))

      if (restFNs.nonEmpty) throw new RuntimeException("FUUUUUUUUUUUUCK!!!!!")

      val restFNsNumber = restFNs.size

      var trueFNsNumber = currentFNsNumber + restFNsNumber

      // Ugly (AND DANGEROUS) hack to avoid counting as mistakes the holdsAt/2 atoms at the first time point of an interval
      //if (trueFNsNumber == 2) trueFNsNumber = 0

      // We have gathered the FNs that have not been inferred, but we need to add the rest of them in the global counter
      totalFNs = totalFNs ++ restFNs

      // Just for debugging.
      val weightsAfter = merged.clauses.map(x => x.w_pos)
      //just for debugging
      val inertiaAfter = inertiaExpert.map(x => x)

      prequentialError = prequentialError :+ (fpsNumber + trueFNsNumber).toDouble

      if (fpsNumber + trueFNsNumber > 0) {
        logger.info(s"\nMade mistakes: FPs: $fpsNumber, " +
          s"FNs: $trueFNsNumber.\nWeights before: $weightsBefore\nWeights after: $weightsAfter\nInertia Before: " +
          s"$inertiaBefore\nInertia after: $inertiaAfter")//\nPredicted with:\n${merged.showWithStats}")
      }
    } else { // No Event Calculus. We'll see what we'll do with that.

    }

  }





















  def updateAnalyticsBuffers(atom: String, initWghtSum: Double, termWghtSum: Double,
                             nonInitWghtSum: Double, nonTermWghtSum: Double,
                             predictInitWghtSum: Double, predictTermWghtSum: Double,
                             inertWghtSum: Double, holdsWght: Double) = {

    if (atom.contains(keepStatsForFluent)) {
      initWeightSums += initWghtSum
      TermWeightSums += termWghtSum
      nonInitWeightSums += nonInitWghtSum
      monTermWeightSums += nonTermWghtSum
      predictInitWeightSums += predictInitWghtSum
      predictTermWeightSums += predictTermWghtSum
      inertWeightSums += inertWghtSum
      prodictHoldsWeightSums += holdsWght
    }
  }

  def updateTrueLabels(atom: String, value: Double) = {
    if (atom.contains(keepStatsForFluent)) {
      trueLabels += value
    }
  }



  def predictAndUpdate(currentAtom: String, currentFluent: String, init: Vector[String],
                       term: Vector[String], markedMap: scala.collection.immutable.Map[String, Clause],
                       testOnly: Boolean, trueAtoms: Set[String], batch: Example) = {

    val (initiatedBy, terminatedBy) = (init, term)

    val initWeightSum = if (initiatedBy.nonEmpty) initiatedBy.map(x => markedMap(x).w_pos).sum else 0.0
    val termWeightSum = if (terminatedBy.nonEmpty) terminatedBy.map(x => markedMap(x).w_pos).sum else 0.0

    val inertiaExpertPrediction = getInertiaExpertPrediction(currentFluent)

    val firingInitRulesIds = initiatedBy

    val nonFiringInitRules =
      markedMap.filter(x =>
        x._2.head.functor.contains("initiated") && !firingInitRulesIds.contains(x._1))

    // Use this to have all rules and their refs vote independently:
    // This was the default but does not seam reasonable.
    val predictInitiated = initWeightSum// - nonFiringInitRules.values.map(_.w).sum

    // Use this to have one prediction per top rule, resulting by combing the
    // opinions of the rule's sub-expert committee (its specializations)
    /*
    val predictInitiatedTimed = Utils.time{
      val individualPredictions =
        theory.head.clauses.map( rule => getRulePrediction(rule, firingInitRulesIds, nonFiringInitRules.keys.toVector) )
      individualPredictions.sum
    }
    val predictInitiated = predictInitiatedTimed._1
    totalPredictionTime += predictInitiatedTimed._2
    */

    // Use this to have one prediction per top rule.
    // The best (based on current weight) between the top-rule's own
    // prediction and the prediction of the rule's best sub-expert:
    /*
    val predictInitiatedTimed = Utils.time{
      val individualPredictions =
        theory.head.clauses.map( rule => getRulePrediction1(rule, firingInitRulesIds, nonFiringInitRules.keys.toVector) )
      individualPredictions.sum
    }
    val predictInitiated = predictInitiatedTimed._1
    totalPredictionTime += predictInitiatedTimed._2
    */

    val firingTermRulesIds = terminatedBy

    val nonFiringTermRules =
      markedMap.filter(x =>
        x._2.head.functor.contains("terminated") && !firingTermRulesIds.toSet.contains(x._1))

    // Use this to have all rules and their refs vote independently:
    // This was the default but does not seam reasonable.
    val predictTerminated = termWeightSum// - nonFiringTermRules.values.map(_.w).sum

    // Use this to have one prediction per top rule, resulting by combing the
    // opinions of the rule's sub-expert committee (its specializations):
    /*
    val predictTerminatedTimed = Utils.time {
      val individualPredictions =
        theory.tail.head.clauses.map( rule => getRulePrediction(rule, firingTermRulesIds, nonFiringTermRules.keys.toVector) )
      individualPredictions.sum
    }
    val predictTerminated = predictTerminatedTimed._1
    totalPredictionTime += predictTerminatedTimed._2
    */

    // Use this to have one prediction per top rule.
    // The best (based on current weight) between the top-rule's own
    // prediction and the prediction of the rule's best sub-expert:
    /*
    val predictTerminatedTimed = Utils.time {
      val individualPredictions =
        theory.tail.head.clauses.map( rule => getRulePrediction1(rule, firingTermRulesIds, nonFiringTermRules.keys.toVector) )
      individualPredictions.sum
    }
    val predictTerminated = predictTerminatedTimed._1
    totalPredictionTime += predictTerminatedTimed._2
    */

    // WITH INERTIA
    ///*
    val _predictAtomHolds = predict(inertiaExpertPrediction, predictInitiated, predictTerminated, isStrongInertia)
    val (predictAtomHolds, holdsWeight) = (_predictAtomHolds._1, _predictAtomHolds._2)
    //*/

    // NO INERTIA
    //val _predictAtomHolds = predictInitiated - predictTerminated
    //val (predictAtomHolds, holdsWeight) = (if (_predictAtomHolds > 0) true else false, _predictAtomHolds)

    updateAnalyticsBuffers(currentAtom, initWeightSum, termWeightSum,
      nonFiringInitRules.values.map(_.w_pos).sum, nonFiringTermRules.values.map(_.w_pos).sum,
      predictInitiated, predictTerminated, inertiaExpertPrediction, holdsWeight)

    /*
    * THIS PREDICTION RULE IS WRONG:
    *
    * val holdsPredictionWeight = inertiaExpertPrediction + predictInitiated - predictTerminated
    * val predictAtomHolds = holdsPredictionWeight > 0.0
    *
    * Look what might happen:
    *
    * Made FP mistake for atom: holdsAt(meeting(id3,id1),2600).
    * Inertia weight: 0.0
    * Firing initiation rules: 0, sum of weight: 0.0
    * Non firing initiation rules: 17, sum of weight: 25.23524944624197
    * Firing termination rules: 3, sum of weight: 101.70330033914848
    * Non firing termination rules: 4, sum of weight: 135.60440045219798
    *
    * Semantically, there is no reason to predict HOLDS: The fluent does not hold by inertia, nor is it
    * initiated by any rule. But we have that predictInitiated = -25.23524944624197 and
    * predictInitiated = -33.901100113049495, because in both cases, the sum of weights of the non-firing
    * is greater than that of the firing ones. Therefore, (and since predictInitiated > 25.23524944624197) we have
    *
    * holdsPredictionWeight = 0.0 + (-25.23524944624197) - (-33.901100113049495) > 0
    *
    * and we get a wrong prediction, while there is no reason for that.
    *
    * */
    //val holdsPredictionWeight = inertiaExpertPrediction + predictInitiated - predictTerminated
    //val predictAtomHolds = holdsPredictionWeight > 0.0



    if (predictAtomHolds) {

      // If the fluent we predicted that it holds is not in the inertia expert map, add it,
      // with the weight it was predicted.
      if (!inertiaExpert.keySet.contains(currentFluent)) {

        // this is guaranteed to be positive from the prediction rule
        val holdsWeight = inertiaExpertPrediction + predictInitiated

        inertiaExpert += (currentFluent -> holdsWeight)
        //inertiaExpert += (currentFluent -> 1.0)
      }

      if (trueAtoms.contains(currentAtom)) {

        // Then it's a TP. Simply return it without updating any weights, after properly scoring the rules.

        // Update the analytics buffer for this atom
        updateTrueLabels(currentAtom, 1.0)

        updateRulesScore("TP", initiatedBy.map(x => markedMap(x)), nonFiringInitRules.values.toVector,
          terminatedBy.map(x => markedMap(x)), nonFiringTermRules.values.toVector)

        totalTPs = totalTPs + currentAtom
        "TP"

      } else {
        // Then it's an FP.

        // Update the analytics buffer for this atom
        updateTrueLabels(currentAtom, 0.0)

        // That's for debugging
        /*
        reportMistake("FP", currentAtom, inertiaExpertPrediction, initiatedBy.size,
          nonFiringInitRules.size, terminatedBy.size, nonFiringTermRules.size, initWeightSum,
          termWeightSum, nonFiringInitRules.values.map(_.w).sum, nonFiringTermRules.values.map(_.w).sum, this.logger)
        */

        totalFPs = totalFPs + currentAtom
        if (!testOnly) {

          // Decrease the weights of all rules that contribute to the FP: Rules that incorrectly initiate it.
          reduceWeights(initiatedBy, markedMap, learningRate)

          // Non-firing rules should not be touched, they should be treated as abstaining experts.
          // Increasing the weights of non-firing rules results in over-training of garbage, which dominate.
          // reduceWeights(nonFiringTermRules.keys.toVector, markedMap, learningRate)

          // Reduce the weight of the inertia expert for the particular atom, if the inertia expert predicted that it holds.
          if (inertiaExpert.keySet.contains(currentFluent)) {
            val newWeight = inertiaExpert(currentFluent) * Math.pow(Math.E, (-1.0) * learningRate)
            inertiaExpert += (currentFluent -> newWeight)
          }

          // Increase the weights of rules that can fix the mistake:
          // Rules that terminate the fluent and initiation rules that do not fire (NO!).
          increaseWeights(terminatedBy, markedMap, learningRate)

          // Non-firing rules should not be touched, they should be treated as abstaining experts.
          // Increasing the weights of non-firing rules results in over-training of garbage, which dominate.
          //increaseWeights(nonFiringInitRules.keys.toVector, markedMap, learningRate)

        }

        updateRulesScore("FP", initiatedBy.map(x => markedMap(x)), nonFiringInitRules.values.toVector,
          terminatedBy.map(x => markedMap(x)), nonFiringTermRules.values.toVector)

        "FP" // result returned to the calling method.
      }
    } else {

      // We predicted that the atom does not hold...
      if (trueAtoms.contains(currentAtom)) {

        // ...while it actually does, so we have an FN.

        // Update the analytics buffer for this atom
        updateTrueLabels(currentAtom, 1.0)

        /*
        reportMistake("FN", currentAtom, inertiaExpertPrediction, initiatedBy.size,
          nonFiringInitRules.size, terminatedBy.size, nonFiringTermRules.size, initWeightSum,
          termWeightSum, nonFiringInitRules.values.map(_.w).sum, nonFiringTermRules.values.map(_.w).sum, this.logger)
        */

        totalFNs = totalFNs + currentAtom
        if (! testOnly) {

          // Increase the weights of all rules that initiate it
          increaseWeights(initiatedBy, markedMap, learningRate)

          // and all rules that do not terminate it (NO!!)
          // Non-firing rules should not be touched, they should be treated as abstaining experts.
          // Increasing the weights of non-firing rules results in over-training of garbage, which dominate.
          //increaseWeights(nonFiringTermRules.keys.toVector, markedMap, learningRate)

          // Increase the weight of the inertia expert for that particular atom,
          // if the inertia expert predicted that it holds.
          if (inertiaExpert.keySet.contains(currentFluent)) {
            var newWeight = inertiaExpert(currentFluent) * Math.pow(Math.E, 1.0 * learningRate)
            newWeight = if (newWeight.isPosInfinity) inertiaExpert(currentFluent) else newWeight
            inertiaExpert += (currentFluent -> newWeight)
          }

          // Also, reduce the weights of all initiation rules that do not fire (NO!) and all termination rules that fire.
          //reduceWeights(nonFiringInitRules.keys.toVector, markedMap, learningRate) // No, maybe that's wrong, there's no point in penalizing a rule that does not fire.
          reduceWeights(terminatedBy, markedMap, learningRate)

        }

        updateRulesScore("FN", initiatedBy.map(x => markedMap(x)), nonFiringInitRules.values.toVector,
          terminatedBy.map(x => markedMap(x)), nonFiringTermRules.values.toVector)

        "FN" // result returned to the calling method.

      } else {
        // Then we have an atom which was erroneously inferred by the (un-weighted) rules (with ordinary logical
        // inference), but which eventually not inferred, thanks to the expert-based weighted framework. That is,
        // either the total weight of the non-firing "initiatedAt" fragment of the theory was greater than the weight
        // of the firing part, or the the total weight of the firing "terminatedAt" path of the theory was greater
        // than the weight of the "initiatedAt" fragment. In either case, the atom is eventually a TN. We don't do
        // anything with it, but we need to instruct the the inertia expert to "forget" the atom.

        // Update the analytics buffer for this atom
        updateTrueLabels(currentAtom, 0.0)

        if (inertiaExpert.keySet.contains(currentFluent)) {
          inertiaExpert -= currentFluent
        }

        updateRulesScore("TN", initiatedBy.map(x => markedMap(x)), nonFiringInitRules.values.toVector,
          terminatedBy.map(x => markedMap(x)), nonFiringTermRules.values.toVector)

        "TN"
      }
    }
  }












}

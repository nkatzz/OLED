package oled.winnow

import java.io.File

import akka.actor.{Actor, ActorRef, PoisonPill, Props}
import app.runutils.IOHandling.Source
import app.runutils.{Globals, RunningOptions}
import logic.Examples.Example
import logic._
import oled.winnow.MessageTypes.{FinishedBatchMsg, ProcessBatchMsg}
import org.slf4j.LoggerFactory
import oled.functions.SingleCoreOLEDFunctions.{crossVal, eval}

import scala.collection.mutable.Map
import AuxFuncs._
import utils.{ASP, Utils}
import utils.Implicits._

import scala.util.control.Breaks._
import scala.reflect.internal.Trees
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
  private val isStrongInertia = true
  //----------------------------------------------------------------------

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
      logger.info(s"Prequential error vector:\n${prequentialError.mkString(",")}")
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

          logger.info(s"Prosessing ${batch.time}")
          //logger.info(currentError)
          evaluateTest_NEW(batch, "", false, true, Theory(rulesParsed))
        }
      }

      logger.info(s"Prequential error vector:\n${prequentialError.mkString(",")}")
      logger.info(s"Prequential error vector (Accumulated Error):\n${prequentialError.scanLeft(0.0)(_ + _).tail}")
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

      //evaluate(nextBatch)

      //evaluateTest(nextBatch)

      evaluateTest_NEW(nextBatch)

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

    logger.info(s"Prequential error vector:\n${prequentialError.mkString(",")}")
    logger.info(s"Prequential error vector (Accumulated Error):\n${prequentialError.scanLeft(0.0)(_ + _).tail}")

    // Just for quick and dirty experiments
    val x = prequentialError.scanLeft(0.0)(_ + _).tail.toString()
    Utils.writeToFile(new File(this.writeExprmtResultsTo), "append") { p => List(x).foreach(p.println) }

    logger.info(s"Total TPs: ${totalTPs.size}, Total FPs: ${totalFPs.size}, Total FNs: ${totalFNs.size}")

    logger.info("Evaluating on the test set")

    val testData = testingDataFunction(testingDataOptions)

    if (trainingDataOptions != testingDataOptions) {
      // Prequential eval on the test set (without weights update at each step).
      logger.info("Evaluating on the test set with the theory found so far (no weights update at each step, no structure updates).")
      prequentialError = Vector[Double]()
      totalTPs = Set[String]()
      totalFPs = Set[String]()
      totalFNs = Set[String]()

      // For each rule in the theory, set its weight to its average weight, so that it uses the latter
      // for prediction during learning. Could we avoid over-training with something like that?
      ///*

      val oldInit = theory.head.clauses
      val oldTerm = theory.tail.head.clauses

      ///*
      oldInit foreach { r =>
        r.w = r.avgWeight
        r.refinements foreach( r1 => r1.w = r1.avgWeight )
      }

      oldTerm foreach { r =>
        r.w = r.avgWeight
        r.refinements foreach( r1 => r1.w = r1.avgWeight )
      }
      //*/

      val newInit = oldInit //theory.head.clauses.filter(x => x.w > 0.0 && x.w < 10.0)
      val newTerm = oldTerm //theory.tail.head.clauses.filter(x => x.w > 0.0 && x.w < 10.0)

      //theory = List(newInit, newTerm)
      //theory = List(newInit.clauses.flatMap(x => x.refinements :+ x), newTerm.clauses.flatMap(x => x.refinements :+ x))

      theory = List(newInit.clauses.flatMap(x => x.refinements :+ x).filter(x => x.w > 0.0 && x.w < 10.0),
        newTerm.clauses.flatMap(x => x.refinements :+ x).filter(x => x.w > 0.0 && x.w < 10.0))



      val (init, term) = (theory.head, theory.tail.head)
      val _merged = init.clauses ++ term.clauses

      logger.info(s"\n\n\nPerforming test with:\n\n${_merged.showWithStats}")

      testData foreach { batch =>
        evaluateTest_NEW(batch, testOnly = true)
      }
      logger.info(s"Prequential error on test set:\n${prequentialError.mkString(",")}")
      logger.info(s"Prequential error vector on test set (Accumulated Error):\n${prequentialError.scanLeft(0.0)(_ + _).tail}")
      logger.info(s"Evaluation on the test set\ntps: ${totalTPs.size}\nfps: ${totalFPs.size}\nfns: ${totalFNs.size}")

      // just for quick and dirty experiments
      val x = s"tps: ${totalTPs.size}\nfps: ${totalFPs.size}\nfns: ${totalFNs.size}\n\n"
      Utils.writeToFile(new File(this.writeExprmtResultsTo), "append") { p => List(x).foreach(p.println) }

      logger.info(s"Total prediction & weights update time: $totalWeightsUpdateTime")
      logger.info(s"Total groundings computation time: $totalgroundingsTime")
      logger.info(s"Total per-rule prediction time (combining rule's sub-experts' predictions): $totalPredictionTime")
    }

    //val (tps,fps,fns,precision,recall,fscore) = crossVal(pruned_, data=testData, globals = inps.globals, inps = inps)

    //logger.info(s"\ntps: $tps\nfps: $fps\nfns: " + s"$fns\nprecision: $precision\nrecall: $recall\nf-score: $fscore)")
  }






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
      val _merged =
        if (!testOnly) {
          Theory(init.clauses ++ term.clauses)
          //Theory( (init.clauses ++ term.clauses).map(x => x.supportSet.clauses.head) )
        } else {
          Theory(init.clauses ++ term.clauses)

          //Theory( (init.clauses ++ term.clauses).map(x => x.supportSet.clauses.head) )

          //Theory( (init.clauses ++ term.clauses).filter(p => p.score > inps.pruneThreshold) )
          //Theory( (init.clauses ++ term.clauses) ++ (init.clauses ++ term.clauses).flatMap(x => x.refinements) )
        }

      _merged.clauses foreach (rule => if (rule.refinements.isEmpty) rule.generateCandidateRefs(inps.globals))

      val mergedWithRefs = Theory(_merged.clauses ++ _merged.clauses.flatMap(_.refinements))
      //val merged = _merged

      val merged = if (testOnly) {
        val t = (_merged.clauses ++ _merged.clauses.flatMap(x => x.refinements)).filter(p => p.score > inps.pruneThreshold)
        Theory(t)
      } else {
        mergedWithRefs
      }
      //val merged = Theory(LogicUtils.compressTheory(_merged.clauses))
      Theory(merged.clauses.filter(p => p.body.nonEmpty))
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






  def evaluateTest_NEW(batch: Example, inputTheoryFile: String = "",
                       testOnly: Boolean = false, weightsOnly: Boolean = false, inputTheory: Theory = Theory()) = {

    if (batchCounter == 364) {
      val stop = "stop"
    }

    if (withec) {

      var merged = if (inputTheory == Theory()) getMergedTheory(testOnly) else inputTheory

      // just for debugging
      val weightsBefore = merged.clauses.map(x => x.w)
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

        val groundingsMap = groundingsMapTimed._1
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

                val initWeightSum = if (initiatedBy.nonEmpty) initiatedBy.map(x => markedMap(x).w).sum else 0.0
                val termWeightSum = if (terminatedBy.nonEmpty) terminatedBy.map(x => markedMap(x).w).sum else 0.0

                // only updates weights when we're not running in test mode.
                val prediction = predictAndUpdate(currentAtom, currentFluent, initiatedBy, terminatedBy, markedMap, testOnly, trueAtoms, batch)

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
      val weightsAfter = merged.clauses.map(x => x.w)
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



  def predictAndUpdate(currentAtom: String, currentFluent: String, init: Vector[String],
                       term: Vector[String], markedMap: scala.collection.immutable.Map[String, Clause],
                       testOnly: Boolean, trueAtoms: Set[String], batch: Example) = {

    val (initiatedBy, terminatedBy) = (init, term)

    val initWeightSum = if (initiatedBy.nonEmpty) initiatedBy.map(x => markedMap(x).w).sum else 0.0
    val termWeightSum = if (terminatedBy.nonEmpty) terminatedBy.map(x => markedMap(x).w).sum else 0.0

    val inertiaExpertPrediction = getInertiaExpertPrediction(currentFluent)

    val firingInitRulesIds = initiatedBy

    val nonFiringInitRules =
      markedMap.filter(x =>
        x._2.head.functor.contains("initiated") && !firingInitRulesIds.contains(x._1))

    val predictInitiated = initWeightSum - nonFiringInitRules.values.map(_.w).sum
    // Use the above to have all rules and their refs voting independently.
    // Use the one below to have one prediction per top rule, resulting by combing the
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

    val firingTermRulesIds = terminatedBy

    val nonFiringTermRules =
      markedMap.filter(x =>
        x._2.head.functor.contains("terminated") && !firingTermRulesIds.toSet.contains(x._1))

    val predictTerminated = termWeightSum - nonFiringTermRules.values.map(_.w).sum
    // Use the above to have all rules and their refs voting independently.
    // Use the one below to have one prediction per top rule, resulting by combing the
    // opinions of the rule's sub-expert committee (its specializations)
    /*
    val predictTerminatedTimed = Utils.time {
      val individualPredictions =
        theory.head.clauses.map( rule => getRulePrediction(rule, firingTermRulesIds, nonFiringTermRules.keys.toVector) )
      individualPredictions.sum
    }
    val predictTerminated = predictTerminatedTimed._1
    totalPredictionTime += predictTerminatedTimed._2
    */

    val predictAtomHolds = predict(inertiaExpertPrediction, predictInitiated, predictTerminated, isStrongInertia)

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

        // Then it's a TP. Simply return it without updating any weights.
        totalTPs = totalTPs + currentAtom
        "TP"

      } else {
        // Then it's an FP.

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
          //reduceWeights(nonFiringTermRules.keys.toVector, markedMap, learningRate)

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
        "FP" // result returned to the calling method.
      }
    } else {

      // We predicted that the atom does not hold...
      if (trueAtoms.contains(currentAtom)) {

        // ...while it actually does, so we have an FN.

        if (testOnly) {
          logger.info(s"InitSum: ${initWeightSum - nonFiringInitRules.values.map(_.w).sum} TermSum: ${termWeightSum - nonFiringTermRules.values.map(_.w).sum}")
        }

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
        "FN" // result returned to the calling method.
      } else {
        // Then we have an atom which was erroneously inferred by the (un-weighted) rules (with ordinary logical
        // inference), but which eventually not inferred, thanks to the expert-based weighted framework. That is,
        // either the total weight of the non-firing "initiatedAt" fragment of the theory was greater than the weight
        // of the firing part, or the the total weight of the firing "terminatedAt" path of the theory was greater
        // than the weight of the "initiatedAt" fragment. In either case, the atom is eventually a TN. We don't do
        // anything with it, but we need to instruct the the inertia expert to "forget" the atom.
        if (inertiaExpert.keySet.contains(currentFluent)) {
          inertiaExpert -= currentFluent
        }
        "TN"
      }
    }
  }












}

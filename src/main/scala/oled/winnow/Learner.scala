package oled.winnow

import java.io.File

import akka.actor.{Actor, ActorRef, PoisonPill, Props}
import app.runutils.IOHandling.Source
import app.runutils.{Globals, RunningOptions}
import logic.Examples.Example
import logic.{Literal, LogicUtils, Theory}
import oled.winnow.MessageTypes.{FinishedBatchMsg, ProcessBatchMsg}
import org.slf4j.LoggerFactory
import oled.functions.SingleCoreOLEDFunctions.{crossVal, eval}

import scala.collection.mutable.Map
import AuxFuncs._
import utils.{ASP, Utils}
import utils.Implicits._
import PredictUpdateHandler._
import scala.collection.immutable.ListMap
import scala.collection.mutable
import scala.reflect.internal.Trees


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
                           val testingDataFunction: T => Iterator[Example]) extends Actor {

  startTime = System.nanoTime()

  /*
  private var totalTPs = 0
  private var totalFPs = 0
  private var totalFNs = 0
  private var totalTNs = 0
  */

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

  private val logger = LoggerFactory.getLogger(self.path.name)

  private val withec = Globals.glvalues("with-ec").toBoolean

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

  private def getNextBatch = {
    this.batchCounter += 1
    if (data.isEmpty) Example() else data.next()
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

    val nextBatch = getNextBatch

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

    logger.info(s"Prequential error vector:\n${prequentialError.mkString(",")}")
    logger.info(s"Prequential error vector (Accumulated Error):\n${prequentialError.scanLeft(0.0)(_ + _).tail}")

    logger.info(s"Total TPs: ${totalTPs.size}, Total FPs: ${totalFPs.size}, Total FNs: ${totalFNs.size}")

    logger.info("Evaluating on the test set")

    val testData = testingDataFunction(testingDataOptions)

    val (tps,fps,fns,precision,recall,fscore) = crossVal(pruned_, data=testData, globals = inps.globals, inps = inps)

    logger.info(s"\ntps: $tps\nfps: $fps\nfns: " + s"$fns\nprecision: $precision\nrecall: $recall\nf-score: $fscore)")
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





  def evaluateTest_NEW(batch: Example, inputTheoryFile: String = "") = {

    if (withec) {

      val (init, term) = (theory.head, theory.tail.head)

      val _merged = {
        /*
        val initRules = LogicUtils.compressTheory(init.clauses.filter(_.body.nonEmpty))
        val termRules = LogicUtils.compressTheory(term.clauses.filter(_.body.nonEmpty))
        Theory(initRules ++ termRules)
        */
        Theory( (init.clauses ++ term.clauses).filter(p => p.body.nonEmpty) )
      }

      val mergedWithRefs = Theory(_merged.clauses ++ _merged.clauses.flatMap(_.refinements))

      val merged = _merged
      //val merged = mergedWithRefs

      // just for debugging
      val weightsBefore = merged.clauses.map(x => x.w)

      val targetFluent = {
        // We can take the first one of the head modes (the target fluent is the same
        // regardless of whether the mode atom is an initiation of termination one).
        // Then, to get the target fluent, simply retrieve the first one of the 'terms' arg.

        val t = inps.globals.MODEHS.head.varbed.terms.head
        // The 'if' is for cases where the target pred is of the form initiatedAt(#fluent, +time), as in
        // initiatedAt(#fluent, +time) where fluent(leisure) is in the BK.
        // The 'else' is for compound fluents.
        if (t.isVariabe) Literal(functor = t._type) else inps.globals.MODEHS.head.varbed.terms.head.asInstanceOf[Literal]
        //modehs.head.varbed.terms.head.asInstanceOf[Literal]
      }.tostring

      val tpRule1 = s"tp(I, holdsAt($targetFluent,Te)) :- rule(I), example( holdsAt($targetFluent,Te) ), marked(I, initiatedAt($targetFluent,Ts) ), next(Ts,Te), time(Te), time(Ts)."
      val tpRule2 = s"tp(I, holdsAt($targetFluent,Te)) :- rule(I), example( holdsAt($targetFluent,Te) ), not marked(I, terminatedAt($targetFluent,Ts) ), next(Ts,Te), time(Te), time(Ts)."
      val tpRule3 = s"tp(inertia, holdsAt($targetFluent,Te)) :- example( holdsAt($targetFluent,Te) ), inertia( holdsAt($targetFluent,Ts) ), next(Ts,Te), time(Te), time(Ts)."

      val fpRule = s"fp(I, holdsAt($targetFluent,Te)) :- rule(I), not example( holdsAt($targetFluent,Te) ), marked(I, initiatedAt($targetFluent,Ts) ), next(Ts,Te), time(Te), time(Ts)."
      val fpRule2 = s"fp(inertia, holdsAt($targetFluent,Te)) :- not example( holdsAt($targetFluent,Te) ), inertia( holdsAt($targetFluent,Ts) ), next(Ts,Te), time(Te), time(Ts)."

      val fnRule = s"fn(I, holdsAt($targetFluent,Te)) :- rule(I), example( holdsAt($targetFluent,Te) ), marked(I, terminatedAt($targetFluent,Ts) ), next(Ts,Te), time(Te), time(Ts)."





      merged.clauses foreach (rule => if (rule.refinements.isEmpty) rule.generateCandidateRefs(inps.globals))
      //val t = merged.clauses.flatMap(x => x.refinements :+ x)

      val _marked = marked(merged.clauses.toVector, inps.globals)
      val markedProgram = _marked._1
      val markedMap = _marked._2
      val e = (batch.annotationASP ++ batch.narrativeASP).mkString("\n")

      val directives = s"\n$tpRule1\n$tpRule2\n$fpRule\n$fnRule"

      val program = e + markedProgram + "\n#include \""+inps.entryPath+"/bk.lp\"." + directives + "\n#show.\n#show tp/2.\n#show fp/2.\n#show fn/2."
      val f2 = Utils.getTempFile(s"quick-and-dirty",".lp")
      Utils.writeToFile(f2, "append")(p => List(program) foreach p.println)
      val paaath = f2.getCanonicalPath
      val _result = ASP.solve(task = Globals.SCORE_RULES, aspInputFile = new File(paaath))

      val result = if (_result.nonEmpty) _result.head.atoms.toSet else Set[String]()

      val (tpAtoms, fpAtoms, fnAtoms) = result.foldLeft(Set[String](), Set[String](), Set[String]()) { (x, atom) =>
        val (a, b, c) = (x._1, x._2, x._3)
        if (atom.startsWith("tp")) (a + atom, b, c)
        else if (atom.startsWith("fp")) (a, b+atom, c)
        else if (atom.startsWith("fn")) (a,b,c+atom)
        else throw new RuntimeException("FUCK This shouldn't have happened")
      }

      //println(tpAtoms)
      //println(fpAtoms)
      //println(fnAtoms)

      val allInferredAtoms = tpAtoms ++ fpAtoms ++ fnAtoms

      // We'll use a map to process the results. The keys will be actual holdsAt/2 atoms.
      // The values will be three lists of clauses:
      // initiatedBy, terminatedBy and notTerminatedBy. Each list contains the ids of the rules that
      // respectively initiate, terminate and not terminate the key atom

      val map = scala.collection.mutable.Map[String, (Vector[String], Vector[String], Vector[String])]()

      allInferredAtoms foreach { s =>
        val l = Literal.parse(s)
        val functor = l.functor
        val ruleId = l.terms.head.tostring
        val holdsAtAtom = l.terms.tail.head.tostring
        val actualRule = markedMap(ruleId)

        //if (map.keySet.contains(holdsAtAtom)) {
        if (functor == "tp") {
          // INITIATED TP
          if (actualRule.head.functor.contains("initiated")) {
            // then the atom is correctly derived by the initiation rule.
            // Add the rule id to the initiatedBy field of the map (the 1st one)
            if (map.keySet.contains(holdsAtAtom)) {
              map(holdsAtAtom) = (map(holdsAtAtom)._1 :+ ruleId, map(holdsAtAtom)._2, map(holdsAtAtom)._3)
            } else {
              map(holdsAtAtom) = (Vector(ruleId), Vector[String](), Vector[String]())
            }
            // TERMINATED TP
          } else {
            // then the rule is correctly not terminated by the termination rule.
            // Add the rule id to the notTerminatedBy field of the map (the 3rd one)
            if (map.keySet.contains(holdsAtAtom)) {
              map(holdsAtAtom) = (map(holdsAtAtom)._1, map(holdsAtAtom)._2, map(holdsAtAtom)._3 :+ ruleId)
            } else {
              map(holdsAtAtom) = (Vector[String](), Vector[String](), Vector(ruleId))
            }
          }
        } else if (functor == "fp") {
          // This can only happen initiation rules. Add the atom to the initiatedBy field of the map.
          if (map.keySet.contains(holdsAtAtom)) {
            map(holdsAtAtom) = (map(holdsAtAtom)._1 :+ ruleId, map(holdsAtAtom)._2, map(holdsAtAtom)._3)
          } else {
            map(holdsAtAtom) = (Vector(ruleId), Vector[String](), Vector[String]())
          }
        } else if (functor == "fn") {
          // This can only happen termination rules. Add the atom to the terminatedBy field of the map.
          if (map.keySet.contains(holdsAtAtom)) {
            map(holdsAtAtom) = (map(holdsAtAtom)._1, map(holdsAtAtom)._2 :+ ruleId, map(holdsAtAtom)._3)
          } else {
            map(holdsAtAtom) = (Vector[String](), Vector(ruleId), Vector[String]())
          }
        } else {
          throw new RuntimeException(s"Unexpected predicate symbol: $functor")
        }
      }

      // Next, we sort the map by the time-stamp of each inferred holdsAt atom in ascending order.
      // For each holdsAt atom we calculate if it should actually be inferred, based on the weights
      // of the rules that initiate or terminate it. In this process, the weights of the rules are
      // updated based on whether the atom is mistakenly/correctly predicted and whether each individual
      // rule mistakenly/correctly predicts it. Sorting the inferred atoms and iterating over them is necessary
      // so as to promote/penalize the rule weights correctly after each mistake.

      /*
      val _sortedMap = scala.collection.mutable.Map[Int, Set[String]]()
      map foreach { entry =>
        val parsed = Literal.parse(entry._1)
        val time = parsed.terms.tail.head.name.toInt
        if (_sortedMap.contains(time)) {
          _sortedMap(time) = _sortedMap(time) + parsed.tostring
        } else {
          _sortedMap(time) = Set(parsed.tostring)
        }
      }
      val sorted = _sortedMap.toSeq.sortBy(_._1)
      */

      ///*
      val sorted = map.map { entry =>
        val parsed = Literal.parse(entry._1)
        val time = parsed.terms.tail.head.name.toInt
        ((entry._1, time), entry._2)
      }.toVector.sortBy(x => x._1._2) // sort by time
      //*/

      //val lastTimePoint = if (sorted.nonEmpty) sorted.last._1._2 else 0

      val inertiaMap = scala.collection.mutable.Map[Int, Set[InertiaFluent]]()

      val trueAtoms = batch.annotation.toSet

      // The sets in the accumulator of the foldLeft represent TPs, FPs, FNs,
      // as identified by the annotation during the foldLeft iteration
      val inferred = sorted.foldLeft(Set[String](), Set[String](), Set[String]()) { (accum, y) =>

        val (atom, time) = (y._1._1, y._1._2)

        val parsed = Literal.parse(atom)

        //-----------------------------------------------------------------------------
        //-----------------------------------------------------------------------------
        // this is for updating inertia. I need to do this properly and optimize it.
        val inert = inertiaMap.head // this is supposed to always have only one record (the previous time-point).
        val (prevTime, inertiaFluents) = (inert._1, inert._2)
        var intertiaExpertPrediction = 0.0
        if (time > prevTime) { // just to be on the safe side
          val currentFluent = parsed.terms.head.tostring
          val inertAtom = inertiaFluents.find{ p => p.fluentAtom == currentFluent }.getOrElse(InertiaFluent())
          if (inertAtom != InertiaFluent()) {
            intertiaExpertPrediction = inertAtom.weight
          }
        } else {
          throw new RuntimeException("Problem with inertia at the experts setting.")
        }
        //-----------------------------------------------------------------------------
        //-----------------------------------------------------------------------------

        val (initiatedBy, terminatedBy, notTerminatedBy) = (y._2._1, y._2._2, y._2._2)

        val initWeightSum = if (initiatedBy.nonEmpty) initiatedBy.map(x => markedMap(x).w).sum else 0.0
        val termWeightSum = if (terminatedBy.nonEmpty) terminatedBy.map(x => markedMap(x).w).sum else 0.0
        val notTermWeightsSum = if (notTerminatedBy.nonEmpty) notTerminatedBy.map(x => markedMap(x).w).sum else 0.0

        val firingInitRules = initiatedBy
        val nonFiringInitRules =
          markedMap.filter(x =>
            x._2.head.functor.contains("initiated") && !firingInitRules.toSet.contains(x._1)).values

        val predictInitiated = (intertiaExpertPrediction + initWeightSum) - nonFiringInitRules.map(_.w).sum

        val firingTermRules = terminatedBy
        val nonFiringTermRules =
          markedMap.filter(x =>
            x._2.head.functor.contains("terminated") && !firingTermRules.toSet.contains(x._1)).values

        val predictTerminated = termWeightSum - nonFiringTermRules.map(_.w).sum

        //val predictAtomHolds = initWeightSum + notTermWeightsSum >= termWeightSum
        //val predictAtomHolds = initWeightSum >= termWeightSum

        /*
        val predictAtomHolds = {
          if (predictInitiated >= predictTerminated) {
            if (predictInitiated > 0.0) true else false
          } else {
            // MAYBE YOU WANT TO THINK THIS OVER...
            false
          }
        }
        */

        val predictAtomHolds = {
          if (predictInitiated > 0 && predictTerminated > 0) {
            // If we predict both initiation and termination keep the largest
            if (predictInitiated >= predictTerminated) true else false
          } else {
            if (predictInitiated > 0) true else false
          }
        }

        if (predictAtomHolds) {

          //----------------------------------------------------------------------------------------
          //----------------------------------------------------------------------------------------
          // this is for updating inertia. I need to do this properly and optimize it.
          val (fluent, time) = (parsed.terms.head.tostring, parsed.terms.tail.head.tostring.toInt)



          if (intertiaExpertPrediction > 0.0) {

          }
          //----------------------------------------------------------------------------------------
          //----------------------------------------------------------------------------------------

          if (trueAtoms.contains(atom)) {
            // Then it's a TP. Simply return it without updating any weights.
            //totalTPs += 1
            totalTPs = totalTPs + atom
            (accum._1 + atom, accum._2, accum._3)
          } else {
            // Then it's an FP. We want to decrease the weights of all rules that contribute to the FP:
            // Rules that incorrectly initiate it and rules that incorrectly terminate it.
            totalFPs = totalFPs + atom
            reduceWeights(initiatedBy, markedMap)
            reduceWeights(terminatedBy, markedMap)
            (accum._1, accum._2 + atom, accum._3)
          }
        } else {
          // We predicted it does not hold
          if (trueAtoms.contains(atom)) {
            // but it actually does, so we have an FN. We want to increase the weights of all rules that initiate it
            // and all rules that do not terminate it.
            totalFNs = totalFNs + atom
            increaseWeights(initiatedBy, markedMap)
            increaseWeights(notTerminatedBy, markedMap)
            (accum._1, accum._2, accum._3 + atom)
          } else {
            // Then we have an atom which was erroneously inferred by the (un-weighted) rules (with ordinary logical
            // inference), but which eventually not inferred, thanks to the expert-based weighted framework. That is,
            // either the total weight of the non-firing "initiatedAt" fragment of the theory was greater than the weight
            // of the firing part, or the the total weight of the firing "terminatedAt" path of the theory was greater
            // than the weight of the "initiatedAt" fragment. In either case, the atom is eventually a TN. We don't do
            // anything with it.
            (accum._1, accum._2, accum._3)
          }
        }
      }

      val (tps, fps, fns) = (inferred._1, inferred._2, inferred._3)

      val (fpsNumber, currentFNsNumber) = (fps.size, fns.size)

      // All atoms in tps are certainly true positives. But we need to account for the real true atoms which are not in there
      val restFNs = trueAtoms.diff(tps)

      val restFNsNumber = restFNs.size

      var trueFNsNumber = currentFNsNumber + restFNsNumber

      // Ugly (AND DANGEROUS) hack to avoid counting as mistakes the holdsAt/2 atoms at the first time point of an interval
      if (trueFNsNumber == 2) trueFNsNumber = 0

      // We have gathered the FNs that have not been inferred, but we need to add the rest of them in the global counter
      totalFNs = totalFNs ++ restFNs

      // Just for debugging.
      val weightsAfter = merged.clauses.map(x => x.w)

      prequentialError = prequentialError :+ (fpsNumber + trueFNsNumber).toDouble

      if (fpsNumber + trueFNsNumber > 0) {
        logger.info(s"\nMade mistakes: FPs: $fpsNumber, " +
          s"FNs: $trueFNsNumber.\nWeights before: $weightsBefore\nWeights after: $weightsAfter\nPredicted with:\n${merged.showWithStats}")
      }
    } else { // No Event Calculus. We'll see what we'll do with that.

    }

  }









  /* Performs online evaluation. Prequential is always performed.
   * Holdout is  performed every 1000 examples if the testing set is non-empty.
   */
  def evaluateTest(batch: Example, inputTheoryFile: String = "") = {

    //if (batch.time == "4000") {
    //  val stop = "stop"
    //}

    // prequential first
    if (withec) {
      val (init, term) = (theory.head, theory.tail.head)

      val merged = Theory( (init.clauses ++ term.clauses).filter(p => p.body.length >= 1) )

      //val merged = Theory( (init.clauses ++ term.clauses).filter(p => p.body.length >= 1 && p.score >= inps.pruneThreshold) )

      //val merged = Theory( init.clauses.filter(p => p.precision >= inps.pruneThreshold) ++ term.clauses.filter(p => p.recall >= inps.pruneThreshold) )

      //------------------
      // TEST STUFF START
      //------------------

      if (true) {
      //if (theory.head.clauses.nonEmpty && theory.tail.head.clauses.nonEmpty) {

        merged.clauses foreach (rule => if (rule.refinements.isEmpty) rule.generateCandidateRefs(inps.globals))
        //val t = merged.clauses.flatMap(x => x.refinements :+ x)

        val _marked = marked(merged.clauses.toVector, inps.globals)

        // QUICK AND DIRTY SOLUTION JUST TO TRY IT.


        val e = (batch.annotationASP ++ batch.narrativeASP).mkString("\n")
        val markedProgram = _marked._1
        val markedMap = _marked._2
        val all = e + markedProgram + "\n#include \""+inps.entryPath+"/bk.lp\"." + "\n#show marked/2."
        val f = Utils.getTempFile(s"quick-and-dirty",".lp")
        Utils.writeToFile(f, "append")(p => List(all) foreach p.println)
        val path = f.getCanonicalPath
        val answerSet = ASP.solve(task = Globals.SCORE_RULES, aspInputFile = new File(path))
        val atoms = if (answerSet.nonEmpty) answerSet.head.atoms.toSet else Set[String]()

        val inferred_temp = atoms.map{ a =>
           val l = Literal.parse(a)
          (l.terms.head.tostring, l.terms.tail.head.tostring)
        }.groupBy(z => z._2).map(z =>  (z._1, z._2.map(_._1)) )



        // Here we get the initiatedAt and terminatedAt atoms inferred by the winnow weighted majority scheme.
        // These are then passed to the reasoner to infer the actual holdsAt atoms
        val inferred_final = inferred_temp.foldLeft(Set[String]()) { (accum, y) =>

          val (atom, firingRuleIds) = (y._1, y._2)

          val nonFiringRuleIds =
            if (atom.contains("initiated")) {
              val allRelevantRules = markedMap.filter(p => p._2.head.functor.contains("initiated")).keys.toSet
              allRelevantRules.diff(firingRuleIds)
            } else {
              val allRelevantRules = markedMap.filter(p => p._2.head.functor.contains("terminated")).keys.toSet
              allRelevantRules.diff(firingRuleIds)
            }

          val firingRulesweightSum = firingRuleIds.map(id => markedMap(id).w).sum
          val nonFiringRulesWeightSum = nonFiringRuleIds.map(id => markedMap(id).w).sum

          ///*
          val initRules = theory.head.clauses
          val initRulesNum = initRules.length
          //val initRulesAll = initRules ++ initRules.flatMap(_.refinements)
          //val initRulesNum = initRulesAll.length


          val termRules = theory.tail.head.clauses
          val termRulesNum = termRules.length
          //val termRulesAll = termRules ++ termRules.flatMap(_.refinements)
          //val termRulesNum = termRulesAll.length

          /*
          val majority = if (atom.contains("initiated")) initRulesNum else termRulesNum
          val weightSum = firingRulesweightSum
          //val majority = 16000
          if (weightSum >= majority) {
            accum + atom
          } else {
            accum
          }
          */

          //if (firingRulesweightSum - nonFiringRulesWeightSum >= 1) {
          if (firingRulesweightSum >= nonFiringRulesWeightSum) {
            accum + atom
          } else {
            accum
          }

        }

        /*
        if (inferred_final.exists(x => x.contains("initiatedAt"))) {
          println("HERE")
          println(inferred_final)
          println(merged.showWithStats)
          val stop = "stop"
        }
        */

        // this is used to generate the actual holdsAt atoms predicted by our theory, using the
        // initiatedAt/terminatedAt inferred previously.
        val evalProgram =
          (batch.narrative.toSet ++ inferred_final).map(x => x+".").mkString("\n")+
          inps.globals.INCLUDE_BK(inps.globals.BK_CROSSVAL)+"\nout(holdsAt(F,T)) :- holdsAt(F,T), fluent(F).\n#show.\n#show out/1."
        val f1 = Utils.getTempFile("isConsistent",".lp")
        Utils.writeLine(evalProgram, f1, "overwrite")

        val inferredState = ASP.solve(task = Globals.INFERENCE, aspInputFile = f1)

        val _inferred = if (inferredState.nonEmpty) inferredState.head.atoms.toSet else Set[String]()
        val inferred = _inferred.map(x => Literal.parse(x).terms.head.tostring)

        val trueAtoms = batch.annotation.toSet

        val (inferredTPs, inferredFPs, inferredFNs) =
        if (inferredState.nonEmpty) {
          val _tps = inferred.intersect(trueAtoms)
          val _fps = inferred.diff(trueAtoms)
          val _fns = trueAtoms.diff(inferred)
          (_tps, _fps, _fns)
        } else {
          (Set[String](), Set[String](), batch.annotation.toSet)
        }

        /*
        if (inferredFPs.nonEmpty) {
          println(s"\n\n\n\n\n\n\n\nHERE+${inferredFPs.size}\n\n\n\n\n\n\n")
        }
        */

        // Ugly hack to avoid counting as mistakes the holdsAt/2 atoms at the first time point of an interval
        val _inferredFNS = if (inferredFNs.size == 2) Set[String]() else inferredFNs

        //prequentialError = prequentialError :+ (inferredFPs.size + inferredFNs.size).toDouble
        prequentialError = prequentialError :+ (inferredFPs.size + _inferredFNS.size).toDouble


        val msg = batch.time+" Test eval for winnow" + s"TPs: ${inferredTPs.size}, FPs: ${inferredFPs.size}, FNs: ${inferredFNs.size}/${_inferredFNS.size}"

        logger.info("\n"+msg+"\n"+merged.clauses.map(x => x.w)+"\n"+prequentialError.mkString(" "))

        //-----------------------
        // WINNOW WEIGHTS UPDATE
        //-----------------------

        var tpRule1 = ""
        var tpRule2 = ""
        var fpRule = ""
        var fnRule = ""

        // This is copied form BKHandling.generateScoringBK
        if (Globals.glvalues("with-ec").toBoolean) { // We're learning with the Event Calculus in the BK.
          // We can get the fluent from the head modes.
          val targetFluent = {
            // We can take the first one of the head modes (the target fluent is the same
            // regardless of whether the mode atom is an initiation of termination one).
            // Then, to get the target fluent, simply retrieve the first one of the 'terms' arg.

            val t = inps.globals.MODEHS.head.varbed.terms.head
            // The 'if' is for cases where the target pred is of the form initiatedAt(#fluent, +time), as in
            // initiatedAt(#fluent, +time) where fluent(leisure) is in the BK.
            // The 'else' is for compound fluents.
            if (t.isVariabe) Literal(functor = t._type) else inps.globals.MODEHS.head.varbed.terms.head.asInstanceOf[Literal]
            //modehs.head.varbed.terms.head.asInstanceOf[Literal]
          }.tostring

          tpRule1 = s"tp(I, holdsAt($targetFluent,Te)) :- rule(I), example( holdsAt($targetFluent,Te) ), marked(I, initiatedAt($targetFluent,Ts) ), next(Ts,Te), time(Te), time(Ts)."
          tpRule2 = s"tp(I, holdsAt($targetFluent,Te)) :- rule(I), example( holdsAt($targetFluent,Te) ), not marked(I, terminatedAt($targetFluent,Ts) ), next(Ts,Te), time(Te), time(Ts)."
          fpRule = s"fp(I, holdsAt($targetFluent,Te)) :- rule(I), not example( holdsAt($targetFluent,Te) ), marked(I, initiatedAt($targetFluent,Ts) ), next(Ts,Te), time(Te), time(Ts)."
          fnRule = s"fn(I, holdsAt($targetFluent,Te)) :- rule(I), example( holdsAt($targetFluent,Te) ), marked(I, terminatedAt($targetFluent,Ts) ), next(Ts,Te), time(Te), time(Ts)."

        } else { // No Event Calculus
          //val targetPred = inps.globals.MODEHS.head.varbed
          //TODO
        }


        val directives = s"\n$tpRule1\n$tpRule2\n$fpRule\n$fnRule"

        val program = e + markedProgram + "\n#include \""+inps.entryPath+"/bk.lp\"." +
          directives + "\n#show.\n#show tp/2.\n#show fp/2.\n#show fn/2."
        val f2 = Utils.getTempFile(s"quick-and-dirty",".lp")
        Utils.writeToFile(f2, "append")(p => List(program) foreach p.println)
        val paaath = f2.getCanonicalPath
        val _result = ASP.solve(task = Globals.SCORE_RULES, aspInputFile = new File(paaath))

        val result = if (_result.nonEmpty) _result.head.atoms.toSet else Set[String]()

        val (tpAtoms, fpAtoms, fnAtoms) = result.foldLeft(Set[String](), Set[String](), Set[String]()) { (x, atom) =>
          val (a, b, c) = (x._1, x._2, x._3)
          if (atom.startsWith("tp")) (a + atom, b, c)
          else if (atom.startsWith("fp")) (a, b+atom, c)
          else if (atom.startsWith("fn")) (a,b,c+atom)
          else throw new RuntimeException("FUCK This shouldn't have happened")
        }


        // foreach inferred FN atom x:
        //    foreach initiation rule r that correctly fires (x is a TP for r):
        //        increase r's weight
        //    foreach termination rule r that correctly does not fire (x is a TP for r):
        //        increase r's weight
        ///*
        _inferredFNS foreach { x =>
        //inferredFNs foreach { x =>
          tpAtoms foreach { y =>
            if (y.contains(x)) {
              val l = Literal.parse(y)
              val ruleId = l.terms.head.tostring
              val rule = markedMap(ruleId)
              //val newWeight = rule.w*2
              val newWeight = rule.w*Math.pow(Math.E, 2.0)
              rule.w = if (newWeight.isPosInfinity) rule.w else newWeight
            }
          }
        }
        //*/

        // foreach inferred FP atom x:
        //   foreach initiated rule r that incorrectly fires (x is an FP for r):
        //     decrease r's weight
        //   foreach termination rule that incorrectly fires (x is an FN for r):
        //     decrease r's weight
        inferredFPs foreach { x =>
          fpAtoms foreach { y =>
            if (y.contains(x)) {
              val l = Literal.parse(y)
              val ruleId = l.terms.head.tostring
              val rule = markedMap(ruleId)
              //val newWeight = rule.w*0.5
              val newWeight = rule.w*Math.pow(Math.E, -2.0)
              rule.w = newWeight
            }
          }
          // this is the same, factor it out
          fnAtoms foreach { y =>
            if (y.contains(x)) {
              val l = Literal.parse(y)
              val ruleId = l.terms.head.tostring
              val rule = markedMap(ruleId)
              //val newWeight = rule.w*0.5
              val newWeight = rule.w*Math.pow(Math.E, -2.0)
              rule.w = newWeight
            }
          }
        }


        //println(result)

        //-----------------------
        // WINNOW WEIGHTS UPDATE
        //-----------------------

        //------------------
        // TEST STUFF END
        //------------------


        // QUICK AND DIRTY SOLUTION JUST TO TRY IT.
      }


    }

    // TODO :
    // Implement holdout evaluation.
  }



}

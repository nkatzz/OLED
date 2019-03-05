package oled.mwua

import java.io.{File, PrintWriter}

import akka.actor.Actor
import app.runutils.IOHandling.Source
import app.runutils.RunningOptions
import logic.Examples.Example
import logic.{Clause, Theory}
import org.slf4j.LoggerFactory

import scala.util.matching.Regex

class Learner_NEW[T <: Source](val inps: RunningOptions,
                               val trainingDataOptions: T,
                               val testingDataOptions: T,
                               val trainingDataFunction: T => Iterator[Example],
                               val testingDataFunction: T => Iterator[Example],
                               val writeExprmtResultsTo: String = "") extends Actor {
  val learningRate = 1.0

  val epsilon = 0.9 // used in the randomized version

  val randomizedPrediction = false

  // If this is false, some non-determinism is introduced (number of mistakes may vary slightly from round to round)
  val specializeAllAwakeRulesOnFPMistake = false

  // Set this to 1.0 to simulate the case of constant feedback at each round.
  // For values < 1.0 we only update weights and structure if a biased coin
  // with receiveFeedbackBias for heads returns heads.
  val receiveFeedbackBias = 1.0 //0.5

  val conservativeRuleGeneration = false

  // A rule must make this much % of the total FPs before it is specialized
  val percentOfMistakesBeforeSpecialize = 0

  // have this set to "" for a regular run without an input theory
  //val inputTheoryFile = "/home/nkatz/Desktop/theory"
  val inputTheoryFile = ""

  val inputTheory: List[Clause] = {
    def matches(p: Regex, str: String) = p.pattern.matcher(str).matches
    if (inputTheoryFile == "") {
      Nil
    }  else {
      val rules = scala.io.Source.fromFile(inputTheoryFile).getLines.toList.filter(line => !matches( """""".r, line) && !line.startsWith("%"))
      val rulesParsed = rules.map(r => Clause.parse(r))
      rulesParsed
    }
  }

  val stateHandler: StateHandler = {
    val stateHandler = new StateHandler
    if (inputTheory.isEmpty) {
      stateHandler
    } else {
      val (inputInitRules, inputTermRules) = inputTheory.foldLeft(List.empty[Clause], List.empty[Clause]){ (x, y) =>
        if (y.head.functor.contains("initiated")) (x._1 :+ y, x._2) else (x._1, x._2 :+ y)
      }
      stateHandler.ensemble.initiationRules = inputInitRules
      stateHandler.ensemble.terminationRules = inputTermRules
      stateHandler
    }
  }


  // Just print-out all the data (vag wanted it).
  /*
  val test = {
    val pw = new PrintWriter(new File("/home/nkatz/Desktop/caviar-whole" ))
    data = getTrainData
    while (data.hasNext) {
      val x = data.next()
      val a = (x.annotation ++ x.narrative).mkString("\n")+"\n\n% New Batch\n\n"
      pw.write(a)
    }
    pw.close()
  }
  */


  private val logger = LoggerFactory.getLogger(self.path.name)

  private val withec = true

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

  /*
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
  */

  def receive = {

    case "start" => {

      data = getTrainData

      if (this.data.isEmpty) {
        logger.error(s"Input source ${inps.train} is empty.")
        System.exit(-1)
      }

      var done = false

      while(! done) {

        val nextBatch = getNextBatch(lleNoise = false)
        logger.info(s"Processing batch $batchCounter")
        if (nextBatch.isEmpty) {
          logger.info(s"Finished the data.")
          endTime = System.nanoTime()
          logger.info("Done.")
          //workers foreach(w => w ! PoisonPill)
          wrapUp()
          done = true
          context.system.terminate()
        } else {
          val trueLabels = nextBatch.annotation.toSet

          if (inputTheory.isEmpty) {
            ExpertAdviceFunctions.process(nextBatch, nextBatch.annotation.toSet, inps,
              stateHandler, trueLabels, learningRate, epsilon, randomizedPrediction,
              batchCounter, percentOfMistakesBeforeSpecialize, specializeAllAwakeRulesOnFPMistake, receiveFeedbackBias, conservativeRuleGeneration)
          } else {
            ExpertAdviceFunctions.process(nextBatch, nextBatch.annotation.toSet, inps,
              stateHandler, trueLabels, learningRate, epsilon, randomizedPrediction,
              batchCounter, percentOfMistakesBeforeSpecialize, specializeAllAwakeRulesOnFPMistake, receiveFeedbackBias, conservativeRuleGeneration, inputTheory = Some(inputTheory))
          }


        }
      }
    }

  }





  def wrapUp() = {

    //logger.info(Theory(stateHandler.ensemble.merged(inps).clauses.sortBy(x => -x.w)).showWithStats)

    /*
    def bestRuleFromExpertTree(topLevelRule: Clause) = {
      (Vector(topLevelRule) ++ topLevelRule.refinements).sortBy(x => -x.w).head
    }
    */

    if (trainingDataOptions != testingDataOptions) {

      // show the info so far:
      wrapUp_NO_TEST()

      // and do the test
      logger.info("\n\nEvaluating on the test set\n\n")

      val _stateHandler = new StateHandler

      _stateHandler.ensemble = {
        val ensemble = stateHandler.ensemble
        val init = ensemble.initiationRules.filter(x => x.body.nonEmpty)
        val term = ensemble.terminationRules.filter(x => x.body.nonEmpty)
        ensemble.initiationRules = init
        ensemble.terminationRules = term
        ensemble
      }

      val testData = testingDataFunction(testingDataOptions)
      val _receiveFeedbackBias = 0.0 // Give no supervision for training, we're only testing
      testData foreach { batch =>
        val trueLabels = batch.annotation.toSet
        ExpertAdviceFunctions.process(batch, batch.annotation.toSet, inps,
          _stateHandler, trueLabels, learningRate, epsilon, randomizedPrediction,
          batchCounter, percentOfMistakesBeforeSpecialize, specializeAllAwakeRulesOnFPMistake, _receiveFeedbackBias, conservativeRuleGeneration)
      }
      logger.info(s"Prequential error vector:\n${_stateHandler.perBatchError.mkString(",")}")
      logger.info(s"Prequential error vector (Accumulated Error):\n${_stateHandler.perBatchError.scanLeft(0.0)(_ + _).tail}")
      logger.info(s"Total TPs: ${_stateHandler.totalTPs}, Total FPs: ${_stateHandler.totalFPs}, Total FNs: ${_stateHandler.totalFNs}, Total TNs: ${_stateHandler.totalTNs}")
      if (_receiveFeedbackBias != 1.0) {
        logger.info(s"\nReceived feedback on ${_stateHandler.receivedFeedback} rounds")
      }
    } else {
      wrapUp_NO_TEST()
    }


  }

  def wrapUp_NO_TEST() = {
    logger.info(Theory(stateHandler.ensemble.initiationRules.sortBy(x => -x.w)).showWithStats)
    logger.info(Theory(stateHandler.ensemble.terminationRules.sortBy(x => -x.w)).showWithStats)

    logger.info(s"Prequential error vector:\n${stateHandler.perBatchError.mkString(",")}")
    logger.info(s"Prequential error vector (Accumulated Error):\n${stateHandler.perBatchError.scanLeft(0.0)(_ + _).tail}")
    logger.info(s"Total TPs: ${stateHandler.totalTPs}, Total FPs: ${stateHandler.totalFPs}, Total FNs: ${stateHandler.totalFNs}, Total TNs: ${stateHandler.totalTNs}")
    logger.info(s"Total time: ${(endTime - startTime)/1000000000.0}")

    if (randomizedPrediction) {
      logger.info(s"\nPredicted with initiation rules: ${stateHandler.predictedWithInitRule} times")
      logger.info(s"\nPredicted with terminated rules: ${stateHandler.predictedWithTermRule} times")
      logger.info(s"\nPredicted with inertia: ${stateHandler.predictedWithInertia} times")
    }

    //logger.info(s"Predictions vector:\n${stateHandler.predictionsVector}")

    logger.info(s"Total number of rounds: ${stateHandler.totalNumberOfRounds}")

    if (receiveFeedbackBias != 1.0) {
      logger.info(s"\nReceived feedback on ${stateHandler.receivedFeedback} rounds")
    }
  }

}

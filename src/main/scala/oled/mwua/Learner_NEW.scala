package oled.mwua

import akka.actor.Actor
import app.runutils.IOHandling.Source
import app.runutils.RunningOptions
import logic.Examples.Example
import logic.Theory
import org.slf4j.LoggerFactory

class Learner_NEW[T <: Source](val inps: RunningOptions,
                               val trainingDataOptions: T,
                               val testingDataOptions: T,
                               val trainingDataFunction: T => Iterator[Example],
                               val testingDataFunction: T => Iterator[Example],
                               val writeExprmtResultsTo: String = "") extends Actor {
  val learningRate = 1.0

  val epsilon = 0.0005 // used in the randomized version

  val randomizedPrediction = true

  // A rule must make this much % of the total FPs before it is specialized
  val percentOfMistakesBeforeSpecialize = 0

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

        if (batchCounter == 38) {
          val stop = "stop"
        }

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
          ExpertAdviceFunctions.process(withSplice = false, nextBatch, inps, stateHandler,
            trueLabels, learningRate, epsilon, randomizedPrediction, batchCounter, percentOfMistakesBeforeSpecialize)
        }
      }
    }

  }


  val stateHandler = new StateHandler


  def wrapUp() = {
    logger.info(Theory(stateHandler.ensemble.merged(inps).clauses.sortBy(x => -x.w)).showWithStats)
    logger.info(s"Prequential error vector:\n${stateHandler.perBatchError.mkString(",")}")
    logger.info(s"Prequential error vector (Accumulated Error):\n${stateHandler.perBatchError.scanLeft(0.0)(_ + _).tail}")
    logger.info(s"Total TPs: ${stateHandler.totalTPs}, Total FPs: ${stateHandler.totalFPs}, Total FNs: ${stateHandler.totalFNs}, Total TNs: ${stateHandler.totalTNs}")
  }

}

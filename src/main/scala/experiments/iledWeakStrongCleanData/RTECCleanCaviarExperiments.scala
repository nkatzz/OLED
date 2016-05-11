package experiments.iledWeakStrongCleanData


import akka.actor.{ActorSystem, Props, Actor}

import akka.pattern.ask
import akka.util.Timeout
import scala.concurrent.{Await, ExecutionContext, Future}
import scala.concurrent.duration._

import com.mongodb.casbah.Imports._
import com.typesafe.scalalogging.LazyLogging
import iled.core.Core
import iled.structures.Examples.Example
import iled.structures.{Theory, Clause, PriorTheory}
import iled.utils.{Utils, Database}
import iled.utils.Utils._
import iled.core.Iled._

import scala.util.{Failure, Success, Try}
import jep.Jep

/**
  * Created by nkatz on 11/30/15.
  */


/**
  *
  *
  * ----------------------------------------------
  * This revisits the historical memory only once
  * after each example has been seen once
  * ----------------------------------------------
  *
  *
  */

object RTECCleanCaviarExperiments {

  class DataSet(val trainingSet: List[(Int, Int)], val testingSet: List[(Int, Int)])

  val allHLEs = List("moving", "meeting", "fighting", "leavingObject")
  val step = 40
  //val withBacktr = false


  def main(args: Array[String]): Unit = {
    val DB = new Database(Core.fromDB)

    val repeatFor = 1

    val HLE = "meeting"// args(0)
    val pruningPercent = 5// args(1).toInt

    val batchSize = 10
    val withBacktr = false
    val weaksSelectionStrategy = Right(50)

    ///*
    val datasets = (for (i <- 1 to repeatFor) yield getTrainingData(HLE)).toList

    //val datasets = List(new DataSet(trainingSet = List((680, 600000)), testingSet = List((600000, 1006880))))

    val system = ActorSystem("ExperimentsRunningActor")
    val actor = system.actorOf(Props(new Collector(DB, HLE, batchSize, withBacktr, weaksSelectionStrategy, pruningPercent, datasets)), name = "ExperimentsColectorActor")
    //*/
    //runSequential(DB,HLE,withWeaks=false,batchSize,withBacktr)
  }

  def runSequential(DB: Database, HLE: String, withWeaks: Boolean,
                    batchSize: Int, withBacktr: Boolean = false,
                    weaksSelectionStrategy: Either[String, Int] = Right(50),
                    pruningPercent: Int = 10, trainingPercent: Int = 10, jep: Jep) = {

    val data = new DataSet(
      trainingSet =
        List((834920,835600), (732600,745040), (718200,721600), (121600,125000), (652560,655960), (491520,492160), (701120,704520), (447240,450640), (931400,934800), (508400,511800), (533760,536360), (333720,337120), (901920,905320), (295360,298760), (247200,250600), (897560,898480), (70000,73400), (6200,6800), (806880,810280), (968960,969800), (728520,730320), (426600,430000))
      ,
      testingSet =
        List((834920,835600), (732600,745040), (718200,721600), (121600,125000), (652560,655960), (491520,492160), (701120,704520), (447240,450640), (931400,934800), (508400,511800), (533760,536360), (333720,337120), (901920,905320), (295360,298760), (247200,250600), (897560,898480), (70000,73400), (6200,6800), (806880,810280), (968960,969800), (728520,730320), (426600,430000))
    )
    val system = ActorSystem("Test")
    val actor = system.actorOf(Props(
      new Runner(DB = DB, HLE = HLE, withWeaks = withWeaks,
      batchSize = batchSize, withBacktr = withBacktr,
      weaksSelectionStrategy = weaksSelectionStrategy,
      pruningPercent = pruningPercent, data = data, jep=jep)), name = "TestRunner")

    actor ! "go"

    //hle.collectKernels.foreach(x => println(x.tostring))
  }




  class Collector(DB: Database, HLE: String,
                  batchSize: Int, withBacktr: Boolean = false,
                  weaksSelectionStrategy: Either[String, Int] = Right(50),
                  pruningPercent: Int = 10, datasets: List[DataSet]) extends Actor {


    var size = 2 * datasets.size // total # of runs, 2 per dataset (1 for weak, 1 for strong)
    var resultsCollector = scala.collection.mutable.Map[String, List[ResultsBean]]()
    for (d <- datasets) {

      context.actorOf(Props(
        new Runner(DB = DB, HLE = HLE, withWeaks = true,
          batchSize = batchSize, withBacktr = withBacktr,
          weaksSelectionStrategy = weaksSelectionStrategy,
          pruningPercent = pruningPercent, data = d, jep = new Jep())
      ), name = s"Weaks-${d.hashCode()}-Actor") ! "go"

      context.actorOf(Props(
        new Runner(DB = DB, HLE = HLE, withWeaks = false,
          batchSize = batchSize, withBacktr = withBacktr,
          weaksSelectionStrategy = weaksSelectionStrategy,
          pruningPercent = pruningPercent, data = d, jep = new Jep())
      ),name = s"Strongs-${d.hashCode()}-Actor") ! "go"
    }

    def receive = {
      case z: scala.collection.mutable.Map[String, ResultsBean] =>
        val what = z.keySet.head
        if (resultsCollector.keySet.contains(what)) resultsCollector(what) = resultsCollector(what) :+ z(what)
        else resultsCollector(what) = List(z(what))
        println(s"Finished $what learning with $HLE")
        println(s"precision: ${z(what).precision} recall: ${z(what).recall} f-score: ${z(what).fscore}")
        size -= 1
        if (size == 0) {
          val strongResults = new AggregatedResultsBean(resultsCollector("strong"))
          val weakResults = new AggregatedResultsBean(resultsCollector("weak"))
          println(strongResults.show)
          println(weakResults.show)
        }
    }
  }



  // trainingPercent is the % of total number of intervals for this HLE used as positive examples
  class Runner(val DB: Database, val HLE: String, val withWeaks: Boolean,
               val batchSize: Int, val withBacktr: Boolean = false,
               val weaksSelectionStrategy: Either[String, Int] = Right(50),
               val pruningPercent: Int, val data: DataSet, jep: Jep) extends Actor with LazyLogging {

    val pruningThreshold: Either[(String, Int), Int] = Left((HLE, pruningPercent))
    val trainingSet = data.trainingSet
    val testingSet = data.testingSet
    val weakStrong = if (withWeaks) "weak" else "strong"

    def receive = {
      case "go" =>
        sender ! this.results
    }

    def results = scala.collection.mutable.Map[String, ResultsBean](weakStrong -> runIled)

    // The training set is given as a set of pairs of start-end points
    // For each such pair (s,e) we fetch data from s-step to e+step
    def runIled = {
      logger.info(s"${lined("Running configuration:")} \n" + (for ((k, v) <- Core.glvalues) yield s"$k: $v").mkString("\n"))
      println(s"Starting $weakStrong learning with ${this.HLE} on \n training set: ${this.trainingSet} \n testing set: ${this.testingSet}")
      val forwardPass = this.trainingSet.foldLeft((List[Example](), new PriorTheory(), List[Double]())) {
        (x, newInterval) =>
          val pastExs = x._1
          val theory = x._2
          val times = x._3
          val (exmpls, hypothesis, totalTime) = runOnInterval(theory, newInterval, withWeaks, jep)
          (pastExs ++ exmpls, hypothesis, times :+ totalTime)
      }
      val seenExmpls = forwardPass._1
      val theory = forwardPass._2
      var times = forwardPass._3
      val backwardPass = time {
        goBack(theory, seenExmpls, jep=jep)
      }
      val finalTheory = backwardPass._1
      times = times :+ backwardPass._2
      val totalTime = times.sum
      val (hypothesisSize,tps,fps,fns,precision,recall,f_score,_) =
        if (!withWeaks) wrapUp(DB = DB, theory = finalTheory, totalTime = times.sum, testingSet = this.testingSet, jep=jep)
        else pruneBadWeaks(seenExmpls, finalTheory.merge, DB, totalTime, pruningThreshold, testingSet = this.testingSet, jep=jep)
      this.jep.close()
      new ResultsBean(theory=finalTheory.merge,theorySize=hypothesisSize, tps=tps,fps=fps,fns=fns,precision=precision,recall=recall,fscore=f_score,times=totalTime)
    }

    // This runs on a single interval (s,e). It backtracks on past examples of
    // the current interval according to the global 'withBacktr' parameter.
    def runOnInterval(pt: PriorTheory, currentInterval: (Int, Int), withWeaks: Boolean, jep: Jep): (List[Example], PriorTheory, Double) = {
      val batches = DB.getBatches(currentInterval._1, currentInterval._2, step, batchSize, withWeaks)
      val result =
        Try {
          time {
            batches.foldLeft((List[Example](), pt)) {
              (x, newBatch) =>
                val pastExs = x._1
                val theory = x._2
                val (_seenExs, _theory, _time) = withWeaks match {
                  case true =>
                    iledTop(pastExs, Example.merge(newBatch.examples),
                      theory, weakBatch = newBatch, withBacktr = withBacktr,
                      weaksSelectionStrategy = weaksSelectionStrategy, jep=jep)
                  case false => iledTop(pastExs, newBatch.asSingleExmpl, theory, withBacktr = withBacktr, jep=jep)
                }
                (_seenExs, _theory)
            }
          }
        }

      val (exmpls, hypothesis, totalTime) = result match {
        case Success(out) => (out._1._1, out._1._2, out._2)
        case Failure(ex) =>
          println(s"$ex\nTraining set:\n${this.trainingSet}\nWith weaks: $withWeaks")
          (Nil, new PriorTheory, 0.0)
      }
      (exmpls, hypothesis, totalTime)
    }

    def collectKernels = {
      def _collectKernels(interval: (Int, Int)) = {
        val batches = DB.getBatches(interval._1, interval._2, step, batchSize, usingWeakExmpls = false)
        val clauses = batches.foldLeft(List[Clause]()) {
          (x, newBatch) =>
            val (kernel, varKernel) = Utils.generateKernel(newBatch.asSingleExmpl.toMapASP, jep=jep)
            println(Theory(kernel).tostring)
            x ++ varKernel
        }
        clauses
      }
      this.trainingSet.foldLeft(List[Clause]()) {
        (x, y) =>
          val k = _collectKernels(y)
          x ++ k
      }
    }
  }


  def getTrainingData(HLE: String, trainingPercent: Int = 10) = {
    // Returns all intervals for the particular hle
    def getHLEIntervals(hle: String) = {
      (RTEC_CAVIAR_Annotation.HLEs(hle) map { case (_, v) => v } toList).flatten
    }
    val otherHLES = allHLEs filter (_ != HLE)
    val thisHLEintervals = getHLEIntervals(HLE)
    val trainingSize = scala.math.ceil(trainingPercent / 100.0 * thisHLEintervals.size) toInt
    //val positives = iled.utils.Utils.sampleN(trainingSize, thisHLEintervals) map (_.asInstanceOf[(Int, Int)])
    // select only two intervals to see how it goes
    val positives = iled.utils.Utils.sampleN(2, thisHLEintervals) map (_.asInstanceOf[(Int, Int)])
    val rest = thisHLEintervals filter (!positives.contains(_)) // these will be used in the testing set
    // select intervals of negative examples to add to the training set, such that the total number of positives in the training set amounts
    // to 10% of the negatives. This (empirically and very very roughly) approaches the actual distribution of positives over negatives in the CAVIAR dataset.
    val totalPositiveSize = positives.foldLeft(0)((x, y) => {
        val size = (y._2 - y._1) / step
        x + size + 1
      })
    // Select 2 intervals of size 10*totalPositiveSize, disjoint to the HLE positive intervals
    // (they are selected from the downtime intervals).
    // These will be the negatives for the training and the testing set respectively.
    val HLEIntervals = otherHLES flatMap getHLEIntervals
    val negativeIntervals = HLEIntervals ++ RTEC_CAVIAR_Annotation.downTimeIntervals
    // 85 is the average interval size.
    val trainingNegatives = Utils.sampleN(10 * positives.length, negativeIntervals).asInstanceOf[List[(Int, Int)]]
    val testingNegatives = Utils.sampleN(10 * positives.length, negativeIntervals.filter(x => !trainingNegatives.contains(x))).asInstanceOf[List[(Int, Int)]]
    val trainingSet = positives ++ trainingNegatives
    val testingSet = rest ++ testingNegatives
    new DataSet(trainingSet, testingSet)
  }


  // container class
  class ResultsBean(val theory: Theory, val theorySize: Double, val tps: Double,
                    val fps: Double, val fns: Double, val precision: Double,
                    val recall: Double, val fscore: Double, val times: Double) {

    val showHypothesis = theory.tostring

    val show = s"\nFinal Hypothesis:\n $showHypothesis" +
      s"\n\nHypothesis size: $theorySize\n" +
      s"Total time (secs): $times \n" +
      s"${lined("Crossvalidation")}\n" +
      s"tps: $tps\n" +
      s"fps: $fps\n" +
      s"fns: $fns\n" +
      s"precision: $precision\n" +
      s"recall: $recall\n" +
      s"f-score: $fscore"
  }

  class AggregatedResultsBean(resultBeans: List[ResultsBean]) {

    def g = (x: List[Double]) => {
      val m = Utils.mean(x)
      val d = Utils.deviation(x, m)
      (m, d)
    }

    val (tps,fps,fns,precision,recall,fscore,times,theorySize) = {
      val z = resultBeans.foldLeft(List[Double](), List[Double](), List[Double](), List[Double](), List[Double](), List[Double](), List[Double](), List[Double]()) {
        (x, y) =>
          val (_tps, _fps, _fns, _precision, _recall, _fscore, _times, _size) = (x._1, x._2, x._3, x._4, x._5, x._6, x._7, x._8)
          val (__tps, __fps, __fns, __precision, __recall, __fscore, __times, __size) = (y.tps, y.fps, y.fns, y.precision, y.recall, y.fscore, y.times, y.theorySize)
          (_tps :+ __tps, _fps :+ __fps, _fns :+ __fns, _precision :+ __precision, _recall :+ __recall, _fscore :+ __fscore, _times :+ __times, _size :+ __size)
      }
      (g(z._1),g(z._2),g(z._3),g(z._4),g(z._5),g(z._6),g(z._7),g(z._8))
    }

    val show =
      s"\ntps: ${tps._1} (+/- ${tps._2})" +
      s"\nfps: ${fps._1} (+/- ${fps._2})" +
      s"\nfns: ${fns._1} (+/- ${fns._2})" +
      s"\nprecision: ${precision._1} (+/- ${precision._2})" +
      s"\nrecall: ${recall._1} (+/- ${recall._2})" +
      s"\nf-score: ${fscore._1} (+/- ${fscore._2})" +
      s"\nsize: ${theorySize._1} (+/- ${theorySize._2})" +
      s"\ntime: ${times._1} (+/- ${times._2})"

  }




}

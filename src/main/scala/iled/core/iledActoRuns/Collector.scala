package iled.core.iledActoRuns

import akka.actor.{Actor, Props}
import iled.structures.Theory
import iled.utils.Utils._
import iled.utils.{Utils, Database}
import jep.Jep

/**
  * Created by nkatz on 12/20/15.
  */

class DataSet(val trainingSet: List[(Int, Int)], val testingSet: List[(Int, Int)])

class Collector(DB: Database, HLE: String,
                batchSize: Int, withBacktr: Boolean = false,
                weaksSelectionStrategy: Either[String, Int] = Right(50),
                pruningPercent: Int = 10, step: Int, datasets: List[DataSet], runWithWeaksAlso: Boolean, runWithStrongs: Boolean) extends Actor {

  var size = if(runWithWeaksAlso && runWithStrongs) 2 * datasets.size else datasets.size // total # of runs, 2 per dataset (1 for weak, 1 for strong)
  var resultsCollector = scala.collection.mutable.Map[String, List[ResultsBean]]()
  for (d <- datasets) {
    if (runWithWeaksAlso) {
      context.actorOf(Props(
        new Runner(DB = DB, HLE = HLE, withWeaks = true,
          batchSize = batchSize, withBacktr = withBacktr,
          weaksSelectionStrategy = weaksSelectionStrategy,
          pruningPercent = pruningPercent, data = d, jep = new Jep(), step = step)
      ), name = s"Weaks-${d.hashCode()}-Actor") ! "go"
    }

    if (runWithStrongs) {
      context.actorOf(Props(
        new Runner(DB = DB, HLE = HLE, withWeaks = false,
          batchSize = batchSize, withBacktr = withBacktr,
          weaksSelectionStrategy = weaksSelectionStrategy,
          pruningPercent = pruningPercent, data = d, jep = new Jep(), step = step)
      ), name = s"Strongs-${d.hashCode()}-Actor") ! "go"
    }
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

class ResultsBean(val theory: Theory, val theorySize: Double, val tps: Double,
                  val fps: Double, val fns: Double, val precision: Double,
                  val recall: Double, val fscore: Double, val times: Double) {

  val showHypothesis = theory.tostring
  val show = s"\nFinal Hypothesis:\n $showHypothesis" + s"\n\nHypothesis size: $theorySize\n" + s"Total time (secs): $times \n" +
    s"${lined("Crossvalidation")}\n" + s"tps: $tps\n" + s"fps: $fps\n" + s"fns: $fns\n" + s"precision: $precision\n" +
    s"recall: $recall\n" + s"f-score: $fscore"
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

  val show = s"\ntps: ${tps._1} (+/- ${tps._2})" + s"\nfps: ${fps._1} (+/- ${fps._2})" + s"\nfns: ${fns._1} (+/- ${fns._2})" +
    s"\nprecision: ${precision._1} (+/- ${precision._2})" + s"\nrecall: ${recall._1} (+/- ${recall._2})" + s"\nf-score: ${fscore._1} (+/- ${fscore._2})" +
    s"\nsize: ${theorySize._1} (+/- ${theorySize._2})" + s"\ntime: ${times._1} (+/- ${times._2})"

}

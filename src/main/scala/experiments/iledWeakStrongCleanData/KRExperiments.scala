package experiments.iledWeakStrongCleanData

import akka.actor.{Props, ActorSystem, Actor}
import com.typesafe.scalalogging.LazyLogging
import experiments.datagen.CaviarDataGenerator
import iled.core.Iled._
import iled.core.{Core, Crossvalidation, IledStreaming}
import iled.structures.Examples.{Example, ExampleBatch}
import iled.structures.{PriorTheory, Clause, Theory}
import iled.utils.{MongoUtils, Utils, Database}
import scala.concurrent.future
import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global
import scala.annotation.tailrec

import scala.util.{Try,Success,Failure,Random}

/**
  * Created by nkatz on 11/8/15.
  */


object KRExperiments extends MongoUtils with LazyLogging {

/*

  val path = Utils.createOrClearFile("/home/nkatz/Desktop/rrrrs")

  def main(args: Array[String]): Unit = {
    runExperiment(batchSize = 10,posPercentPerInterval = 20)
  }

  def runExperiment(batchSize: Int, posPercentPerInterval:Int) = {
    val system = ActorSystem("RunExperiment")
    val actor = system.actorOf(Props(new RunExperiment(batchSize, posPercentPerInterval)), name = s"RunExperiment")
    actor ! "go"
  }



  def _run_(db: Database,batchSize: Int, trainingSetSize: Int, startTimes: List[Int], what: String) = {
    val system = ActorSystem("RunWeak")
    val actor = system.actorOf(Props(new Experiment(db, batchSize, trainingSetSize, startTimes, what)))
    actor ! "go"
  }




  class RunExperiment(batchSize: Int, posPercentPerInterval:Int) extends Actor {
    val dbname = CaviarDataGenerator.generateForExperiments("iddddddddddd",posPercentPerInterval)
    val db = new Database(dbname,"examples")
    def receive = {
      case "go" =>
        //for (trainingSize <- List(1000,2000,3000,4000,5000)) {
        for (trainingSize <- List(1000,2000,3000,4000,5000)) {
        //for (trainingSize <- List(200,500,1000, 2000)) {
          val startTimes = Utils.sampleN(5,List.range(1,db.size-trainingSize+1))
          _run_(db, batchSize, trainingSize,startTimes.asInstanceOf[List[Int]],"weak")
          //_run_(db, batchSize, trainingSize,startTimes.asInstanceOf[List[Int]],"strong")
        }
    }
  }


  class Experiment(db: Database,batchSize: Int, trainingSetSize: Int,startTimes: List[Int], what: String) extends Actor {
    val withBacktr = true
    val withWeaks = if (what == "weak") true else false
    val weaksSelectionStrategy = Right(50)
    Core.glvalues("withWeaks") = withWeaks.toString
    Core.glvalues("withBacktr") = withBacktr.toString
    def receive = {
      case "go" =>
        val msg = if (withWeaks) "weak" else "strong"
        println(s"Starting $msg training size: $trainingSetSize")
        //val f = Future.sequence(go(db,batchSize,trainingSetSize,withBacktr,withWeaks,weaksSelectionStrategy))

        val f = go(db,batchSize,trainingSetSize,withBacktr,withWeaks,weaksSelectionStrategy,startTimes, what)
        Utils.writeLine(s"\nDB: ${db.name}, Training size: ${f._9}"+s"\n${f._10} results:" +
          s"\ntps: ${f._2._1} (+/- ${f._2._2})" +
          s"\nfps: ${f._3._1} (+/- ${f._3._2})" +
          s"\nfns: ${f._4._1} (+/- ${f._4._2})" +
          s"\nprecision: ${f._5._1} (+/- ${f._5._2})" +
          s"\nrecall: ${f._6._1} (+/- ${f._6._2})" +
          s"\nf-score: ${f._7._1} (+/- ${f._7._2})" +
          s"\nsize: ${f._1._1} (+/- ${f._1._2})" +
          s"\ntime: ${f._8._1} (+/- ${f._8._2})",path,"append")

    }
  }


  def go(db: Database,batchSize: Int, trainingSetSize: Int, withBacktr: Boolean, withWeaks:Boolean,
         weaksSelectionStrategy: Either[String,Int],startTimes: List[Int], what: String) = {

    def g = (x:List[Double]) => {
      val m = Utils.mean(x)
      val d = Utils.deviation(x,m)
      (m,d)
    }
    // repeat runSinle 5 times and get averages

    println(s"training size: $trainingSetSize, start times: $startTimes")
    val a = startTimes.par.foldLeft(List[Double](),List[Double](),List[Double](),List[Double](),List[Double](),List[Double](),List[Double](),List[Double]()){
      (x,y) =>
        val res = runSingle(db,batchSize,trainingSetSize,withBacktr,withWeaks,weaksSelectionStrategy,y)
        (x._1:+res._1, x._2:+res._2, x._3:+res._3, x._4:+res._4, x._5:+res._5, x._6:+res._6, x._7:+res._7, x._8:+res._8)
    }
    ( g(a._1.map(x=>x.toDouble)), g(a._2), g(a._3), g(a._4), g(a._5), g(a._6), g(a._7), g(a._8),  trainingSetSize, what)
  }




  def runSingle(db: Database,batchSize: Int,
                trainingSetSize: Int,
                withBacktr: Boolean,
                withWeaks:Boolean,
                weaksSelectionStrategy: Either[String,Int],
                startTime: Int) = {

    def runTry(db: Database,batchSize: Int, trainingSetSize: Int, withBacktr: Boolean,
               withWeaks:Boolean, weaksSelectionStrategy: Either[String,Int], startTime: Int) = {
      val result = Try(run(db,batchSize,trainingSetSize,withBacktr,withWeaks,weaksSelectionStrategy,startTime))

      val out = result match {
        case Success(x) => x
        case Failure(_) => (0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0)
      }

      out
    }

    val result = runTry(db,batchSize,trainingSetSize,withBacktr,withWeaks,weaksSelectionStrategy,startTime)

    val out = result match {
      case (0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0) =>
        var done = false
        var x = (1000.0,1000.0,1000.0,1000.0,1000.0,1000.0,1000.0,1000.0)
        while (!done){
          println("Failed, trying a different batch")
          val newStratTime = Utils.sampleN(1,List.range(1,db.size-trainingSetSize+1))
          x = runTry(db,batchSize,trainingSetSize,withBacktr,withWeaks,weaksSelectionStrategy,newStratTime.head.asInstanceOf[Int])
          if (x != (0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0)) done = true
        }
        x
      case _ => result
    }
    out

  }

  /*
  class RunAll extends Actor {
    def receive = {
      case "go" =>
        for (x <- List(20,50,80)) {
          runExperiment(batchSize = 10,posPercentPerInterval = x)
        }
    }
  }
*/

/*
  println(s"\nDB: ${db.name}, Training size: $trainingSetSize"+"\nWeak results:" + s"\ntps: ${tps}" +
    s"\nfps: ${fps}" + s"\nfns: ${fns}" + s"\nprecision: ${precision}" +
    s"\nrecall: ${recall}" + s"\nf-score: ${f_score}" + s"\nsize: ${hypothesisSize}")
*/








  // Here we work with the containedExamples field, which is a set of
  // Example instances. Each such instance consists of pairs of consecutive
  // examples, representing a potentially weak example. The annotation from the first example
  // of each pair has been left out, to enforce learning from this example,
  // instead of accounting for its annotation via the BK (inertia).
  // The way we proceed is thus as follows:
  // We loop over batch.containedExamples, learning something from each weak examples.
  // At each iteration, we add each weak example to the past examples set,
  // after we add back its suppressed annotation, in order to be used for inference.
  // We also mark each freshly generated rule as weak or strong, depending on whether
  // it is generated from a weak or strong example respectively.

  def sampleN(N: Int, sampleFrom: List[Int], byStep: Int = 0): List[Int] = {
    @tailrec
    def sampleN(N: Int, sampleFrom: List[Int], sample: List[Int], byStep: Int, counter: Int): List[Int] = {
      sample.length match {
        case N => sample
        case _ =>
          val splitOk = byStep > 0 && sampleFrom.length / byStep > N
          //val test = (_sample:) if (byStep)
          val newCounter = counter + byStep
          val range = splitOk match {
            case false =>
              sampleFrom.head to sampleFrom.tail.reverse.head
            case _ =>
              counter to newCounter
          }
          val newValue = range(Random.nextInt(range length))
          //val newValue = Random.shuffle(sampleFrom).head
          val newSample =
            if (!sample.contains(newValue))
              sample :+ newValue
            else sample
          sampleN(N, sampleFrom, newSample, byStep, newCounter)
      }
    }
    sampleN(N, sampleFrom, List(), byStep, 0)
  }



*/


}
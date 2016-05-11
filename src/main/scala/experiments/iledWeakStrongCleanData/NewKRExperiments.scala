package experiments.iledWeakStrongCleanData

import com.typesafe.scalalogging.LazyLogging
import experiments.datagen.CaviarDataGenerator
import iled.core.Iled
import iled.core.Iled._
import iled.utils.{Database, Utils, MongoUtils}

import akka.actor.{Props, ActorSystem, Actor}

import scala.util.{Failure, Success, Try}

/**
  * Created by nkatz on 11/23/15.
  */
object NewKRExperiments extends MongoUtils with LazyLogging {

/*

  val path = Utils.createOrClearFile("/home/nkatz/Desktop/rrrrs")

  def main(args: Array[String]): Unit = {
    //posPercentPerInterval is applied to the training set construction
    runExperiment(batchSize = 10, posPercentPerInterval = 50)
  }

  def runExperiment(batchSize: Int, posPercentPerInterval: Int) = {
    val system = ActorSystem("RunExperiment")
    val actor = system.actorOf(Props(new RunExperiment(batchSize, posPercentPerInterval)), name = s"RunExperiment")
    actor ! "go"
  }

  class RunExperiment(batchSize: Int, posPercentPerInterval: Int) extends Actor {
    val dbname = CaviarDataGenerator.generateForExperiments("iddddddddddd", posPercentPerInterval)
    //val dbname = "CAVIAR_SYNTH-50%-strong-id"
    val db = new Database(dbname, "examples")

    def receive = {
      case "go" =>
        for (trainingSize <- List(1000,2000,3000,4000,5000,10000)) {
        //for (trainingSize <- List(2000)) {
          val startTimes = Utils.sampleN(5, List.range(1, db.size - trainingSize + 1))
          //val startTimes = List(0,1000,2000,3000,4000)
          context.actorOf(Props(new ResultsCollector(db, batchSize, trainingSize, startTimes.asInstanceOf[List[Int]], "weak")))
          context.actorOf(Props(new ResultsCollector(db, batchSize, trainingSize, startTimes.asInstanceOf[List[Int]], "strong")))
        }
    }
  }

  class ResultsCollector(db: Database, batchSize: Int, trainingSetSize: Int, startTimes: List[Int], what: String) extends Actor {
    val signature = s"db: ${db.name} training size: $trainingSetSize $what"
    val withBacktr = true
    val withWeaks = if (what == "weak") true else false
    val weaksSelectionStrategy = Right(50)
    var results: List[(Double, Double, Double, Double, Double, Double, Double, Double)] = Nil
    var size = startTimes.size

    for (time <- startTimes) {
      context.actorOf(Props(new Learner(db, batchSize, trainingSetSize, withBacktr, withWeaks, weaksSelectionStrategy))) ! time
    }

    def receive = {
      case (hyp: Double, tps: Double, fps: Double, fns: Double, precision: Double, recall: Double, f_score: Double, totalTime: Double) =>
        results = results :+(hyp, tps, fps, fns, precision, recall, f_score, totalTime)
        size -= 1
        if (size == 0) {
          //context.system.shutdown()
          println(s"Done with $signature")
          val f = results.map { x => List(x._1, x._2, x._3, x._4, x._5, x._6, x._7, x._8) }.transpose.map(p => g(p))
          Utils.writeLine(s"\nDB: ${db.name}, Training size: $trainingSetSize" + s"\n$what results:" +
            s"\ntps: ${f(1)._1} (+/- ${f(1)._2})" +
            s"\nfps: ${f(2)._1} (+/- ${f(2)._2})" +
            s"\nfns: ${f(3)._1} (+/- ${f(3)._2})" +
            s"\nprecision: ${f(4)._1} (+/- ${f(4)._2})" +
            s"\nrecall: ${f(5)._1} (+/- ${f(5)._2})" +
            s"\nf-score: ${f(6)._1} (+/- ${f(6)._2})" +
            s"\nsize: ${f(0)._1} (+/- ${f(0)._2})" +
            s"\ntime: ${f(7)._1} (+/- ${f(7)._2})", path, "append")
        }
    }

  }

  def g = (x: List[Double]) => {
    val m = Utils.mean(x)
    val d = Utils.deviation(x, m)
    (m, d)
  }

  class Learner(db: Database, batchSize: Int, trainingSetSize: Int,
                withBacktr: Boolean, withWeaks: Boolean,
                weaksSelectionStrategy: Either[String, Int]) extends Actor {


    def receive = {
      case time: Int =>
        println(s"starting (training size: $trainingSetSize, weaks:$withWeaks, from time: $time )")
        sender ! runOne(db = this.db, batchSize = this.batchSize,
          trainingSetSize=this.trainingSetSize, withBacktr=this.withBacktr,
          withWeaks=this.withWeaks, weaksSelectionStrategy=this.weaksSelectionStrategy, startTime=time)
    }
  }

  def runOne(db: Database,
             batchSize: Int,
             trainingSetSize: Int,
             withBacktr: Boolean,
             withWeaks: Boolean,
             weaksSelectionStrategy: Either[String, Int],
             startTime: Int) = {

    def runTry(d: Database, bs: Int, tss: Int, wb: Boolean, ww: Boolean, wss: Either[String, Int], st: Int) = {
      val result = Try(Iled.run(DB=d, batchSize=bs, trainingSetSize=tss, withBacktr=wb, withWeaks=ww, weaksSelectionStrategy=wss, startTime=st))
      val out = result match {
        case Success(x) => x
        case Failure(f) =>
          println(f)
          //println(f.printStackTrace())
          (0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0)
      }
      out
    }

    val result = runTry(db, batchSize, trainingSetSize, withBacktr, withWeaks, weaksSelectionStrategy, startTime)
    val out = result match {
      case (0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0) =>
        var done = false
        var x = (1000.0, 1000.0, 1000.0, 1000.0, 1000.0, 1000.0, 1000.0, 1000.0)
        while (!done) {
          val newStartTime = Utils.sampleN(1, List.range(1, db.size - trainingSetSize + 1))
          println(s"Failed (startTime: $startTime, training size: $trainingSetSize withWeaks: $withWeaks, db: ${db.name}), trying a different batch with startTime: $newStartTime")
          x = runTry(db, batchSize, trainingSetSize, withBacktr, withWeaks, weaksSelectionStrategy, newStartTime.head.asInstanceOf[Int])
          if (x !=(0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0)) done = true
        }
        x
      case _ => result
    }
    out
  }


*/


}

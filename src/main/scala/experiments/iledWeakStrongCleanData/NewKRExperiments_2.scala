package experiments.iledWeakStrongCleanData

import akka.actor.{Props, Actor, ActorSystem}
import com.mongodb.casbah.Imports._
import experiments.datagen.CaviarDataGenerator
import iled.core.Iled
import iled.utils.{Utils, Database}

import scala.util.{Failure, Success, Try}

/**
  * Created by nkatz on 11/25/15.
  */
object NewKRExperiments_2 {

/*

  var allDBs = Map

  def main(args: Array[String]): Unit = {
    //posPercentPerInterval is applied to the training set construction
    runExperiment(batchSize = 10, posPercentPerInterval = 50)


    /*use this to drop all dbs if things go wrong
    val mongoClient = MongoClient()
    val all = mongoClient.databaseNames.filter(x => x.contains("CAVIAR_SYNTH-50%-strong") && !x.contains("id"))
    all.foreach(x => mongoClient.dropDatabase(x))
    mongoClient.close()
    */
  }

  def runExperiment(batchSize: Int, posPercentPerInterval: Int) = {
    val system = ActorSystem("RunExperiment")
    val actor = system.actorOf(Props(new RunExperiment(batchSize, posPercentPerInterval)), name = s"RunExperiment")
    actor ! "go"
  }

  class RunExperiment(batchSize: Int, posPercentPerInterval: Int) extends Actor {
    def receive = {
      case "go" =>
        val dbs = List.range(1,50).map( _ => generateDB(posPercentPerInterval) )
        //val dbs = List(1).map( _ => generateDB(posPercentPerInterval) )
        for (trainingSize <- List(1000, 2000, 3000, 4000)) {
        //for (trainingSize <- List(4000)) {
          context.actorOf(Props(new ResultsCollector(dbs, batchSize, trainingSize, posPercentPerInterval, "weak")))
          context.actorOf(Props(new ResultsCollector(dbs, batchSize, trainingSize, posPercentPerInterval, "strong")))
        }

    }
  }


  class ResultsCollector(dbs: List[Database], batchSize: Int, trainingSetSize: Int, posPercentPerInterval: Int, what: String) extends Actor {
    val withBacktr = true
    val withWeaks = if (what == "weak") true else false
    val weaksSelectionStrategy = Right(50)
    var results: List[(Double, Double, Double, Double, Double, Double, Double, Double)] = Nil
    var size = dbs.size

    for (db <- dbs) {
      context.actorOf(Props(new Learner(db, batchSize, trainingSetSize, withBacktr, withWeaks, weaksSelectionStrategy))) ! "go"
    }

    def receive = {
      case (hyp: Double, tps: Double, fps: Double, fns: Double, precision: Double, recall: Double, f_score: Double, totalTime: Double) =>
        results = results :+(hyp, tps, fps, fns, precision, recall, f_score, totalTime)
        size -= 1
        if (size == 0) {
          //context.system.shutdown()
          println(s"Done with training size: $trainingSetSize, $what\n used dbs:\n ${dbs.map(x => x.name).mkString("\n")}")
          val f = results.map { x => List(x._1, x._2, x._3, x._4, x._5, x._6, x._7, x._8) }.transpose.map(p => g(p))
          val path = Utils.createOrClearFile(s"/home/nkatz/Desktop/KR-Paper-experiment-results/$trainingSetSize")
          //Utils.writeLine(s"\nTraining size: $trainingSetSize" + s"\n$what results:" +
          println(s"\nTraining size: $trainingSetSize" + s"\n$what results:" +
            s"\ntps: ${f(1)._1} (+/- ${f(1)._2})" +
            s"\nfps: ${f(2)._1} (+/- ${f(2)._2})" +
            s"\nfns: ${f(3)._1} (+/- ${f(3)._2})" +
            s"\nprecision: ${f(4)._1} (+/- ${f(4)._2})" +
            s"\nrecall: ${f(5)._1} (+/- ${f(5)._2})" +
            s"\nf-score: ${f(6)._1} (+/- ${f(6)._2})" +
            s"\nsize: ${f(0)._1} (+/- ${f(0)._2})" +
            s"\ntime: ${f(7)._1} (+/- ${f(7)._2})", path, "append")
          // drop the dbs:
          //closeAndDropDBs(usedDBs)
        }
    }

  }



  class Learner(db: Database, batchSize: Int, trainingSetSize: Int,
                withBacktr: Boolean, withWeaks: Boolean,
                weaksSelectionStrategy: Either[String, Int]) extends Actor {
    def receive = {
      //case time: Int =>
      case "go" =>
        println(s"starting (db: ${db.name} training size: $trainingSetSize, weaks:$withWeaks)")
        sender ! runOne(db = this.db, batchSize = this.batchSize,
          trainingSetSize=this.trainingSetSize, withBacktr=this.withBacktr,
          withWeaks=this.withWeaks, weaksSelectionStrategy=this.weaksSelectionStrategy, startTime=0)
    }
  }


  def closeAndDropDBs(dbs: List[Database]): Unit = {
    dbs foreach {x => x.close()}
    val mongoClient = MongoClient()
    dbs foreach {x => mongoClient.dropDatabase(x.name)}
    mongoClient.close()
  }

  def generateDB(posPercentPerInterval: Int) = {
    val id = java.util.UUID.randomUUID.toString
    val dbname = CaviarDataGenerator.generateForExperiments(id, posPercentPerInterval)
    val db = new Database(dbname, "examples")
    db
  }

  def g = (x: List[Double]) => {
    val m = Utils.mean(x)
    val d = Utils.deviation(x, m)
    (m, d)
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

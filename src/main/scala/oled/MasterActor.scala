package oled

import akka.actor.{PoisonPill, Actor, Props}
import com.typesafe.scalalogging.LazyLogging
import app.Globals
import utils.DataUtils.{ResultsContainer, TrainingSet}
import utils._
import scala.collection.mutable.ListBuffer


/**
  * Created by nkatz on 9/14/16.
  */


/**
  * Starts a new new actor for each training-testing set. All tasks run in parallel here
  *
  * @param DB
  * @param delta
  * @param breakTiesThreshold
  * @param postPruningThreshold
  * @param minSeenExmpls
  * @param trainingSetSize
  * @param repeatFor
  * @param chunkSize
  * @param withInertia
  * @param withPostPruning
  * @param trainingData
  * @param HLE
  */
class MasterActor(DB: Database, delta: Double, breakTiesThreshold: Double,postPruningThreshold: Double,
                  minSeenExmpls: Int,trainingSetSize: Int,repeatFor: Int,chunkSize: Int, withInertia: Boolean,
                  withPostPruning: Boolean, onlinePruning: Boolean, trainingData: List[TrainingSet]=Nil,
                  HLE: String, handCraftedTheoryFile: String="", msg: String,
                  globals: Globals, tryMoreRules: Boolean) extends Actor with LazyLogging{

  //val debugTrainingData = trainingData.take(4)
  var remainingTasks =  trainingData.size //debugTrainingData.size //
  println(remainingTasks)
  val results = new ListBuffer[ResultsContainer]

  def g = (x: List[Double]) => {
    val m = Utils.mean(x)
    val d = Utils.deviation(x, m)
    (m, d)
  }

  def receive = {

    case "EvaluateHandCrafted" =>
      for (dataset <- trainingData) {
        //for (dataset <- debugTrainingData) {
        context.actorOf(Props(
          new Dispatcher(DB,delta,breakTiesThreshold,postPruningThreshold,minSeenExmpls,
            trainingSetSize,repeatFor,chunkSize,withInertia,withPostPruning,onlinePruning,dataset,HLE,
            handCraftedTheoryFile, globals=globals, tryMoreRules=tryMoreRules)
        ), name = s"Dispatcher-Actor-${dataset.##}") ! "EvaluateHandCrafted"
      }

    case "start" =>
      for (dataset <- trainingData) {
        //for (dataset <- debugTrainingData) {
        context.actorOf(Props(
          new Dispatcher(DB,delta,breakTiesThreshold,postPruningThreshold,minSeenExmpls,
            trainingSetSize,repeatFor,chunkSize,withInertia,withPostPruning,onlinePruning,dataset,HLE,globals=globals,tryMoreRules=tryMoreRules)
        ), name = s"Dispatcher-Actor-${dataset.##}") ! "start"
      }

    case x: ResultsContainer =>
      results += x
      remainingTasks -= 1
      sender ! PoisonPill
      logger.info(s"Remaining tasks: $remainingTasks")
      if (remainingTasks == 0) {

        logger.info(s"All done!")

        val (tps,fps,fns,precision,recall,fscore,theorySize,times) = {
          val z = results.foldLeft(List[Double](), List[Double](), List[Double](), List[Double](), List[Double](), List[Double](), List[Double](), List[Double]()) {
            (x, y) => (x._1:+y.tps, x._1:+y.fps, x._3:+y.fns, x._4:+y.precision, x._5:+y.recall, x._6:+y.fscore, x._7:+y.theorySize, x._8:+y.time)
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
        println(show)

        Utils.writeLine(show,"/home/nkatz/Desktop/script-results/"+msg,"append")

        //context.stop(self)
        context.system.shutdown()
      }
  }

}


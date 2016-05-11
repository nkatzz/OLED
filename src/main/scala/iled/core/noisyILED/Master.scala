package iled.core.noisyILED

import java.io.File
import java.util.concurrent.CountDownLatch

import akka.actor.{PoisonPill, Actor, Props, ActorSystem}
import akka.util.Timeout
import com.typesafe.scalalogging.LazyLogging
import iled.core.Core
import iled.globalValues.GlobalValues
import iled.utils.CaviarUtils.Interval
import iled.utils._
import scala.collection.mutable.ListBuffer


import akka.pattern.ask
import akka.util.Timeout
import scala.concurrent.{Await, ExecutionContext, Future}
import scala.concurrent.duration._

/**
  * Created by nkatz on 28/2/2016.
  */

object TrainingSet {
  def apply() = {
    new TrainingSet(List(),List())
  }
}
class TrainingSet(val trainingSet: List[Interval], val testingSet: List[Interval])


object Runner extends App{

  val WHOLE_DATA_SET_VALE = 1000000000

  val delta = args(0).toDouble//0.00001
  val postPruningThreshold = args(1).toDouble//0.7
  val minSeenExmpls = args(2).toInt//1000 // break ties for a rule only after it has been evaluated on minSeenExmpls
  val specializationDepth = args(3).toInt//2
  val dataSetNumber = args(4).toInt


  val breakTiesThreshold = 0.05
  val repeatFor = 1     // re-see the data repeatFor
  val chunkSize = 50  // learn from chunks instead of pairs (to speed things up)
  val trainingSetSize = 1500 // WHOLE_DATA_SET_VALE
  val withPostPruning = true
  val withInertia = false
  // !!!!!!!!!!!!!!!!!!!!!!!!!!!
  val HLE = "meeting"
  // !!!!!!!!!!!!!!!!!!!!!!!!!!!-


  val msg = s"δ=$delta-prune=$postPruningThreshold-minseen=$minSeenExmpls-depth=$specializationDepth"

  //val trainingSets = List(MovingTrainingData.allTrainingSets(dataSetNumber))

  val trainingSets = List(MeetingTrainingData.wholeCAVIAR1)

  GlobalValues.glvalues("specializationDepth") = specializationDepth.toString
  // !!!!!!!!!!!!!!!!!!!!!!!!!!!
  val HAND_CRAFTED = if(HLE=="meeting"){
    GlobalValues.HAND_CRAFTED_RULES+"/meeting-hand-crafted.lp"
  } else {
    GlobalValues.HAND_CRAFTED_RULES+"/moving-hand-crafted.lp"
  }


  val DB = new Database(Core.fromDB, "examples")


  val x = CaviarUtils.getDataFromIntervals(DB,HLE,this.trainingSets.head.trainingSet,chunkSize)


  val system = ActorSystem("HoeffdingLearningSystem")
  val latch = new CountDownLatch(1)
  val actor =
    system.actorOf(Props(
      new MasterActor(DB,delta,breakTiesThreshold,postPruningThreshold,minSeenExmpls,
        trainingSetSize,repeatFor,chunkSize,withInertia,withPostPruning,trainingSets,HLE,HAND_CRAFTED, msg)
    ), name = "Master-Actor") !  "start" //"EvaluateHandCrafted"


}




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
                 withPostPruning: Boolean, trainingData: List[TrainingSet]=Nil,
                  HLE: String, handCraftedTheoryFile: String="", msg: String) extends Actor with LazyLogging{

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
            trainingSetSize,repeatFor,chunkSize,withInertia,withPostPruning,dataset,HLE,handCraftedTheoryFile)
        ), name = s"Dispatcher-Actor-${dataset.##}") ! "EvaluateHandCrafted"
      }

    case "start" =>
      for (dataset <- trainingData) {
      //for (dataset <- debugTrainingData) {
        context.actorOf(Props(
          new Dispatcher(DB,delta,breakTiesThreshold,postPruningThreshold,minSeenExmpls,
            trainingSetSize,repeatFor,chunkSize,withInertia,withPostPruning,dataset,HLE)
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


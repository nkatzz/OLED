package all.core.oled

import java.io.File
import java.util.concurrent.CountDownLatch

import akka.actor.{PoisonPill, Actor, Props, ActorSystem}
import akka.util.Timeout
import com.typesafe.scalalogging.LazyLogging
import all.core.Core
import all.globalValues.GlobalValues
import all.utils.CaviarUtils.Interval
import all.utils._
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


object Runner extends App {
  val WHOLE_DATA_SET_VALE = 1000000000

  // example:
  // db=CAVIAR_Real_FixedBorders target=meeting dnum=1 delta=0.00001 prune=0.3 nmin=1000 sdepth=1

  val help = args.contains("--help")
  if (help) {
    println("Input parameters:\n-------------------\ndb=dbname: (mandatory) 'dbname' is a name of a mongodb to learn from.\n" +
      "target=event: (mandatory) 'event' is the name of a target complex event (either meeting or moving).\n" +
      "dnum=integer: (mandatory). 'integer' is the number of a training set (the code contains 10" +
      " pairs of training/test sets of the CAVIAR dataset -- see  all.core.oled.MeetingTrainingData).\n" +
      "delta=double: (optional, default is 10^-5). 'double' is the δ parameter (significance) for the Hoeffding bound.\n" +
      "prune=double: (optional, default is 0.0). 'double' is a clause quality threshold (clauses with score lower than that will be pruned.)\n" +
      "nmin=integer: (optional, default is 1000). 'integer' is a minimum number of examples on which each clause must be evaluated.\n" +
      "sdepth=integer: (optional, default is 1). 'integer' is the specialization depth.")
    System.exit(-1)
  }


  val args_ = args.toList.map(x => x.split("="))
  val dbname = args_.filter(z => z(0) == "db")
  val target = args_.filter(z => z(0) == "target")
  val dnum = args_.filter(z => z(0) == "dnum")

  val dataSetNumber = if (dnum.isEmpty) 1 else dnum.head(1).toInt

  if (dbname.isEmpty) throw new RuntimeException("Please provide a database to learn from. Use the parameter 'db=yourdb' (no quotes)")
  if (target.isEmpty) throw new RuntimeException("Please indicate a target complex event (meeting or moving). Use the parameter 'target=event' (no quotes)")

  GlobalValues.inputPath =
    if (target.head(1) == "meeting") s"${GlobalValues.cwd}/datasets/Caviar/meeting" else s"${GlobalValues.cwd}/datasets/Caviar/moving"

  val delta_ = args_.filter(z => z(0) == "delta")//0.00001
  val delta = if (delta_.isEmpty) 0.00001 else delta_.head(1).toDouble

  val prune = args_.filter(z => z(0) == "prune")
  val postPruningThreshold = if (prune.isEmpty) 0.4 else prune.head(1).toDouble

  val nmin = args_.filter(z => z(0) == "nmin")
  val minSeenExmpls = if (nmin.isEmpty) 1000 else nmin.head(1).toInt

  val sdepth = args_.filter(z => z(0) == "sdepth")
  val specializationDepth = if (sdepth.isEmpty) 1 else sdepth.head(1).toInt

  val breakTiesThreshold = 0.05
  val repeatFor = 1
  val chunkSize = 10
  val trainingSetSize = 1500
  val withPostPruning = true
  val withInertia = false
  // !!!!!!!!!!!!!!!!!!!!!!!!!!!
  //val HLE = "meeting"
  //val HLE = "moving"
  val HLE = target.head(1)
  // !!!!!!!!!!!!!!!!!!!!!!!!!!!-

  val msg = s"δ=$delta-prune=$postPruningThreshold-minseen=$minSeenExmpls-depth=$specializationDepth"
  //val trainingSets = List(MovingTrainingData.allTrainingSets(dataSetNumber))
  val trainingSets = if (HLE == "meeting") List(MeetingTrainingData.allTrainingSets(dataSetNumber)) else List(MovingTrainingData.allTrainingSets(dataSetNumber))
  //val trainingSets = //List(MeetingTrainingData.wholeCAVIAR1)

  GlobalValues.glvalues("specializationDepth") = specializationDepth.toString
  // !!!!!!!!!!!!!!!!!!!!!!!!!!!
  val HAND_CRAFTED = if(HLE=="meeting"){
    GlobalValues.HAND_CRAFTED_RULES+"/meeting-hand-crafted.lp"
  } else {
    GlobalValues.HAND_CRAFTED_RULES+"/moving-hand-crafted.lp"
  }

  val DB = new Database(Core.fromDB, "examples")

  val x = CaviarUtils.getDataFromIntervals(DB,HLE,this.trainingSets.head.trainingSet,chunkSize)//.toList

  val system = ActorSystem("HoeffdingLearningSystem")
  val latch = new CountDownLatch(1)
  val actor =
    system.actorOf(Props(
      new MasterActor(DB,delta,breakTiesThreshold,postPruningThreshold,minSeenExmpls,
        trainingSetSize,repeatFor,chunkSize,withInertia,withPostPruning,trainingSets,HLE,HAND_CRAFTED, msg)
    ), name = "Master-Actor") !  "start" //"EvaluateHandCrafted"


}

/* Contains utilities for collecting statistics */

object Stats {

  def getExampleStats(exmpls: List[Exmpl]) = {

    def append(x: List[String], y: List[String]) = {
      x ++ y.filter(p => !x.contains(p))
    }

    val (ratios,annotSizes,narSizes,totalSize,wholeAnnotation,wholeNarrative) =
      exmpls.foldLeft(List[Double](),List[Double](),List[Double](),List[Double](),List[String](),List[String]()){ (s,e) =>
        val (rats,annots,nars,total,a,n) = (s._1,s._2,s._3,s._4,s._5,s._6)
        val holdsAtoms = e.exmplWithInertia.annotation
        val narrativeAtoms = e.exmplWithInertia.narrative
        val holdsSize = holdsAtoms.size.toDouble
        val narrativeSize = narrativeAtoms.size.toDouble
        val t = holdsSize+narrativeSize
        val ratio = holdsSize/narrativeSize
        //(rats :+ ratio, annots :+ holdsSize, nars :+ narrativeSize, a ++ holdsAtoms, n ++ narrativeAtoms)
        (rats :+ ratio, annots :+ holdsSize, nars :+ narrativeSize, total :+ t, a, n)
    }
    val meanRatio = Utils.mean(ratios)
    val meanAnnotSize = Utils.mean(annotSizes)
    val meanNarrativeSize = Utils.mean(narSizes)
    val totalAnnotSize = List[String]() // wholeAnnotation.distinct.size
    val totalNarSize = List[String]() //wholeNarrative.distinct.size
    println(s"Mean total example size: ${Utils.mean(totalSize)}" +
      s"\nMean annotation size per example: $meanAnnotSize\nMean narrative size per example: $meanNarrativeSize\n" +
      s"Mean ratio (annot/nar) per example: $meanRatio\nTotal annotation size: $totalAnnotSize\n" +
      s"Total narrative size: $totalNarSize")
    (meanRatio,meanAnnotSize,meanNarrativeSize,totalAnnotSize,totalNarSize)
  }



}



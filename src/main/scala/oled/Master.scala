package oled

import akka.actor.{PoisonPill, ActorSystem, Props, Actor}
import com.typesafe.scalalogging.LazyLogging
import iled.globalValues.GlobalValues

//import iled.core.noisyILED.{ResultsContainer, TrainingSet}
import iled.core.noisyILED.experimentsMLNdata.MLNDataHandler
import iled.utils.{Utils, Database}

import scala.collection.mutable.ListBuffer

/**
  * Created by nkatz on 4/11/16.
  */


/**
  * The path can be either the parent folder of the MLN data (in which case it learns from everything
  * and prints averages), or it can be a single fold path.
  *
  * @param dbName
  * @param path
  * @param chunkSize
  * @param delta
  * @param ties
  * @param pruneThrld
  * @param minSeenExmpls
  * @param repeatFor
  * @param HLE
  * @param specializationDepth
  * @param learningInitWithInertia
  * @param evaluateOrLearn
  * @param experimentType
  * @param withPruning
  */

class Master(dbName: String,path: String, chunkSize: Int, delta: Double, ties: Double, pruneThrld: Double,
             minSeenExmpls: Int, repeatFor: Int, HLE: String, specializationDepth: Int, learningInitWithInertia: Boolean,
             evaluateOrLearn: String, experimentType: String, withPruning: Boolean = true, msg: String = "") extends Actor with LazyLogging {

  val trainingSetSize = 1500 // never used anywhere, old glitch kept here only for things to works, need to clean it up
  val withInertia = false // also needs clean-up (used in some stupid code for getting exmpls with inertia or not?)

  val HAND_CRAFTED = if(HLE=="meeting"){
    GlobalValues.HAND_CRAFTED_RULES+"/meeting-hand-crafted.lp"
  } else {
    GlobalValues.HAND_CRAFTED_RULES+"/moving-hand-crafted.lp"
  }

  var remainingTasks = 0
  val results = new ListBuffer[ResultsContainer]

  def g = (x: List[Double]) => {
    val m = Utils.mean(x)
    val d = Utils.deviation(x, m)
    (m, d)
  }

  def getData(path: String, DB: Database) = {
    // Return the data from a single fold
    def getFoldData(foldPath: String, DB: Database) = {
      if (experimentType == "mln-fragment") {
        // run it on the MLN experiments data
        MLNDataHandler.getData(foldPath, chunkSize)
      } else {
        // add extra negatives from the entire caviar
        // to each MLN experiments fold.
      val mlnDataset = MLNDataHandler.getData(foldPath, chunkSize)
        val index = foldPath.last.toString.toInt
        val dataContainer = new WholeCaviarExperimentsData(DB, HLE)
        dataContainer.getDataForOneFold(mlnDataset,index)
      }
    }
    // If the path is a fold path, then we should get the data and proceed
    // (learn from a single dataset)
    // If the path is the parent folder then we should learn from every dataset
    // and get averages
    if(path.contains("fold")) {
      // we learn from a single fold
      List(getFoldData(path,DB))
    } else {
      // we learn from a single fold add extra negatives
      // (the entire caviar except positive instances for meeting/moving)
      // in the training and testing set of each fold.
      val d = new java.io.File(path)
      val innerFolders = d.listFiles.filter(x => x.getName.contains("fold")).sortBy(_.getName.split("_")(1).toInt).toList
      innerFolders.map(x => getFoldData(x.getAbsolutePath, DB))
    }
  }

  def receive = {

    case job: String =>
      val DB = new Database(dbName, "examples")
      val data = getData(path, DB)
      remainingTasks = data.length
      data foreach { datafold =>
        context.actorOf (
          Props(new OLED(DB, delta, ties, pruneThrld, minSeenExmpls, trainingSetSize, repeatFor,
            chunkSize, withInertia, withPruning, datafold, HLE, HAND_CRAFTED, learningInitWithInertia)), name = "Learner-"+datafold.##
        ) ! evaluateOrLearn
      }

    case x: ResultsContainer =>
      results += x
      remainingTasks -= 1
      println("results size:",results.size)
      println("remaining tasks", remainingTasks)
      sender ! PoisonPill

      if (remainingTasks == 0) {
        val r = results.toList
        // we're doing micro-averages here
        val tps = r.map(x => x.tps).sum
        val fps = r.map(x => x.fps).sum
        val fns = r.map(x => x.fns).sum
        val microPrecision = tps.toDouble/(tps.toDouble+fps.toDouble)
        val microRecall = tps.toDouble/(tps.toDouble+fns.toDouble)
        val microfscore = (2*microPrecision*microRecall)/(microPrecision+microRecall)
        val times = r.map(x => x.time).sum/r.length.toDouble
        val theorySize = r.map(x => x.theorySize).sum/r.length.toDouble

        logger.info(s"\n$msg\ntps: $tps\nfps: $fps\nfns: $fns\nprecision: $microPrecision\nrecall: $microRecall\nfscore: $microfscore\ntheory size: $theorySize\ntime: $times")

        context.system.shutdown()
      }

  }

}

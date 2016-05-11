package oled

import akka.actor.{Props, ActorSystem}
import iled.core.noisyILED.TrainingSet
import iled.core.noisyILED.experimentsMLNdata.MLNDataHandler
import iled.globalValues.GlobalValues
import iled.utils.Database

/**
  * Created by nkatz on 4/11/16.
  */
object OLEDRunner {

  var results = List[ResultsContainer]()

  def main(args: Array[String]) = {
    //runMLNExperiments("/home/nkatz/dev/CAVIAR_MLN/CAVIAR_MLN/meet", chunkSize = 10)
    val path = args(0)
    val delta = args(1).toDouble //0.00000001 // 7 0's
    val ties = args(2).toDouble  //0.05
    val pruneThrld = args(3).toDouble//0.7
    val minSeenExmpls = args(4).toInt//1000 // break ties for a rule only after it has been evaluated on minSeenExmpls
    val repeatFor = args(5).toInt//1     // re-see the data repeatFor
    val trainingSetSize = 1500 // WHOLE_DATA_SET_VALE
    // !!!!!!!!!!!!!!!!!!!!!!!!!!!
    val HLE = args(6)//"meeting"
    // !!!!!!!!!!!!!!!!!!!!!!!!!!!
    val specializationDepth = args(7).toInt//3
    GlobalValues.glvalues("specializationDepth") = specializationDepth.toString
    val chunkSize = args(8).toInt
    val learningInitWithInertia = args(9).toBoolean
    val evalorLearn = args(10) // should be "eval" or "learn"
    val experimentType = args(11) // should be "mln-fragment" or "whole-caviar"
    val withPruning = true
    val withInertia = false

    val HAND_CRAFTED = if(HLE=="meeting"){
      GlobalValues.HAND_CRAFTED_RULES+"/meeting-hand-crafted.lp"
    } else {
      GlobalValues.HAND_CRAFTED_RULES+"/moving-hand-crafted.lp"
    }

    val names = List("path","delta","pruningThreshold","tiesThreshold","minSeenExmpls",
      "repeatFor","HLE","specializationDepth","chunkSize","learningInitWithInertia","evalorLearn","experimentType")
    val z = names zip args.toList
    z foreach (x => println(s"${x._1} = ${x._2}"))

    run(foldPath = path, chunkSize = chunkSize, delta, ties, pruneThrld,
      minSeenExmpls, repeatFor, HLE, specializationDepth, learningInitWithInertia, evalorLearn, experimentType)

    def run(foldPath: String, chunkSize: Int, delta: Double, ties: Double, pruneThrld: Double,
            minSeenExmpls: Int, repeatFor: Int, HLE: String, specializationDepth: Int,
            learningInitWithInertia: Boolean, evaluateOrLearn: String, experimentType: String) = {
      // The DB instance is not used anywhere, its given only for the Dispatcher consrtuctor to compile.
      // I need to fix this, by adding a more generic constructor to Dispatcher that may accept an empty
      // DB object (and also need to implement the logic that will construct and empty DB at the Database class)
      val DB = new Database("CAVIAR_Real_FixedBorders", "examples")
      val data =
        if (experimentType == "mln-fragment") { // We're doing learning here
          MLNDataHandler.getData(foldPath, chunkSize)
        } else { // We're evaluating the hand-crafted knowledge base here
          val mlnDataset = MLNDataHandler.getData(foldPath, chunkSize)
          val index = foldPath.last.toString.toInt
          val dataContainer = new WholeCaviarExperimentsData(DB, HLE)
          dataContainer.getDataForOneFold(mlnDataset,index)
          //WholeCaviarExperiments.getDataForOneFold(mlnDataset,index)
        }
      val system = ActorSystem("HoeffdingLearningSystem")
      val job = if (evalorLearn == "eval") "EvaluateHandCrafted" else "start"
      val actor =
        system.actorOf (
          Props(new OLED(DB, delta, ties, pruneThrld, minSeenExmpls, trainingSetSize, repeatFor,
            chunkSize, withInertia, withPruning, data,HLE,HAND_CRAFTED, learningInitWithInertia)), name = "Learner"
        ) ! job
    }

  }


  /*
  //path = "/home/nkatz/dev/CAVIAR_MLN/CAVIAR_MLN/meet"
  def runMLNExperiments(path: String, chunkSize: Int) = {

    def runFold(foldPath: String, chunkSize: Int) = {
      // The DB instance is not used anywhere, its given only for the Dispatcher consrtuctor to compile.
      // I need to fix this, by adding a more generic constructor to Dispatcher that may accept an empty
      // DB object (and also need to implement the logic that will construct and empty DB at the Database class)
      val DB = new Database("CAVIAR_Real_FixedBorders", "examples")
      val dataset = MLNDataHandler.getData(foldPath, chunkSize)
      val system = ActorSystem("HoeffdingLearningSystem")

      val actor =
        system.actorOf (
          Props(new OLED(DB, delta, ties, pruneThrld, minSeenExmpls, trainingSetSize, repeatFor,
            chunkSize, withInertia, withPruning, dataset.asInstanceOf[TrainingSet],HLE,HAND_CRAFTED)), name = "Learner"
        ) ! "start" // "EvaluateHandCrafted"
    }

    val d = new File(path)
    val innerFolders = d.listFiles.filter(x => x.getName.contains("fold")).sortBy(_.getName.split("_")(1).toInt)
    innerFolders.foreach(x => runFold(x.getAbsolutePath, chunkSize))
  }
*/

  //val d = new File(path).getParent


}
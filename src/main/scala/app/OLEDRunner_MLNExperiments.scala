package app

import akka.actor.{Props, ActorSystem}
import logic.{LogicUtils, Theory, Clause}
import oled.mln_caviar_data.MLNDataHandler
import utils.DataUtils.DataAsExamples
import utils.{Utils, Database}
import jep.Jep
import oled.Dispatcher
import xhail.Xhail

/**
  * Created by nkatz on 9/14/16.
  */

object OLEDRunner_MLNExperiments extends App {

  Globals.glvalues("OLEDdownscoreBySimilarity") = "false" // re-set this if you need to try it
  val learnWholeTheories = false
  val foldPath = "/home/nkatz/dev/CAVIAR_MLN/CAVIAR_MLN/meet/fold_2"
  val entryPath = "/home/nkatz/dev/ILED/datasets/CaviarMLN"
  val fromDB = ""

  val globals = new Globals(entryPath, fromDB)

  val delta = 0.00001
  val ties = 0.05
  val prune = 0.5
  val nmin = 15000 //15000 //15000
  val repeatFor = 2     // re-see the data repeatFor
  val trainSize = 1500 // WHOLE_DATA_SET_VALE
  val onlinePruning = false
  val withPostPruning = true
  val withInertia = false
  // !!!!!!!!!!!!!!!!!!!!!!!!!!!
  val HLE = "meeting"
  // !!!!!!!!!!!!!!!!!!!!!!!!!!!
  val specializationDepth = 2
  val chunkSize = 10
  Globals.glvalues("specializationDepth") = specializationDepth.toString

  val HAND_CRAFTED = if(HLE=="meeting"){
    globals.HAND_CRAFTED_RULES+"/meeting-hand-crafted.lp"
  } else {
    globals.HAND_CRAFTED_RULES+"/moving-hand-crafted.lp"
  }

  //def run(foldPath: String, chunkSize: Int, mode: String = "online", learnWholeTheories: Boolean = false) = {
    // The DB instance is not used anywhere, its given only for the Dispatcher consrtuctor to compile.
    // I need to fix this, by adding a more generic constructor to Dispatcher that may accept an empty
    // DB object (and also need to implement the logic that will construct and empty DB at the Database class)
  val DB = new Database("CAVIAR_Real_FixedBorders", "examples")
  val dataset = MLNDataHandler.getData(foldPath, chunkSize)

  var kernel = Theory()
    /*
    val annot = dataset.testingData.head.annotation.map(x => x+".")
    val nar = dataset.testingData.head.narrative.map(x => x+".")
    iled.utils.Utils.writeLine((annot++nar).mkString("\n"),"/home/nkatz/Desktop/kernel.txt","overwrite")
    */

  val mode = "online"
    ///*
  if (mode == "offline") {
    kernel = collectBottomClauses(dataset, globals)
  }
    //*/

  if (!learnWholeTheories) {
    val system = ActorSystem("HoeffdingLearningSystem")
    val actor =
      system.actorOf(Props(new Dispatcher(DB,delta,ties,prune,nmin,trainSize,repeatFor,chunkSize,withInertia,withPostPruning,onlinePruning,
        dataset, HLE, HAND_CRAFTED, kernel, globals) ), name = "Learner") ! "start"
    //dataset.asInstanceOf[TrainingSet],HLE,HAND_CRAFTED) ), name = "Learner") ! "EvaluateHandCrafted"
  } else {

      /* This doesn't work */

      /*
      Globals.LEARNING_WHOLE_THEORIES = true
      val system = ActorSystem("HoeffdingLearningSystem")
      system.actorOf(Props(new WholeTheoryLearner(DB,delta,ties,prune,nmin,trainSize,repeatFor,chunkSize,"",withInertia,withPostPruning,onlinePruning,
        dataset, HLE, true, kernel)), name = "Learner") ! "go"
      */
  }


  //}

  /* This is used in "offline" mode (EXPERIMENTAL): the training set is passed once
   * to collect the bottom clauses and these bottom clauses are generalized
   * with a second pass over the data. The advantage of this approach is that
   * a bottom theory is available from the start and hopefully there is enough
   * time to generalize it to a good hypothesis (better than one constructed in
   * online, where new bottom clauses are constructed "on demand" by the theory
   * expansion routine.)*/
  def collectBottomClauses(dataset: DataAsExamples, globals: Globals): Theory = {
    val infile = Utils.getTempFile("example", ".lp", deleteOnExit = true)
    val jep = new Jep()
    val bk = globals.BK_WHOLE_EC
    Globals.glvalues("perfect-fit") = "false"
    var time = 0
    val (accumKernel, accumAnnotation, accumNarrative) =
      dataset.testingSet.foldLeft( List[Clause](), List[String](), List[String]() ) { (x,y) =>
        val ker = x._1
        val annotAccum = x._2
        val narrativeAccum = x._3
        println(y.time.toInt)
        if (y.time.toInt <= time) time = y.time.toInt
        if (y.time == "6717") {
          val stop = "stop"
        }
        // generate a kernel set from the current example
        val interpretation = y.annotationASP ++ y.narrativeASP
        Utils.writeToFile(infile, "overwrite") { p => interpretation.foreach(p.println) }
        val (_, varKernel) =
          Xhail.runXhail(fromFile=infile.getAbsolutePath, kernelSetOnly=true,
            fromWeakExmpl=false, jep=jep, bkFile=bk, learningTerminatedAtOnly=false, globals=globals)
        (ker ++ varKernel, annotAccum ++ y.annotation, narrativeAccum ++ y.narrative)
      }
    val compressedKernel = LogicUtils.compressTheory(accumKernel.toList)
    Theory(compressedKernel)
  }

}

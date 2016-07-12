package all.core.oled.experimentsMLNdata

import java.io.File

import akka.actor.{Props, ActorSystem}
import all.core.oled.Dispatcher
import all.globalValues.GlobalValues
import all.structures.Examples.Example
import all.utils.Database
import scala.io.Source
import all.core.oled.TrainingSet

import scala.util.Random

/**
  * Created by nkatz on 3/4/16.
  */

object Runner {
  def main(args: Array[String]) = {
    //val trainingSet = MLNDataHandler.getData("/home/nkatz/dev/CAVIAR_MLN/CAVIAR_MLN/meet/fold_0",10)
    //run(foldPath = "/home/nkatz/dev/CAVIAR_MLN/CAVIAR_MLN/meet/fold_0", chunkSize = 10)

    //val trainingSet = MLNDataHandler.getData(foldPath = "/home/nkatz/dev/CAVIAR_MLN/CAVIAR_MLN/move/fold_0", chunkSize = 10)


    /*
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
    */

    val args_ = args.toList.map(x => x.split("="))
    val path = args_.filter(z => z(0) == "path")
    val target = args_.filter(z => z(0) == "target")

    if (path.isEmpty) throw new RuntimeException("Please indicate a datapath to learn from. Use the parameter 'db=yourdb' (no quotes)")
    if (target.isEmpty) throw new RuntimeException("Please indicate a target complex event (meeting or moving). Use the parameter 'target=event' (no quotes)")

    GlobalValues.inputPath =
      if (target.head(1) == "meeting") s"${GlobalValues.cwd}/datasets/CaviarMLN/meeting" else s"${GlobalValues.cwd}/datasets/CaviarMLN/moving"

    val delta_ = args_.filter(z => z(0) == "delta")//0.00001
    val delta = if (delta_.isEmpty) 0.00001 else delta_.head(1).toDouble

    val prune = args_.filter(z => z(0) == "prune")
    val pruningThreshold = if (prune.isEmpty) 0.4 else prune.head(1).toDouble

    val nmin = args_.filter(z => z(0) == "nmin")
    val minSeen = if (nmin.isEmpty) 1000 else nmin.head(1).toInt

    val sdepth = args_.filter(z => z(0) == "sdepth")
    val specializationDepth = if (sdepth.isEmpty) 1 else sdepth.head(1).toInt

    // example:
    // path=/home/nkatz/dev/CAVIAR_MLN/CAVIAR_MLN/meet/fold_2 target=meeting delta=0.00001 prune=0.7 nmin=1000 sdepth=2

    run(foldPath=path.head(1), hle=target.head(1), chunkSize=10, delta=delta, prune=pruningThreshold, nmin=minSeen, sdepth=specializationDepth)
    //"/home/nkatz/dev/CAVIAR_MLN/CAVIAR_MLN/meet/fold_2"
  }


  //val delta = 0.00001 // 7 0's
  val breakTiesThreshold = 0.05
  //val postPruningThreshold = 0.7
  //val minSeenExmpls = 1000
  val repeatFor = 1
  val trainingSetSize = 1500
  val withPostPruning = true
  val withInertia = false
  // !!!!!!!!!!!!!!!!!!!!!!!!!!!
  //val HLE = "meeting"
  // !!!!!!!!!!!!!!!!!!!!!!!!!!!
  //val specializationDepth = 2
  //GlobalValues.glvalues("specializationDepth") = specializationDepth.toString


/*
  val HAND_CRAFTED = if(HLE=="meeting"){
    GlobalValues.HAND_CRAFTED_RULES+"/meeting-hand-crafted.lp"
  } else {
    GlobalValues.HAND_CRAFTED_RULES+"/moving-hand-crafted.lp"
  }
*/
  def run(foldPath: String, hle: String, chunkSize: Int, delta: Double, prune: Double, nmin: Int, sdepth: Int) = {

    GlobalValues.glvalues("specializationDepth") = sdepth.toString

    /*
    val HAND_CRAFTED = if(hle=="meeting"){
      GlobalValues.HAND_CRAFTED_RULES+"/meeting-hand-crafted.lp"
    } else {
      GlobalValues.HAND_CRAFTED_RULES+"/moving-hand-crafted.lp"
    }
    */
    val HAND_CRAFTED = ""

    // The DB instance is not used anywhere, its given only for the Dispatcher constructor to compile.
    // I need to fix this, by adding a more generic constructor to Dispatcher that may accept an empty
    // DB object (and also need to implement the logic that will construct and empty DB at the Database class)
    val DB = new Database("CAVIAR_Real_FixedBorders", "examples")
    val dataset = MLNDataHandler.getData(foldPath, chunkSize)


    val annot = dataset.testingData.head.annotation.map(x => x+".")
    val nar = dataset.testingData.head.narrative.map(x => x+".")
    all.utils.Utils.writeLine((annot++nar).mkString("\n"),"/home/nkatz/Desktop/kernel.txt","overwrite")


    val system = ActorSystem("HoeffdingLearningSystem")

    val actor =
      system.actorOf(Props(new Dispatcher(DB,delta,breakTiesThreshold,
        prune,nmin,trainingSetSize,repeatFor,
        chunkSize,withInertia,withPostPruning,
        dataset.asInstanceOf[TrainingSet],hle,HAND_CRAFTED) ), name = "Learner") ! "start" // "EvaluateHandCrafted"


  }


}

object MLNDataHandler {

  //"/home/nkatz/dev/CAVIAR_MLN/CAVIAR_MLN/meet/fold_0/training/batch/training.fold_0.db"
  ///home/nkatz/dev/CAVIAR_MLN/CAVIAR_MLN/meet/fold_0

  class TrainingSet(val trainingData: List[Example], val testingData: List[Example]) extends all.core.oled.TrainingSet(Nil, Nil)



  def getData(foldPath: String, chunkSize: Int) = {

    val word = "[\\w]+".r
    val map = Map("HoldsAt" -> "holdsAt", "Happens" -> "happensAt", "Close" -> "close", "OrientationMove" -> "orientationMove",
      "Active" -> "active", "Inactive" -> "inactive", "Walking" -> "walking", "Abrupt" -> "abrupt" -> "Running" -> "running",
      "Enter" -> "appear", "Exit" -> "disappear", "Meet" -> "meeting", "Move" -> "moving")

    def process(s: String) = {
      def replAll(s: String, chunks: List[(String,String)]) = chunks.foldLeft(s)( (x,y) => x.replaceAll(y._1,y._2) )
      val wordChunks = word findAllIn s toList
      val time = wordChunks.reverse.head
      val chunks = ((word findAllIn s).map(x => (x,map.getOrElse(x, handleInnerTerms(x)))).toList,time.toInt)
      val newS = (replAll(s, chunks._1).replaceAll("\\s",""),time)
      newS
    }

    def isPred(s: String) = {
      s.startsWith("HoldsAt") || s.startsWith("Happens") || s.startsWith("Close") || s.startsWith("OrientationMove")
    }

    def handleInnerTerms(s: String) = {
      def lowerFirst(s: String) = s.replace(s(0),s(0).toLower)
      //val p = "[A-Z][\\w]+[_][\\w]" // matches stuff like Walking_A and Meet_B_A
      val split = s.split("_")
      split.length match {
        case 1 => lowerFirst(s) // no underscores, simply to lower-case
        case 2 => s"${map(split(0))}(${lowerFirst(split(1))})"
        case 3 => s"${map(split(0))}(${lowerFirst(split(1))},${lowerFirst(split(2))})"
      }
    }

    def formatAndSplitData(data: List[String], split: Boolean) = {
      //val slideStep = if (split) chunkSize - 1 else data.length-1
      val sorted = (data map process _ groupBy(_._2) map {case (k,v) => (k,v.map(_._1))}).toList.sortBy(_._1.toInt) map (_._2)
      val iter = if (split) sorted.sliding(chunkSize, chunkSize-1) map (_.flatten) toList else sorted
      val d = iter map { p =>
        val (annotation,narrative) = p.foldLeft(List[String](),List[String]()) { (x,y) =>
          if (y.startsWith("holdsAt")) (x._1 :+ y, x._2) else (x._1, x._2 :+ y)
        }
        val time = (word findAllIn p.head toList).reverse.head
        new Example(annot = annotation, nar = narrative, _time = time)
      }
      d.toList
    }
    /*
    def formatNoSplit(data: List[String]) = {
      val sorted = (data map process _ groupBy(_._2) map {case (k,v) => (k,v.map(_._1))}).toList.sortBy(_._1.toInt) map (_._2)
      val d = sorted map { p =>
        val (annotation,narrative) = p.foldLeft(List[String](),List[String]()) { (x,y) =>
          if (y.startsWith("holdsAt")) (x._1 :+ y, x._2) else (x._1, x._2 :+ y)
        }
        val time = (word findAllIn p.head toList).reverse.head
        new Example(annot = annotation, nar = narrative, _time = time)
      }
      d.toList
    }
*/
    val training = {
      val trainingSetPath = s"$foldPath/training/batch"
      val innerFiles = new File(trainingSetPath).listFiles
      innerFiles flatMap (f => Source.fromFile(f).getLines.toList.filter( isPred _ )) toList
    }

    val testingData = {
      val testingSetPath = s"$foldPath/testing"
      val annotationPath = s"${new File(new File(testingSetPath).getParent).getParent}/annotation"
      val innerFiles = new File(testingSetPath).listFiles
      val testingFilesNames = innerFiles.map(f => f.getName.substring(0,f.getName.lastIndexOf("."))).toList

      val exmpls = testingFilesNames.foldLeft(List[Example]()) { (p,testingFile) =>
        val narrative = Source.fromFile(testingSetPath+"/"+testingFile+".db").getLines.toList.filter( isPred _ )
        // Find the proper annotation file
        val annotFile = new File(annotationPath).listFiles.toList.filter(f => f.getName.contains(testingFile)).head
        val annotation = Source.fromFile(annotFile).getLines.toList.filter(p => isPred(p) && p.split(" ")(1) == "1" ).map(_.split(" ")(0))
        val e = formatAndSplitData(narrative++annotation, split=false)
        p :+ Example.merge(e)
      }
      exmpls
    }
    val trainingData = formatAndSplitData(training, split=true)
    // make sure that we start with positive examples (don't waste negatives from which nothing is learnt)
    val pos = trainingData.filter(x => x.annotation.nonEmpty)
    val _trainingData = List(pos.head) ++ trainingData //Random.shuffle(trainingData)
    new TrainingSet(_trainingData,testingData)
  }



}

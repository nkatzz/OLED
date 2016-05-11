package iled.core.noisyILED.experimentsMLNdata

import java.io.File

import akka.actor.{Props, ActorSystem}
import iled.core.noisyILED.Dispatcher
import iled.globalValues.GlobalValues
import iled.structures.Examples.Example
import iled.utils.Database
import scala.io.Source
import iled.core.noisyILED.TrainingSet

import scala.util.Random

/**
  * Created by nkatz on 3/4/16.
  */

object Runner {
  def main(args: Array[String]) = {
    //val trainingSet = MLNDataHandler.getData("/home/nkatz/dev/CAVIAR_MLN/CAVIAR_MLN/meet/fold_0",10)
    //run(foldPath = "/home/nkatz/dev/CAVIAR_MLN/CAVIAR_MLN/meet/fold_0", chunkSize = 10)

    //val trainingSet = MLNDataHandler.getData(foldPath = "/home/nkatz/dev/CAVIAR_MLN/CAVIAR_MLN/move/fold_0", chunkSize = 10)

    run(foldPath = "/home/nkatz/dev/CAVIAR_MLN/CAVIAR_MLN/meet/fold_2", chunkSize = 10)
  }


  val delta = 0.0001 // 7 0's
  val breakTiesThreshold = 0.05
  val postPruningThreshold = 0.7
  val minSeenExmpls = 1000 // break ties for a rule only after it has been evaluated on minSeenExmpls
  val repeatFor = 1     // re-see the data repeatFor
  val trainingSetSize = 1500 // WHOLE_DATA_SET_VALE
  val withPostPruning = true
  val withInertia = false
  // !!!!!!!!!!!!!!!!!!!!!!!!!!!
  val HLE = "meeting"
  // !!!!!!!!!!!!!!!!!!!!!!!!!!!
  val specializationDepth = 3
  GlobalValues.glvalues("specializationDepth") = specializationDepth.toString

  val HAND_CRAFTED = if(HLE=="meeting"){
    GlobalValues.HAND_CRAFTED_RULES+"/meeting-hand-crafted.lp"
  } else {
    GlobalValues.HAND_CRAFTED_RULES+"/moving-hand-crafted.lp"
  }

  def run(foldPath: String, chunkSize: Int) = {
    // The DB instance is not used anywhere, its given only for the Dispatcher consrtuctor to compile.
    // I need to fix this, by adding a more generic constructor to Dispatcher that may accept an empty
    // DB object (and also need to implement the logic that will construct and empty DB at the Database class)
    val DB = new Database("CAVIAR_Real_FixedBorders", "examples")
    val dataset = MLNDataHandler.getData(foldPath, chunkSize)
    val system = ActorSystem("HoeffdingLearningSystem")

    val actor =
      system.actorOf(Props(new Dispatcher(DB,delta,breakTiesThreshold,
        postPruningThreshold,minSeenExmpls,trainingSetSize,repeatFor,
        chunkSize,withInertia,withPostPruning,
        dataset.asInstanceOf[TrainingSet],HLE,HAND_CRAFTED) ), name = "Learner") ! "start" // "EvaluateHandCrafted"


  }


}

object MLNDataHandler {

  //"/home/nkatz/dev/CAVIAR_MLN/CAVIAR_MLN/meet/fold_0/training/batch/training.fold_0.db"
  ///home/nkatz/dev/CAVIAR_MLN/CAVIAR_MLN/meet/fold_0

  class TrainingSet(val trainingData: List[Example], val testingData: List[Example]) extends iled.core.noisyILED.TrainingSet(Nil, Nil)



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

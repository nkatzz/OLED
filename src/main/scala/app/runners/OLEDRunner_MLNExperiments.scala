package app.runners

import java.io.File

import akka.actor.{ActorSystem, Props}
import app.runners.MLNDataHandler.MLNDataOptions
import app.runutils.{CMDArgs, Globals}
import jep.Jep
import logic.Examples.Example
import logic.{Clause, LogicUtils, Theory}
import oled.single_core.Dispatcher
import utils.DataUtils.DataAsExamples
import utils.Utils
import xhail.Xhail

import scala.io.Source
import scala.util.Random

/**
  * Created by nkatz on 9/14/16.
  */

object OLEDRunner_MLNExperiments {

  def main(args: Array[String]) = {

    val argsok = CMDArgs.argsOk(args)

    if (!argsok._1) {
      println(argsok._2)
      System.exit(-1)
    } else {
      Globals.glvalues("OLEDdownscoreBySimilarity") = "false" // re-set this if you need to try it
      val learnWholeTheories = false

      //val foldPath = "/home/nkatz/dev/CAVIAR_MLN/CAVIAR_MLN/move/fold_9"

      val foldPath = args.map(x => x.split("=")).find(x => x(0) == "--foldpath").getOrElse(throw new RuntimeException("--foldpath missing."))(1)

      val inps = CMDArgs.getOLEDInputArgs(args)

      val trainingDataOptions = new MLNDataOptions(foldPath, inps.chunkSize)
      val testingDataOptions = trainingDataOptions
      val trainingDataFunction: MLNDataOptions => Iterator[Example] = MLNDataHandler.getTrainingData
      val testingDataFunction: MLNDataOptions => Iterator[Example] = MLNDataHandler.getTestingData

      if (!learnWholeTheories) {
        val system = ActorSystem("HoeffdingLearningSystem")
        system.actorOf(Props(new Dispatcher(inps, trainingDataOptions, testingDataOptions, trainingDataFunction, testingDataFunction) ), name = "Learner") ! "start"
      } else {
        /* This doesn't work */
        /*
        Globals.LEARNING_WHOLE_THEORIES = true
        val system = ActorSystem("HoeffdingLearningSystem")
        system.actorOf(Props(new WholeTheoryLearner(DB,delta,ties,prune,nmin,trainSize,repeatFor,chunkSize,"",withInertia,withPostPruning,onlinePruning,
          dataset, HLE, true, kernel)), name = "Learner") ! "go"
        */
      }
    }


  }


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


object MLNDataHandler {

  //"/home/nkatz/dev/CAVIAR_MLN/CAVIAR_MLN/meet/fold_0/training/batch/training.fold_0.db"
  ///home/nkatz/dev/CAVIAR_MLN/CAVIAR_MLN/meet/fold_0


  class MLNDataOptions(val foldPath: String, val chunkSize: Int) extends app.runutils.IOHandling.MongoSource

  def getTrainingData(opts: MLNDataOptions): Iterator[Example] = {
    val d = getData(opts)
    d._1.toIterator
  }

  def getTestingData(opts: MLNDataOptions): Iterator[Example] = {
    val d = getData(opts)
    d._2.toIterator
  }

  def getData(opts: MLNDataOptions) = {

    val word = "[\\w]+".r
    val map = Map("HoldsAt" -> "holdsAt", "Happens" -> "happensAt", "Close" -> "close", "OrientationMove" -> "orientationMove",
      "Active" -> "active", "Inactive" -> "inactive", "Walking" -> "walking", "Abrupt" -> "abrupt" -> "Running" -> "running",
      "Enter" -> "appear", "Exit" -> "disappear", "Meet" -> "meeting", "Move" -> "moving")

    def process(s: String) = {
      def replAll(s: String, chunks: List[(String, String)]) = chunks.foldLeft(s)((x, y) => x.replaceAll(y._1, y._2))
      val wordChunks = word findAllIn s toList
      val time = wordChunks.reverse.head
      val chunks = ((word findAllIn s).map(x => (x, map.getOrElse(x, handleInnerTerms(x)))).toList, time.toInt)
      val newS = (replAll(s, chunks._1).replaceAll("\\s", ""), time)
      newS
    }

    def isPred(s: String) = {
      s.startsWith("HoldsAt") || s.startsWith("Happens") || s.startsWith("Close") || s.startsWith("OrientationMove")
    }

    def handleInnerTerms(s: String) = {
      def lowerFirst(s: String) = s.replace(s(0), s(0).toLower)
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
      val sorted = (data map process _ groupBy (_._2) map { case (k, v) => (k, v.map(_._1)) }).toList.sortBy(_._1.toInt) map (_._2)
      val iter = if (split) sorted.sliding(opts.chunkSize, opts.chunkSize - 1) map (_.flatten) toList else sorted
      val d = iter map { p =>
        val (annotation, narrative) = p.foldLeft(List[String](), List[String]()) { (x, y) =>
          if (y.startsWith("holdsAt")) (x._1 :+ y, x._2) else (x._1, x._2 :+ y)
        }
        val time = (word findAllIn p.head toList).reverse.head
        new Example(annot = annotation, nar = narrative, _time = time)
      }
      d.toList
    }

    val training = {
      val trainingSetPath = s"${opts.foldPath}/training/batch"
      val innerFiles = new File(trainingSetPath).listFiles
      innerFiles flatMap (f => Source.fromFile(f).getLines.toList.filter(isPred _)) toList
    }

    val testingData = {
      val testingSetPath = s"${opts.foldPath}/testing"
      val annotationPath = s"${new File(new File(testingSetPath).getParent).getParent}/annotation"
      val innerFiles = new File(testingSetPath).listFiles
      val testingFilesNames = innerFiles.map(f => f.getName.substring(0, f.getName.lastIndexOf("."))).toList

      val exmpls = testingFilesNames.foldLeft(List[Example]()) { (p, testingFile) =>
        val narrative = Source.fromFile(testingSetPath + "/" + testingFile + ".db").getLines.toList.filter(isPred _)
        // Find the proper annotation file
        val annotFile = new File(annotationPath).listFiles.toList.filter(f => f.getName.contains(testingFile)).head
        val annotation = Source.fromFile(annotFile).getLines.toList.filter(p => isPred(p) && p.split(" ")(1) == "1").map(_.split(" ")(0))
        val e = formatAndSplitData(narrative ++ annotation, split = false)
        p :+ Example.merge(e)
      }
      exmpls
    }
    val trainingData = formatAndSplitData(training, split = true)
    // make sure that we start with positive examples (don't waste negatives from which nothing is learnt)
    val pos = trainingData.filter(x => x.annotation.nonEmpty)
    val _trainingData = trainingData //List(pos.head) ++ Random.shuffle(trainingData) // trainingData //
    (_trainingData, testingData)
  }

}


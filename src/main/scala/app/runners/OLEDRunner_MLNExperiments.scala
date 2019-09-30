/*
 * Copyright (C) 2016  Nikos Katzouris
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

package app.runners

import java.io.File

import akka.actor.{ActorSystem, Props}
import app.runners.MLNDataHandler.MLNDataOptions
import app.runutils.{CMDArgs, Globals}
import logic.Examples.Example
import logic.{Clause, Literal, LogicUtils, Theory}
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

      val trainingDataOptions = new MLNDataOptions(foldPath, inps.chunkSize, take = 10000)
      //val trainingDataOptions = new MLNDataOptions(foldPath, inps.chunkSize)
      val testingDataOptions = new MLNDataOptions(foldPath, inps.chunkSize)
      val trainingDataFunction: MLNDataOptions => Iterator[Example] = MLNDataHandler.getTrainingData
      val testingDataFunction: MLNDataOptions => Iterator[Example] = MLNDataHandler.getTestingData

      //val msg = "eval"
      val msg = "start"

      if (msg == "eval" && inps.evalth == "None") {
        throw new RuntimeException("No theory file provided (start msg = eval)")
      }

      if (!learnWholeTheories) {
        val system = ActorSystem("HoeffdingLearningSystem")
        system.actorOf(Props(new Dispatcher(inps, trainingDataOptions, testingDataOptions, trainingDataFunction, testingDataFunction) ), name = "Learner") ! msg
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
    val infile = Utils.getTempFile("example", ".lp")
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
          Xhail.runXhail(fromFile=infile.getAbsolutePath, kernelSetOnly=true, bkFile=bk, globals=globals)
        (ker ++ varKernel, annotAccum ++ y.annotation, narrativeAccum ++ y.narrative)
      }
    val compressedKernel = LogicUtils.compressTheory(accumKernel.toList)
    Theory(compressedKernel)
  }

}


object MLNDataHandler {

  //"/home/nkatz/dev/CAVIAR_MLN/CAVIAR_MLN/meet/fold_0/training/batch/training.fold_0.db"
  ///home/nkatz/dev/CAVIAR_MLN/CAVIAR_MLN/meet/fold_0


  class MLNDataOptions(val foldPath: String, val chunkSize: Int, val take: Int = 0) extends app.runutils.IOHandling.MongoSource

  def getTrainingData(opts: MLNDataOptions): Iterator[Example] = {
    val d = getData(opts)
    val training = if(opts.take == 0) d._1 else d._1.take(opts.take/opts.chunkSize)
    training.toIterator
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

    ///*
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
    //*/

    /*
    def formatAndSplitData(data: List[String], split: Boolean): List[Example] = {
      //val slideStep = if (split) chunkSize - 1 else data.length-1
      val sorted = (data map process _ groupBy (_._2) map { case (k, v) => (k, v.map(_._1)) }).toList.sortBy(_._1.toInt) map (_._2)
      val iter = if (split) sorted.sliding(opts.chunkSize, opts.chunkSize - 1) map (_.flatten) toList else sorted
      //val iter = if (split) sorted.sliding(opts.chunkSize, opts.chunkSize) map (_.flatten) toList else sorted
      val d = iter map { p =>
        val (annotation, narrative) = p.foldLeft(List[String](), List[String]()) { (x, y) =>
          if (y.startsWith("holdsAt")) (x._1 :+ y, x._2) else (x._1, x._2 :+ y)
        }
        val time = (word findAllIn p.head toList).reverse.head

        // Remove all narrative atoms with a lastTime time stamp. This is to
        // avoid situations where e.g. an initiation rule correctly fires at
        // the last time point of a mini-batch, but there is no corresponding
        // annotation atom in the mini-batch (it's been carried over to the next batch)
        // therefore we erroneously count FPs. The "edge" narrative atoms are carried over
        // to the next batch (notice the "sliding(chunkSize, chunkSize - 1)" above).
        val lastTime = (word findAllIn p.last toList).reverse.head
        val narrative_ = narrative.filter{ x =>
          val lit = Literal.parseWPB2(x)
          val litTime = lit.terms.last.name
          litTime != lastTime
        }
        //new Example(annot = annotation, nar = narrative, _time = time)
        new Example(annot = annotation, nar = narrative_, _time = time)
      }
      d.toList
    }
    */

    val training = {
      val trainingSetPath = s"${opts.foldPath}/training/batch"
      val innerFiles = new File(trainingSetPath).listFiles
      innerFiles flatMap (f => Source.fromFile(f).getLines.toList.filter(isPred _)) toList
    }

    // This is for getting one video per mini-batch in the training set.
    // It's how the data were partitioned for the OSLa experiments and that's how we'll compare.
    val t = {
      val trainingSetPath = s"${opts.foldPath}/training"
      val innerFiles = new File(trainingSetPath).listFiles.filter(p => p.getName.contains("training")).sortBy{ file =>
        file.getName.split("\\.")(1).split("_")(2).toInt
      }
      innerFiles.map(f => Source.fromFile(f).getLines.toList.filter(isPred _)).
        map(singleVideo => formatAndSplitData(singleVideo, split = false)).map(x => Example.merge(x))
    }.toList

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

    // use this to split training videos to mini-batches
    val trainingData = formatAndSplitData(training, split = true)
    // use this to use a whole video per mini-batch (as in the OSLa experiments)
    //val trainingData = t


    // make sure that we start with positive examples (don't waste negatives from which nothing is learnt)
    val pos = trainingData.filter(x => x.annotation.nonEmpty)

    //val _trainingData = trainingData//List(pos.head) ++ Random.shuffle(trainingData) // trainingData //
    val _trainingData = trainingData //List(pos.head) ++ trainingData // trainingData //

    (_trainingData, testingData)
  }

}
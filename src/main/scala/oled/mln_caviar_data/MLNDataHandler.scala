package oled.mln_caviar_data

import logic.Examples.Example
import scala.io.Source
import java.io.File
import utils.DataUtils.DataAsExamples

/**
  * Created by nkatz on 9/14/16.
  */


object MLNDataHandler {

  //"/home/nkatz/dev/CAVIAR_MLN/CAVIAR_MLN/meet/fold_0/training/batch/training.fold_0.db"
  ///home/nkatz/dev/CAVIAR_MLN/CAVIAR_MLN/meet/fold_0

  def getData(foldPath: String, chunkSize: Int) = {

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
      val iter = if (split) sorted.sliding(chunkSize, chunkSize - 1) map (_.flatten) toList else sorted
      val d = iter map { p =>
        val (annotation, narrative) = p.foldLeft(List[String](), List[String]()) { (x, y) =>
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
      innerFiles flatMap (f => Source.fromFile(f).getLines.toList.filter(isPred _)) toList
    }

    val testingData = {
      val testingSetPath = s"$foldPath/testing"
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
    val _trainingData = List(pos.head) ++ trainingData //Random.shuffle(trainingData)
    new DataAsExamples(_trainingData, testingData)
  }

}

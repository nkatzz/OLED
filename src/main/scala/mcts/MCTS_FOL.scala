package mcts

import app.runners.{MLNDataHandler, OLEDRunner_MLNExperiments}
import app.runners.MLNDataHandler.MLNDataOptions
import app.runutils.Globals
import com.typesafe.scalalogging.LazyLogging
import jep.Jep
import logic.Examples.Example
import logic.{Clause, LogicUtils, Theory}
import utils.DataUtils.DataAsExamples
import utils.Utils
import xhail.Xhail

/**
  * Created by nkatz on 9/14/17.
  */

object MCTS_FOL extends LazyLogging {

  def main(args: Array[String]) = {
    val foldPath = "/home/nkatz/dev/CAVIAR_MLN/CAVIAR_MLN/move/fold_2"
    val chunkSize = 50

    val opts = new MLNDataOptions(foldPath, chunkSize)

    val dataset = getData(opts)

    val globals = new Globals("/home/nkatz/dev/OLED/iled/datasets/CaviarMLN", "")

    val bottomTheory = constructBottomTheory(dataset, globals)

    println(bottomTheory.tostring)

    /*
    val exmpls = getData(new MLNDataOptions(foldPath, 10000)).next()
    val theory = Xhail.findHypothesis(bottomTheory.clauses,
                                      examples = Map("annotation" -> exmpls.annotationASP, "narrative" -> exmpls.narrativeASP),
                                      jep= new Jep(), globals = globals)

    logger.info(theory.tostring)
    */

  }

  def getData(opts : MLNDataOptions) = MLNDataHandler.getTrainingData(opts)

  def constructBottomTheory(trainingSet: Iterator[Example], globals: Globals): Theory = {
    val infile = Utils.getTempFile("example", ".lp")
    val jep = new Jep()
    val bk = globals.BK_WHOLE_EC
    Globals.glvalues("perfect-fit") = "false"
    var time = 0
    val (accumKernel, accumAnnotation, accumNarrative) =
      trainingSet.foldLeft( List[Clause](), List[String](), List[String]() ) { (x,y) =>
        val ker = x._1
        val annotAccum = x._2
        val narrativeAccum = x._3
        println(y.time.toInt)
        if (y.time.toInt <= time) time = y.time.toInt
        // generate a kernel set from the current example
        val interpretation = y.annotationASP ++ y.narrativeASP
        Utils.writeToFile(infile, "overwrite") { p => interpretation.foreach(p.println) }
        val (_, varKernel) =
          Xhail.runXhail(fromFile=infile.getAbsolutePath, kernelSetOnly=true, jep=jep, bkFile=bk, globals=globals)
        (ker ++ varKernel, annotAccum ++ y.annotation, narrativeAccum ++ y.narrative)
      }
    val compressedKernel = LogicUtils.compressTheory(accumKernel)
    jep.close()
    Theory(compressedKernel)
  }



}

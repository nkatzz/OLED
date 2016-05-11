package iled.core.noisyILED.experimentsMLNdata

import com.typesafe.scalalogging.LazyLogging
import iled.core.Core
import iled.core.Xhail._
import iled.globalValues.GlobalValues
import iled.structures.{Literal, Theory, Clause}
import iled.structures.Examples.Example
import iled.utils._
import jep.Jep
import iled.core.Core.glvalues
import iled.core.Implicits._
/**
  * Created by nkatz on 3/9/16.
  */
object XHAILExperiments extends LazyLogging {

  def evaluateTheory(theory: Theory, e: Exmpl, withInertia: Boolean = true, jep: Jep, globals: GlobalValues, handCraftedTheoryFile: String = ""): Unit = {
    val varbedExmplPatterns = for (x <- Core.examplePatterns) yield x.varbed.tostring
    val coverageConstr = s"${globals.TPS_RULES}\n${globals.FPS_RULES}\n${globals.FNS_RULES}"
    val t =
      if(theory != Theory()) {
        theory.clauses.map(x => x.withTypePreds().tostring).mkString("\n")
      } else {
        globals.INCLUDE_BK(handCraftedTheoryFile)
      }

    val show = globals.SHOW_TPS_ARITY_1 + globals.SHOW_FPS_ARITY_1 + globals.SHOW_FNS_ARITY_1
    val ex = if(withInertia) e.exmplWithInertia.tostring else e.exmplNoInertia.tostring
    val program = ex + globals.INCLUDE_BK(GlobalValues.BK_CROSSVAL) + t + coverageConstr + show
    val f = Utils.getTempFile("isConsistent",".lp",deleteOnExit = true)
    Utils.writeLine(program, f, "overwrite")
    val answerSet = ASP.solve(task = Core.INFERENCE, aspInputFile = f, jep=jep)
    if (answerSet.nonEmpty) {
      val atoms = answerSet.head.atoms
      atoms.foreach { a=>
        val lit = Literal.toLiteral(a)
        val inner = lit.terms.head
        lit.functor match {
          case "tps" => theory.tps += inner.tostring
          case "fps" => theory.fps += inner.tostring
          case "fns" => theory.fns += inner.tostring
        }
      }
    }
  }



  def main(args: Array[String])= {
    //val foldPath = "/home/nkatz/dev/CAVIAR_MLN/CAVIAR_MLN/move/fold_1"
    val foldPath = args(0)
    val chunkSize = 10 //500000000 // we need to get the whole dataset in one batch so give a very large chunk size
    val dataset = MLNDataHandler.getData(foldPath, chunkSize)
    //Utils.writeLine(dataset.trainingData.head.
    //  annotationASP.mkString("\n")+dataset.trainingData.head.narrativeASP.mkString("\n"),"/home/nkatz/Desktop/kernel.txt","overwrite")
    // The dataset should be passed as a single example (we're doing batch leanring with XHAIL)

    //-------------------------------
    //val e = dataset.trainingData.head
    //-------------------------------
      //if (dataset.trainingData.length > 1) throw new RuntimeException("The dataset should be passed as a single example") else  dataset.trainingData.head

    val infile = Utils.getTempFile("example", ".lp", deleteOnExit = true)
    val jep = new Jep()
    val bk = GlobalValues.BK_WHOLE_EC
    glvalues("perfect-fit") = "false"
    var time = 0
    val (accumKernel, accumAnnotation, accumNarrative) =
      //dataset.trainingData.take(200).foldLeft( List[Clause](), List[String](), List[String]() ) { (x,y) =>
      dataset.trainingData.foldLeft( List[Clause](), List[String](), List[String]() ) { (x,y) =>
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
          runXhail(fromFile=infile.getAbsolutePath, kernelSetOnly=true,
            fromWeakExmpl=false, jep=jep, bkFile=bk, learningTerminatedAtOnly=false)
        (ker ++ varKernel, annotAccum ++ y.annotation, narrativeAccum ++ y.narrative)
    }
    val compressedKernel = compressTheory(accumKernel.toList)
    val e = new Example(annot = accumAnnotation, nar = accumNarrative, _time = time.toString)
    println(Theory(compressedKernel).tostring)

    //Map("annotation" -> annotation, "narrative" -> narrative)

    val theory = findHypothesis(compressedKernel,
      examples = Map("annotation" -> e.annotationASP, "narrative" -> e.narrativeASP), jep=jep)

    println(theory.tostring)

    val testSet =
      dataset.asInstanceOf[iled.core.noisyILED.experimentsMLNdata.MLNDataHandler.TrainingSet].testingData.toIterator


    while (testSet.hasNext) {
      val e = testSet.next()
      println(e.time)
      evaluateTheory(theory, e, withInertia = true, jep, new GlobalValues)
    }

    val stats = theory.stats
    val (tps,fps,fns,precision,recall,fscore) = (stats._1, stats._2, stats._3, stats._4, stats._5, stats._6)
    val theorySize = theory.clauses.foldLeft(0)((x,y) => x + y.body.length + 1)
    logger.info(s"\ntps: $tps\nfps: $fps\nfns: $fns\nprecision: $precision\nrecall: $recall\nf-score: $fscore\ntraining time: " +
      s"$time\ntheory size: $theorySize")
    logger.info(s"\nDone. Theory found:\n ${theory.showWithStats}")
    jep.close()









    //Xhail.runXhail(fromFile=infile.getAbsolutePath, kernelSetOnly=false,fromWeakExmpl=false, jep=jep, bkFile=bk, learningTerminatedAtOnly=false)

  }

}

package mcts

import java.io.File
import java.util.UUID

import app.runners.{MLNDataHandler, OLEDRunner_MLNExperiments}
import app.runners.MLNDataHandler.MLNDataOptions
import app.runutils.Globals
import com.mongodb.casbah.MongoClient
import com.typesafe.scalalogging.LazyLogging
import datautils.caviar_intervals.MeetingTrainingData
import iled.ILED
import iled.ILED.updateSupport
import logic.Examples.Example
import logic._
import utils.DataUtils.{DataAsExamples, DataAsIntervals}
import utils.{ASP, CaviarUtils, Utils}
import xhail.Xhail

/**
  * Created by nkatz on 9/14/17.
  */

object HillClimbing extends App with LazyLogging {

  //runCaviarMLN()

  runCaviarFull()

  def runCaviarFull() = {
    Globals.glvalues("perfect-fit") = "false"

    val chunkSize = 1

    val data = MeetingTrainingData.getMeetingTrainingData(3, randomOrder = false)
    //List(MovingTrainingData.getMovingTrainingData(trainSetNum, randomOrder = randomOrder))

    val mongoClient = MongoClient()
    //val collection = mongoClient("caviar-whole")("examples")
    val collection = mongoClient("ctm")("examples")

    //def getTrainingData() = CaviarUtils.getDataFromIntervals(collection, "meeting", data.asInstanceOf[DataAsIntervals].trainingSet, chunkSize)
    def getTrainingData() = {
      val chunked =
        if (chunkSize > 1) collection.find().limit(100).map(x => Example(x)).sliding(chunkSize, chunkSize-1)
        else collection.find().limit(100).map(x => Example(x)).sliding(chunkSize, chunkSize)
      chunked map { x =>
        val merged = x.foldLeft(Example()) { (z, y) =>
          new Example(annot = z.annotation ++ y.annotation, nar = z.narrative ++ y.narrative, _time = x.head.time)
        }
        merged
      }
    }



    def iterateOnce(selectedNode: Theory, bottomTheory: Theory, gl: Globals) = {
      val children = generateChildrenNodes(selectedNode, bottomTheory, getTrainingData(), gl).filter(x => x != Theory())
      scoreNodes(children, gl)
      val f1 = (t: Theory) => t.stats._6
      val selectedChildNode = children.sortBy( x => -f1(x) ).head // sort by f1-score
      logger.info(s"Best theory so far (F1-score ${selectedChildNode.stats._6}):\n${selectedChildNode.tostring}")
      selectedChildNode
    }

    def scoreNodes(children: Vector[Theory], gl: Globals) = {
      logger.info("Scoring children nodes")
      children.foreach { childNode =>
        crossVal(childNode, getTrainingData(), "", gl)
      }
      //children.foreach(x => println(x.tostring + " " + x.stats._6))
    }

    //val globals = new Globals("/home/nkatz/dev/iled/datasets/Caviar/meeting", "")
    val globals = new Globals("/home/nkatz/dev/iled/datasets/CTM/whole-hierarchy")


    val bottomTheory = constructBottomTheory(getTrainingData(), globals)
    val iterations = 8
    val theory = (1 to iterations).foldLeft(Theory()) { (x, y) =>
      logger.info(s"Iteration $y")
      iterateOnce(x, bottomTheory, globals)
    }
    logger.info("Done")
    logger.info("Cross-validation...")
    val testSet = CaviarUtils.getDataFromIntervals(collection, "meeting", data.asInstanceOf[DataAsIntervals].testingSet, chunkSize, withChunking = false)
    val theory_ = Theory(theory.clauses)
    crossVal(theory_, testSet, "", globals) // generate new theory to clear the stats counter
    logger.info(s"F1-score on test set: ${theory_.stats._6}")
  }




  def runCaviarMLN() = {
    Globals.glvalues("perfect-fit") = "false"
    val foldPath = "/home/nkatz/dev/CAVIAR_MLN/CAVIAR_MLN/move/fold_2"
    val chunkSize = 50
    val opts = new MLNDataOptions(foldPath, chunkSize)
    val dataset = getData(opts)
    val globals = new Globals("/home/nkatz/dev/iled/datasets/CaviarMLN")
    val bottomTheory = constructBottomTheory(dataset, globals)
    //println(bottomTheory.tostring)
    val iterations = 2
    /*
    // Just a test. Run XHAIL:

    val exmpls = getData(new MLNDataOptions(foldPath, 10000)).next()
    val theory = Xhail.findHypothesis(bottomTheory.clauses,
                                      examples = Map("annotation" -> exmpls.annotationASP, "narrative" -> exmpls.narrativeASP),
                                      jep= new Jep(), globals = globals)

    logger.info(theory.tostring)
    */
    val theory = (1 to iterations).foldLeft(Theory()) { (x, y) =>
      logger.info(s"Iteration $y")
      iterateOnce(x, bottomTheory, globals, opts)
    }
    logger.info("Done")
    logger.info("Cross-validation...")
    val testSet = MLNDataHandler.getTestingData(opts)
    val theory_ = Theory(theory.clauses)
    crossVal(theory_, testSet, "", globals) // generate new theory to clear the stats counter
    logger.info(s"F1-score on test set: ${theory_.stats._6}")
    //iterateOnce(Theory(), bottomTheory, jep, globals, opts)
  }

  def getData(opts : MLNDataOptions) = MLNDataHandler.getTrainingData(opts)

  def iterateOnce(selectedNode: Theory, bottomTheory: Theory, gl: Globals, opts: MLNDataOptions) = {
    val children = generateChildrenNodes(selectedNode, bottomTheory, getData(opts), gl).filter(x => x != Theory())
    scoreNodes(children, gl, opts)
    val f1 = (t: Theory) => t.stats._6
    val selectedChildNode = children.sortBy( x => -f1(x) ).head // sort by f1-score
    logger.info(s"Best theory so far (F1-score ${selectedChildNode.stats._6}):\n${selectedChildNode.tostring}")
    selectedChildNode
  }

  def scoreNodes(children: Vector[Theory], gl: Globals, opts: MLNDataOptions) = {
    logger.info("Scoring children nodes")
    children.foreach { childNode =>
      crossVal(childNode, getData(opts), "", gl)
    }
    //children.foreach(x => println(x.tostring + " " + x.stats._6))
  }

  def generateChildrenNodes(currentNode: Theory, bottomTheory: Theory, trainingSet: Iterator[Example], gl: Globals) = {
    logger.info("Generating children nodes")
    trainingSet.foldLeft(Vector[Theory]()){ (theories, newExample) =>
      println(s"Generating children at example ${newExample.time}")
      val isSat = ILED.isSAT(currentNode, newExample, ASP.check_SAT_Program, gl)
      if (isSat) {
        theories
      } else {
        val aspFile: File = utils.Utils.getTempFile("aspInduction", ".lp")
        val (_, use2AtomsMap, defeasible, use3AtomsMap, _, _) =
          ASP.inductionASPProgram(kernelSet = bottomTheory, priorTheory = currentNode, examples = newExample.toMapASP, aspInputFile = aspFile, globals = gl)
        val answerSet = ASP.solve("iled", use2AtomsMap ++ use3AtomsMap, aspFile, newExample.toMapASP)
        if (answerSet != Nil) {
          val newRules = Rules.getNewRules(answerSet.head.atoms, use2AtomsMap)
          ILED.updateSupport(newRules, bottomTheory)
          val icRules = Rules.getInconsistentRules(answerSet.head.atoms, currentNode, use3AtomsMap)
          val retainedRules = Theory(currentNode.clauses.filter(x => icRules.forall(y => y.rule != x)))
          updateSupport(retainedRules, bottomTheory)
          //val refinedRules = Rules.getRefined(icRules, retainedRules, newRules, newExample, "fullSupport", jep, gl)
          val refinedRules = icRules.map(x => x.initialRefinement)
          val newTheory = new PriorTheory(retainedRules, newRules, Theory(refinedRules)).merge
          if (theories.exists(theory => theory.thetaSubsumes(newTheory) && newTheory.thetaSubsumes(theory))) theories else theories :+ newTheory
        } else {
          theories
        }
      }
    }//.map(x => x.initialRefinement)
  }

  def constructBottomTheory(trainingSet: Iterator[Example], globals: Globals): Theory = {
    val infile = Utils.getTempFile("example", ".lp")
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
          Xhail.runXhail(fromFile=infile.getAbsolutePath, kernelSetOnly=true, bkFile=bk, globals=globals)
        logger.info("Compressing bottom theory")

        val usefulNewBottomRules = varKernel.foldLeft(List[Clause]()) { (accum, bottomClause) =>
          if (ker.forall(p => ! bottomClause.thetaSubsumes(p))) {
            accum :+ bottomClause
          } else {
            accum
          }

        }

        //(ker ++ varKernel, annotAccum ++ y.annotation, narrativeAccum ++ y.narrative)
        (ker ++ usefulNewBottomRules, annotAccum ++ y.annotation, narrativeAccum ++ y.narrative)
      }
    //val compressedKernel = Theory(LogicUtils.compressTheory(accumKernel))
    val compressedKernel = Theory(accumKernel)
    compressedKernel
  }





  def crossVal(t: Theory, data: Iterator[Example], handCraftedTheoryFile: String = "", globals: Globals) = {
    while (data.hasNext) {
      val e = data.next()
      evaluateTheory(t, e, handCraftedTheoryFile, globals)
    }
    //val stats = t.stats
    //(stats._1, stats._2, stats._3, stats._4, stats._5, stats._6)
  }


  def evaluateTheory(theory: Theory, e: Example, handCraftedTheoryFile: String = "", globals: Globals): Unit = {

    val varbedExmplPatterns = globals.EXAMPLE_PATTERNS_AS_STRINGS
    val coverageConstr = s"${globals.TPS_RULES}\n${globals.FPS_RULES}\n${globals.FNS_RULES}"
    val t =
      if(theory != Theory()) {
        theory.clauses.map(x => x.withTypePreds(globals).tostring).mkString("\n")
      } else {
        globals.INCLUDE_BK(handCraftedTheoryFile)
      }
    val show = globals.SHOW_TPS_ARITY_1 + globals.SHOW_FPS_ARITY_1 + globals.SHOW_FNS_ARITY_1
    val ex = e.tostring
    val program = ex + globals.INCLUDE_BK(globals.BK_CROSSVAL) + t + coverageConstr + show
    val f = Utils.getTempFile(s"eval-theory-${UUID.randomUUID().toString}-${System.currentTimeMillis()}", ".lp")
    Utils.writeLine(program, f.getCanonicalPath, "overwrite")
    val answerSet = ASP.solve(task = Globals.INFERENCE, aspInputFile = f)
    if (answerSet.nonEmpty) {
      val atoms = answerSet.head.atoms
      atoms.foreach { a=>
        val lit = Literal.parse(a)
        val inner = lit.terms.head
        lit.functor match {
          case "tps" => theory.tps += inner.tostring
          case "fps" => theory.fps += inner.tostring
          case "fns" => theory.fns += inner.tostring
        }
      }
    }
  }




}

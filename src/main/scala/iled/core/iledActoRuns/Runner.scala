package iled.core.iledActoRuns

import akka.actor.Actor
import com.mongodb.casbah.commons.MongoDBObject
import com.typesafe.scalalogging.LazyLogging
import iled.core.Core
import iled.core.Iled._
import iled.core.noisyILED.TrainingSet
import iled.globalValues.GlobalValues
import iled.structures.Examples.Example
import iled.structures.{Literal, Theory, Clause, PriorTheory}
import iled.utils.CaviarUtils.Interval
import iled.utils._
import iled.utils.Utils._
import jep.Jep
import iled.core.Implicits._

import scala.util.{Failure, Success, Try}

/**
  * Created by nkatz on 12/20/15.
  */


// trainingPercent is the % of total number of intervals for this HLE used as positive examples
class Runner(val DB: Database, val HLE: String, val withWeaks: Boolean,
             val batchSize: Int, val withBacktr: Boolean = false,
             val weaksSelectionStrategy: Either[String, Int] = Right(50),
             val pruningPercent: Int, val data: DataSet, jep: Jep, step: Int) extends Actor with LazyLogging {

  val pruningThreshold: Either[(String, Int), Int] = Left((HLE, pruningPercent))
  val trainingSet = data.trainingSet
  val testingSet = data.testingSet
  val weakStrong = if (withWeaks) "weak" else "strong"

  def receive = {
    case "go" =>
      sender ! this.results
  }

  def results = scala.collection.mutable.Map[String, ResultsBean](weakStrong -> runIled)

  // The training set is given as a set of pairs of start-end points
  // For each such pair (s,e) we fetch data from s-step to e+step
  def runIled = {
    logger.info(s"${lined("Running configuration:")} \n" + (for ((k, v) <- Core.glvalues) yield s"$k: $v").mkString("\n"))
    println(s"Starting $weakStrong learning with ${this.HLE} on \n training set: ${this.trainingSet} \n testing set: ${this.testingSet}")
    val forwardPass = this.trainingSet.foldLeft((List[Example](), new PriorTheory(), List[Double]())) {
      (x, newInterval) =>
        val pastExs = x._1
        val theory = x._2
        val times = x._3
        val (exmpls, hypothesis, totalTime) = runOnInterval(theory, newInterval, withWeaks, jep)
        (pastExs ++ exmpls, hypothesis, times :+ totalTime)
    }
    val seenExmpls = forwardPass._1
    val theory = forwardPass._2
    var times = forwardPass._3
    val backwardPass = time {
      goBack(theory,seenExmpls,jep=jep,withWeaks=withWeaks)
    }
    val finalTheory = backwardPass._1
    times = times :+ backwardPass._2
    val totalTime = times.sum
    val (hypothesisSize,tps,fps,fns,precision,recall,f_score,_) =
      if (!withWeaks) {
        //wrapUp(DB = DB, theory = finalTheory, totalTime = times.sum, testingSet = this.testingSet, jep=jep)
        //val _data = CaviarUtils.getDataFromIntervals(DB, HLE, data.testingSet.map(x => new Interval(HLE,x._1,x._2)), batchSize, withChunking = false)
        val dataset = new TrainingSet(data.trainingSet.map(x => new Interval(HLE,x._1,x._2)),data.testingSet.map(x => new Interval(HLE,x._1,x._2)))
        cv(finalTheory.merge,DB,jep,dataset,totalTime)

      } else {
        RulePruning.pruneBadWeaks(seenExmpls,finalTheory.merge,DB,totalTime,pruningThreshold,testingSet=this.testingSet,jep=jep)
      }
    this.jep.close()

    logger.info(s"\ntps: $tps\nfps: $fps\nfns: $fns\nprecision: $precision\nrecall: $recall\nf-score: $f_score\ntraining time: " +
      s"$totalTime\ntheory size: $hypothesisSize")
    logger.info(s"\nDone. Theory found:\n ${finalTheory.merge.showWithStats}")

    new ResultsBean(theory=finalTheory.merge,theorySize=hypothesisSize, tps=tps,fps=fps,fns=fns,precision=precision,recall=recall,fscore=f_score,times=totalTime)
  }

  // This runs on a single interval (s,e). It backtracks on past examples of
  // the current interval according to the global 'withBacktr' parameter.
  def runOnInterval(pt: PriorTheory, currentInterval: (Int, Int), withWeaks: Boolean, jep: Jep): (List[Example], PriorTheory, Double) = {
    val batches = DB.getBatches(currentInterval._1, currentInterval._2, step, batchSize, withWeaks)
    val result =
      Try {
        time {
          batches.foldLeft((List[Example](), pt)) {
            (x, newBatch) =>
              val pastExs = x._1
              val theory = x._2
              val (_seenExs, _theory, _time) = withWeaks match {
                case true =>
                  iledTop(pastExs, Example.merge(newBatch.examples),
                    theory, weakBatch = newBatch, withBacktr = withBacktr,
                    weaksSelectionStrategy = weaksSelectionStrategy, jep=jep)
                case false => iledTop(pastExs, newBatch.asSingleExmpl, theory, withBacktr = withBacktr, jep=jep)
              }
              (_seenExs, _theory)
          }
        }
      }

    val (exmpls, hypothesis, totalTime) = result match {
      case Success(out) => (out._1._1, out._1._2, out._2)
      case Failure(ex) =>
        println(s"$ex\nTraining set:\n${this.trainingSet}\nWith weaks: $withWeaks")
        (Nil, new PriorTheory, 0.0)
    }
    (exmpls, hypothesis, totalTime)
  }

  def collectKernels = {
    def _collectKernels(interval: (Int, Int)) = {
      val batches = DB.getBatches(interval._1, interval._2, step, batchSize, usingWeakExmpls = false)
      val clauses = batches.foldLeft(List[Clause]()) {
        (x, newBatch) =>
          val (kernel, varKernel) = Utils.generateKernel(newBatch.asSingleExmpl.toMapASP, jep=jep)
          println(Theory(kernel).tostring)
          x ++ varKernel
      }
      clauses
    }
    this.trainingSet.foldLeft(List[Clause]()) {
      (x, y) =>
        val k = _collectKernels(y)
        x ++ k
    }
  }


  def cv(merged: Theory, db: Database, jep: Jep, trainingData: iled.core.noisyILED.TrainingSet, totalTime: Double) = {
    val hypothesisSize = merged.clauses.foldLeft(0)((x,y) => x + y.body.length + 1)
    val (tp,fp,fn,p,r,f) = crossVal(merged ,db, jep, globals = new GlobalValues, trainingData=trainingData)
    (hypothesisSize.toDouble,tp,fp,fn,p,r,f,totalTime)
  }

  /**
    *
    * Evaluate a hypothesis on the testing data
    *
    * @param t
    * @param DB
    * @param jep

    * @param testData This list contains exmplIds for the examples that should be used for testing. This is
    *                 used in case we train on particular dataset and test on another (that's the normal way to
    *                 do it, stuff with testingSetSize/trainingSetSize & take/drop are used only for developement/debugging)
    */
  def crossVal(t: Theory, DB: Database, jep: Jep, testData: List[Int] = Nil,
               globals: GlobalValues, trainingData: TrainingSet = TrainingSet(), handCraftedTheoryFile: String = "") = {

    //val testingIntervals = trainingData.testingSet
    //val data = CaviarUtils.getDataFromIntervals(DB,HLE,testingIntervals,chunkSize)
    val data = getTestingData(trainingData)
    while (data.hasNext) {
      val e = data.next()
      println(e.time)
      evaluateTheory(t, e, withInertia = true, jep, globals, handCraftedTheoryFile)
    }
    val stats = t.stats
    (stats._1.toDouble, stats._2.toDouble, stats._3.toDouble, stats._4.toDouble, stats._5.toDouble, stats._6.toDouble)
  }

  def getTestingData(trainingData: iled.core.noisyILED.TrainingSet): Iterator[Exmpl] = {
    if (trainingData.asInstanceOf[TrainingSet].testingSet == Nil) {
      // normally just return x below.
      val x = trainingData.asInstanceOf[iled.core.noisyILED.experimentsMLNdata.MLNDataHandler.TrainingSet].testingData.toIterator
      x
      // The following logic merges all the testing data into one interpretation (for better handling inertia during inference)
      /*
      val y = x.toList
      val time = y.head.time
      val merged = y.foldLeft(List[String](),List[String]()) { (p,q) =>
        val nar = p._1
        val anot = p._2
        (nar ++ q.narrative,anot ++ q.annotation)
      }
      val e = new Example(annot = merged._2, nar = merged._1, _time = time)
      val ee = new Exmpl(_id = e.time, exampleWithInertia = e)
      List(ee).toIterator
      */
    } else {
      CaviarUtils.getDataFromIntervals(DB, HLE, trainingData.testingSet, 0, withChunking = false)
    }
  }

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



}


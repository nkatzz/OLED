package all.core.oled

import akka.actor.{PoisonPill, Props, Actor}
import com.mongodb.casbah.commons.MongoDBObject
import com.typesafe.scalalogging.LazyLogging
import all.core.Core
import all.globalValues.GlobalValues
import all.structures.Examples.Example
import all.structures.{Literal, Theory}
import all.utils._
import all.core.Implicits._
import jep.Jep


/**
  * Created by nkatz on 28/2/2016.
  */


class ResultsContainer(val tps: Double, val fps: Double, val fns: Double,
                       val precision: Double, val recall: Double,
                       val fscore: Double, val theorySize: Double, val time: Double, val theory: Theory)


/*
class ResultsContainer(val theory: Theory, val theorySize: Double, val time: Double, val data: TrainingSet)
*/
class Dispatcher(val DB: Database, val delta: Double, val breakTiesThreshold: Double,
                 val postPruningThreshold: Double, val minSeenExmpls: Double, val trainingSetSize: Int,
                 val repeatFor: Int, val chunkSize: Int, withInertia: Boolean, withPostPruning: Boolean,
                 trainingData: TrainingSet = TrainingSet(), val HLE: String, handCraftedTheoryFile: String = "") extends Actor with LazyLogging {

  val WHOLE_DATA_SET_VALE = 1000000000
  var size = 2 // two processes are started, one for learning the initiatedAt part and one for the terminatedAt
  var theories = List[(Theory,Double)]()
  var merged = Theory()
  var time = 0.0
  //var done = false

  def receive = {

    case "EvaluateHandCrafted" =>
      println(s"Evaluating hand-crafted theory from $handCraftedTheoryFile")
      val crossValJep = new Jep()
      //crossValJep.runScript(GlobalValues.ASPHandler)
      val (tps,fps,fns,precision,recall,fscore) =
        crossVal(merged ,DB, crossValJep, trainingSetSize*chunkSize, globals = new GlobalValues, trainingData=trainingData, handCraftedTheoryFile=handCraftedTheoryFile)
      logger.info(s"\ntps: $tps\nfps: $fps\nfns: $fns\nprecision: $precision\nrecall: $recall\nf-score: $fscore\ntraining time:" +
        s"$time\ntheory size: 0.0")
      logger.info(s"\nDone. Theory found:\n ${merged.showWithStats}")
      crossValJep.close()


    case "start" =>

      context.actorOf(Props(
        new TheoryLearner(DB, delta, breakTiesThreshold, postPruningThreshold,
          minSeenExmpls, trainingSetSize, repeatFor, chunkSize, "initiated", withInertia=false, withPostPruning=withPostPruning, trainingData=trainingData, HLE)
      ), name = s"initiated-learner-${this.##}") ! "go"

///*
      context.actorOf(Props(
        new TheoryLearner(DB, delta, breakTiesThreshold, postPruningThreshold,
          minSeenExmpls, trainingSetSize, repeatFor, chunkSize, "terminated", withInertia=false, withPostPruning=withPostPruning, trainingData=trainingData, HLE)
      ), name = s"terminated-learner-${this.##}") ! "go"
//*/
    case x: (Theory,Double) =>
      theories = theories :+ x
      size -= 1
      sender ! PoisonPill // kill the child actor
      if(size == 0) {
        // merge the theories and do cross-validation
        val first = theories.head
        val second = theories.tail.head
        merged = first._1.clauses ++ second._1.clauses
        val theorySize = merged.clauses.foldLeft(0)((x,y) => x + y.body.length + 1)
        time = Math.max(first._2,second._2)

        logger.info("Performing cross-validation")

        //=============================================
        val crossValJep = new Jep()
        //crossValJep.runScript(GlobalValues.ASPHandler)
        //=============================================

        val (tps,fps,fns,precision,recall,fscore) =
          crossVal(merged ,DB, crossValJep, trainingSetSize*chunkSize, globals = new GlobalValues, trainingData=trainingData)
        logger.info(s"\ntps: $tps\nfps: $fps\nfns: $fns\nprecision: $precision\nrecall: $recall\nf-score: $fscore\ntraining time: " +
          s"$time\ntheory size: $theorySize")
        logger.info(s"\nDone. Theory found:\n ${merged.showWithStats}")
        crossValJep.close()

        context.parent ! new ResultsContainer(tps.toFloat,fps.toFloat,fns.toFloat,precision,recall,fscore,theorySize.toFloat,time,merged)

        println("sent results, shutting down")
        //context.stop(self)
        context.system.shutdown()
      }
  }


  /**
    *
    * Evaluate a hypothesis on the testing data
    *
    * @param t
    * @param DB
    * @param jep
    * @param testingSetSize this is used to drop the slice of the data on the which we didn't train
    *                       (so use them for testing). This works in conjunction with trainingSetSize
    *                       (see runWithDataChunks). E.g. if DB.size = 2000 and trainingSetSize = 1000
    *                       then runWithDataChunks will take the first 1000 examples for training. So,
    *                       here, we'll drop the first 1000 and use the rest for testing.
    * @param testData This list contains exmplIds for the examples that should be used for testing. This is
    *                 used in case we train on particular dataset and test on another (that's the normal way to
    *                 do it, stuff with testingSetSize/trainingSetSize & take/drop are used only for developement/debugging)
    */
  def crossVal(t: Theory, DB: Database, jep: Jep, testingSetSize: Int, testData: List[Int] = Nil,
               globals: GlobalValues, trainingData: TrainingSet = TrainingSet(), handCraftedTheoryFile: String = "") = {
    if (trainingData == TrainingSet()) {
      val dataIterator =
        if (testingSetSize < WHOLE_DATA_SET_VALE) DB.collection.find().sort(MongoDBObject("exampleId" -> 1)).drop(testingSetSize)
        else DB.collection.find().sort(MongoDBObject("exampleId" -> 1))
      while (dataIterator.hasNext) {
        val e = dataIterator.next()
        val ee = new Exmpl(e)
        println(ee.id)
        evaluateTheory(t, ee, withInertia = true, jep, globals, handCraftedTheoryFile)
      }

    } else {
      //val testingIntervals = trainingData.testingSet
      //val data = CaviarUtils.getDataFromIntervals(DB,HLE,testingIntervals,chunkSize)
      val data = getTestingData
      while (data.hasNext) {
        val e = data.next()
        println(e.time)
        evaluateTheory(t, e, withInertia = true, jep, globals, handCraftedTheoryFile)
      }
    }
    val stats = t.stats
    (stats._1, stats._2, stats._3, stats._4, stats._5, stats._6)
  }

  def getTestingData: Iterator[Exmpl] = {
    if (trainingData.asInstanceOf[TrainingSet].testingSet == Nil) {
      // normally just return x below.
      val x = trainingData.asInstanceOf[all.core.oled.experimentsMLNdata.MLNDataHandler.TrainingSet].testingData.toIterator
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
      CaviarUtils.getDataFromIntervals(DB,HLE,trainingData.testingSet,chunkSize, withChunking = false)
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

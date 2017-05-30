package oled_distributed

import akka.actor.{Actor, Props}
import app.{Globals, InputParameters}
import com.mongodb.casbah.MongoClient
import com.typesafe.scalalogging.LazyLogging
import jep.Jep
import logic.{LogicUtils, Theory, Clause}
import oled_distributed.Structures.{FinalTheoryMessage, Initiated, Terminated}
import utils.DataUtils.{DataAsIntervals, Interval}
import utils.{Database, Exmpl}

/**
  * Created by nkatz on 3/14/17.
  *
  *
  * This actor starts two top-level actors to coordinate learning
  * the initiated and the terminated part of the theory respectively.
  *
  */

class Dispatcher(val databases: List[String] = Nil,
                 val intervals: List[DataAsIntervals] = Nil,
                 val masterDB: String = "None",
                 val getDataFunction: (String, String, Int, DataAsIntervals) => Iterator[Exmpl],
                 val inputParams: InputParameters) extends Actor with LazyLogging {

  private var counter = 2
  private var initTheory = List[Clause]()
  private var termTheory = List[Clause]()
  private var initTrainingTime = ""
  private var termTrainingTime = ""
  private var theory = List[Clause]() // this is for future use with single-predicate learning
  private var theoryTrainingTime = ""
  private var initTotalMsgNum = 0
  private var initTotalMsgSize = 0L
  private var termTotalMsgNum = 0
  private var termTotalMsgSize = 0L

  def updateMessages(m: FinalTheoryMessage, what: String) = {
    what match {
      case "init" =>
        initTotalMsgNum = m.totalMsgNum
        initTotalMsgSize = m.totalMsgSize
      case "term" =>
        termTotalMsgNum = m.totalMsgNum
        termTotalMsgSize = m.totalMsgSize
      case _ => logger.info("UNKNOWN MESSAGE!")
    }
  }

  def receive = {
    case "go" =>
      context.actorOf(Props(
        new TopLevelActor(databases, Utils.getDataFromDB, inputParams, targetConcept = new Initiated, masterDB, intervals ) ), name = "InitTopLevelActor") ! "go"
      context.actorOf(Props(
        new TopLevelActor(databases, Utils.getDataFromDB, inputParams, targetConcept = new Terminated, masterDB, intervals) ), name = "TermTopLevelActor") ! "go"

    /*---------------------------------------------------------------------------*/
    // For debugging (trying to see if the sub-linear speed-up is due to blocking)
    /*---------------------------------------------------------------------------*/
    case "go-no-communication" =>
      context.actorOf(Props(
        new TopLevelActor(databases, Utils.getDataFromDB, inputParams, targetConcept = new Initiated, masterDB, intervals ) ), name = "InitTopLevelActor") ! "go-no-communication"
      context.actorOf(Props(
        new TopLevelActor(databases, Utils.getDataFromDB, inputParams, targetConcept = new Terminated, masterDB, intervals) ), name = "TermTopLevelActor") ! "go-no-communication"


    case msg: FinalTheoryMessage =>
      msg.targetPredicate match {
        case x: Initiated =>
          this.initTheory = msg.theory
          this.initTrainingTime = msg.trainingTime
          updateMessages(msg, "init")

        case x: Terminated =>
          this.termTheory = msg.theory
          this.termTrainingTime = msg.trainingTime
          updateMessages(msg, "term")

        case _ =>
          this.theory =  msg.theory
          this.theoryTrainingTime = msg.trainingTime
          //updateMessages(msg)
      }
      counter -= 1
      if (counter == 0) {
        logger.info(s"\n\nThe initiated part of the theory is\n${Theory(this.initTheory).showWithStats}\nTraining" +
          s" time: $initTrainingTime\nTotal messages: $initTotalMsgNum\nTotal message size: $initTotalMsgSize")
        logger.info(s"\n\nThe terminated part of the theory is\n${Theory(this.termTheory).showWithStats}\nTraining" +
          s" time: $termTrainingTime\nTotal messages: $termTotalMsgNum\nTotal message size: $termTotalMsgSize")
        /*
        * Cross-validation...
        * */
        if(intervals.nonEmpty) { // works for 10-fold cross-val from intervals
          crossVal()
        }
        val mongoClient = MongoClient()
        databases foreach{ x =>
          println(s"Dropping db $x")
          mongoClient.dropDatabase(x)
        }
        context.system.shutdown()
      }
  }


  def showTheory(t: Theory) = {
    val showClause  = (c: Clause) => {
      s"score (${if (c.head.functor=="initiatedAt") "precision" else "recall"}):" +
        s"${c.score}, tps: ${c.tps}, fps: ${c.fps}, fns: ${c.fns} Evaluated on: ${c.seenExmplsNum} examples\n$$c.tostring}"
    }
    t.clauses.map(x => showClause(x)).mkString("\n")
  }

  def crossVal() = {
    val merged_ = Theory(this.initTheory ++ this.termTheory)
    val compressed = Theory(LogicUtils.compressTheory(merged_.clauses))

    /*------------------*/
    // DEBUGGING-TESTING
    /*------------------*/

    val filtered = Theory(compressed.clauses.filter(x => x.tps > 50))

    val crossValJep = new Jep()

    val (tps,fps,fns,precision,recall,fscore) =
      Functions.crossVal(filtered, new Database(masterDB), crossValJep, 0,
        new DataAsIntervals(trainingSet=Nil, testingSet=intervals.head.testingSet), "",  new Globals(inputParams.entryPath, ""), "meeting", 0)

    val time = Math.max(this.initTrainingTime.toDouble, this.termTrainingTime.toDouble)

    val theorySize = filtered.clauses.foldLeft(0)((x,y) => x + y.body.length + 1)

    logger.info(s"\ntps: $tps\nfps: $fps\nfns: $fns\nprecision:" +
      s" $precision\nrecall: $recall\nf-score: $fscore\ntraining time: " +
      s"$time\ntheory size: $theorySize\n" +
      s"Total number of messages: ${initTotalMsgNum+termTotalMsgNum}\n" +
      s"Total message size: ${initTotalMsgSize+termTotalMsgSize}")

    logger.info(s"\nDone. Theory found:\n ${filtered.showWithStats}")
    logger.info(s"Mean time per batch: ${Globals.timeDebug.sum/Globals.timeDebug.length}")
    logger.info(s"Total batch time: ${Globals.timeDebug.sum}")
    crossValJep.close()
  }


}

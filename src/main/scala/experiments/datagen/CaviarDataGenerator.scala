package experiments.datagen

import com.mongodb.casbah.Imports._
import com.mongodb.casbah.MongoCollection
import com.mongodb.casbah.commons.MongoDBObject
import com.typesafe.scalalogging.LazyLogging
import experiments.datagen.Data._
import iled.utils.Utils


import scala.annotation.tailrec
import scala.util.Random

/**
  * Created by nkatz on 11/7/15.
  */

object CaviarDataGenerator extends LazyLogging{



  val maxIntervalSpan = 30 // minimum & maximum number of time points in an interval
  val maxDownTimeIntervalSpan = 100
  val minIntervalSpan = 10
  val coordsUpper = 100 // all coords are in 1..100
  val orientUpper = 100 // all orients are in 1..100
  val percentOfStrongExamples = 20.0

  //val DB = s"CAVIAR_SYNTH-${percentOfStrongExamples.toInt}%-strong" // The name of the db to generate/update
  val DB = s"CAVIAR_SYNTH-test-set"

  val COLLECTION = "examples" // The mongo collection name
  val REPEAT_FOR = 2 // repeat the generating loop, shuffling the combinations of init-term conditions at each loop

  val CWD = System.getProperty("user.dir")
  val APP_PATH = s"$CWD/Caviar_synth"
  val BK_FILE =  s"$APP_PATH/bk.lp"
  val MODES_FILE = s"$APP_PATH/modes"
  val BK_NO_INERT_FILE = s"$APP_PATH/bk-no-inertia.lp"
  val BK_MARKED_FILE = s"$APP_PATH/bk-score-initiated.lp"


  val distance = (x1: Int, y1: Int, x2: Int, y2: Int) =>
    Math.sqrt(Math.pow(Math.abs(x1 - x2), 2) + Math.pow(Math.abs(y1 - y2), 2))
  val allBehaviours =
    Fighting.initiateBehaviours ++ Fighting.terminateBehaviours ++
      Meeting.initiateBehaviours ++ Meeting.terminateBehaviours ++
      Moving.initiateBehaviours ++ Moving.terminateBehaviours ++ LeavingObject.initiateBehaviours ++ LeavingObject.terminateBehaviours
  val allCoords =
    Fighting.initCoords ++ Fighting.termCoords ++
      Meeting.initCoords ++ Meeting.termCoords ++
      Moving.initCoords ++ Moving.termCoords ++ LeavingObject.initCoords ++ LeavingObject.termCoords
  val allOrientations =
    Fighting.initOrientations ++ Fighting.termOrientations ++
      Meeting.initOrientations ++ Meeting.termOrientations ++
      Moving.initOrientations ++ Moving.termOrientations ++ LeavingObject.initOrientations ++ LeavingObject.termOrientations


  def generateForExperiments(id: String, posPercentPerInterval: Int): String = {

    val mongoClient = MongoClient()
    val dbname = s"CAVIAR_SYNTH-${posPercentPerInterval.toInt}%-strong-$id"
    val collection = mongoClient(dbname)(COLLECTION)
    collection.drop // clear it in any case
    generateData(collection,REPEAT_FOR,posPercentPerInterval.toDouble)
    mongoClient.close()
    setModesAndBK
    dbname
  }

  def main(args: Array[String]): Unit = {
    val mongoClient = MongoClient()
    val collection = mongoClient(DB)(COLLECTION)
    collection.drop // clear it in any case
    generateData(collection,REPEAT_FOR, percentOfStrongExamples)
    mongoClient.close()
    setModesAndBK
  }

  // create appropriate set of modes according to the data
  // and add data-specific extra bk (e.g. definitions of entites, time etc.)
  def setModesAndBK = {
    val knowledge =
      (allBehaviours.foldLeft(List[String]()){
        (list,tuple) => (list :+ tuple._1) :+ tuple._2
      }.distinct map (
        p =>
          (if (List("inactive","appears","disappears").contains(p)) s"modeb(happensAt($p(+object),+time))" else s"modeb(happensAt($p(+person),+time))",
            s"time(X):- happensAt($p(_),X).",
            s"person(X):-happensAt($p(X),_).")
        )).foldLeft( ((List[String](),List[String](),List[String]())))((x,y) => (x._1 :+ y._1,x._2 :+ y._2,x._3 :+ y._3))

    val modesFile = Utils.createOrClearFile(this.MODES_FILE)
    val bkFile = Utils.createOrClearFile(this.BK_FILE)
    val bkNoInertFile = Utils.createOrClearFile(this.BK_NO_INERT_FILE)
    val bkNoInertMarkedFile = Utils.createOrClearFile(this.BK_MARKED_FILE)
    Utils.writeLine((BK.modes ++ knowledge._1 ++ BK.exPatterns).mkString("\n"),modesFile,"append")
    Utils.writeLine((List(BK.bk) ++ (knowledge._2 :+ "time(T):-example(holdsAt(_,T)).") ++ knowledge._3 ).mkString("\n"),bkFile,"append")
    Utils.writeLine((List(BK.bkNoInertia) ++ (knowledge._2 :+ "time(T):-example(holdsAt(_,T)).") ++ knowledge._3 ).mkString("\n"),bkNoInertFile,"append")
    Utils.writeLine((List(BK.bkNoInertiaMarked) ++ (knowledge._2 :+ "time(T):-example(holdsAt(_,T)).") ++ knowledge._3 ).mkString("\n"),bkNoInertMarkedFile,"append")



  }

  def generateData(collection: MongoCollection, repeatFor: Int = 1, percentOfStrong: Double = 100.0) = {
    def storeData(interval: TimeInterval) = {
      def toMongo(i: Interpretation) = {
        val entry =
          MongoDBObject("time" -> i.time)++
            ("annotation" -> i.annotation)++
            ("narrative" -> i.narrative)
        collection.insert(entry)
      }
      interval.interpretations foreach (x => toMongo(x))
    }

    val fight = new Fighting()
    val meet = new Meeting()
    val move = new Moving()
    //val leave = new LeavingObject()
    //val downTimeBehaviours = fight.notHoldsBehaviors ++ meet.notHoldsBehaviors ++ move.notHoldsBehaviors ++ leave.notHoldsBehaviors
    val downTimeBehaviours = fight.notHoldsBehaviors ++ meet.notHoldsBehaviors ++ move.notHoldsBehaviors

    val combinations =
      //Random.shuffle(List(fight,meet,move,leave).map{
      Random.shuffle(List(fight,meet,move).map{
        p => for (i <- p.initiateBehaviours;
                  j <- p.terminateBehaviours) yield (p,i,j)
      }.flatten)
    var interval = Interval()
    var downTime = DownTimeInterval()
    var endTime = 0
    var startTime = 10

    val show = (x:TimeInterval) => (x.interpretations map (p => (p.narrative ++ p.annotation).mkString(" "))).mkString("\n")
    for (i <- 1 to repeatFor) {
      val _combinations = Random.shuffle(combinations)
      for (combination <- _combinations) {
        interval = Interval(combination._1, startTime, combination._2, combination._3, percentOfStrong)
        storeData(interval)
        logger.info("\n\nINTERVAL\n\n" + show(interval) + s"\nstart: ${interval.startTime}, end: ${interval.endTime}, innerPoints: ${interval.inner.map(x => x.narrative.mkString(" "))}")
        endTime = interval.endTime
        downTime = DownTimeInterval(endTime + 1, Random.shuffle(downTimeBehaviours).head)
        storeData(downTime)
        logger.info("\n\nDOWNTIME\n\n" + show(downTime))
        endTime = downTime.endTime
        startTime = endTime + 1
      }
    }
  }




  /* Represents an interval in which an HLE holds */

  def createInterpretation(HLE: HighLevelEvent, p1: String, p2: String,
                           time: String, what: String, LLEs: (String, String) = ("", "")): Interpretation = {
    HLE.name match {
      case "dummy" =>  // create a downtime interval
        val coords = Random.shuffle(DownTime.coords).head
        val orientations = Random.shuffle(DownTime.orientations).head
        val visibility = (Random.shuffle(List("visible", "invisible")).head,
          Random.shuffle(List("visible", "invisible")).head)
        val lles = LLEs
        val annotation = List()
        val _p1 = new Person(p1, lles._1, coords._1, coords._2, orientations._1, visibility._1, time)
        val _p2 = new Person(p2, lles._2, coords._3, coords._4, orientations._2, visibility._2, time)
        new Interpretation(_p1, _p2, annotation, time.toInt)
      case _ => // create a regular interval
        val coords = what match {
          case ("initiation" | "StrongIntervalPoint") => Random.shuffle(HLE.initCoords).head
          case "termination" => Random.shuffle(HLE.termCoords).head
          case ("weakIntervalPoint" | "NoIntervalPoint") => Random.shuffle(allCoords).head
        }
        val orientations = what match {
          case ("initiation" | "StrongIntervalPoint") => Random.shuffle(HLE.initOrientations).head
          case "termination" => Random.shuffle(HLE.termOrientations).head
          case ("weakIntervalPoint" | "NoIntervalPoint") => Random.shuffle(allOrientations).head
        }
        val visibility = what match {
          case ("initiation" | "termination" | "weakIntervalPoint" | "StrongIntervalPoint") => ("visible", "visible")
          case "NoIntervalPoint" =>
            (Random.shuffle(List("visible", "invisible")).head,
              Random.shuffle(List("visible", "invisible")).head)
          case _ => ("", "")
        }
        val lles = LLEs match {
          case ("", "") =>
            what match {
              case ("initiation" | "StrongIntervalPoint") => Random.shuffle(HLE.initiateBehaviours).head
              case "termination" => Random.shuffle(HLE.terminateBehaviours).head
              case "weakIntervalPoint" => Random.shuffle(HLE.holdsBehaviours).head
              case "NoIntervalPoint" => Random.shuffle(HLE.notHoldsBehaviors).head
              case _ => ("", "")
            }
          case _ => LLEs
        }
        val annotation = what match {
          case "initiation" => List()
          case "termination" =>
            if (HLE.name != "leavingObject") {
              List(s"holdsAt(${HLE.name}($p1,$p2),$time)", s"holdsAt(${HLE.name}($p2,$p1),$time)")
            } else {
              List(s"holdsAt(${HLE.name}($p1,$p2),$time)")
            }
          case ("weakIntervalPoint" | "StrongIntervalPoint") =>
            if (HLE.name != "leavingObject") {
              List(s"holdsAt(${HLE.name}($p1,$p2),$time)", s"holdsAt(${HLE.name}($p2,$p1),$time)")
            } else {
              List(s"holdsAt(${HLE.name}($p1,$p2),$time)")
            }
          case "NoIntervalPoint" => List()
        }
        val _p1 = new Person(p1, lles._1, coords._1, coords._2, orientations._1, visibility._1, time)
        val _p2 = new Person(p2, lles._2, coords._3, coords._4, orientations._2, visibility._2, time)

        new Interpretation(_p1, _p2, annotation, time.toInt)
    }
  }

  def getCoords(upper: Int, threshold: Int, what: String) = {
    val next = (x: Unit) => Random.nextInt(upper)
    var done = false
    val test = (x: Int, y: Int, z: Int, t: Int) => what match {
      case "smallerThan" => distance(x, y, z, t) <= threshold
      case "greaterThan" => distance(x, y, z, t) > threshold
    }
    var (x1, y1, x2, y2) = (0, 0, 0, 0)
    while (!done) {
      x1 = next();
      y1 = next();
      x2 = next();
      y2 = next()
      if (test(x1, y1, x2, y2)) done = true
    }
    (x1, y1, x2, y2)
  }

  def getOrientations(upper: Int, threshold: Int, what: String) = {
    val next = (x: Unit) => Random.nextInt(upper)
    var done = false
    val test = (x: Int, y: Int) => what match {
      case "smallerThan" => Math.abs(x - y) <= threshold
      case "greaterThan" => Math.abs(x - y) > threshold
    }
    var (x, y) = (0, 0)
    while (!done) {
      x = next();
      y = next()
      if (test(x, y)) done = true
    }
    (x, y)
  }

  trait TimeInterval {
    val interpretations: List[Interpretation]
  }

  trait HighLevelEvent {

    val name: String = ""
    val initiateBehaviours: List[(String, String)] = List()
    val terminateBehaviours: List[(String, String)] = List()
    /*
     Combinations of LLEs that occur when the HLE holds within an interval
     (weak examples). Such a combination may be any combination of LLEs
     that does not strongly terminate the HLE, or it does not strongly initiate other HLEs i.e.

      holdsBehaviours = allBehaviours - this.terminateBehaviours - (allHLEs - this).initiateBehaviours

      Then for this combination
     one on the following holds, indicated by the data:
     (a) It re-initiates the HLE, in which case we may learn a rule from
         the particular example.
     (b) It is a notHolds combination, in which case the rule learnt from
         the particular example covers negatives and (given the no noise
         assumption) it should be discarded (along with its support set)
     (c) This combination never appears in the data again. Then a rule
         learnt from the particular example is consistent, but it does not
         contribute to the coverage of the hypothesis. If such rules occur
         in a hypothesis they may be removed by post-pruning, to reduce the
         size and the complexity of the hypohthesis.
    */
    val holdsBehaviours: List[(String, String)] = List()
    /*
      Combinations of LLEs that occur when the LLE does not hold, i.e. they
      occur outside of an interval. Such combinations are

      holdsBehaviours = allBehaviours - this.initiateBehaviours

      (there is an overlapping between holds and notHolds as intended).
     */
    val notHoldsBehaviors: List[(String, String)] = List()
    val initCoords: List[(Int, Int, Int, Int)] = List()
    val termCoords: List[(Int, Int, Int, Int)] = List()
    // holds/notHolds thresholds (occurs inside/outside an interval)
    // may have any value. A holds threshold cannot terminate the
    // HLE because terminating behaviours are excluded from within the
    // interval and similarly an notHolds thresholds cannot initiate
    // the HLE because initiating behaviours are excluded outside the
    // interval. The same holds for orientation thresholds.Orientation
    // thresholds are treated similarly
    val initOrientations: List[(Int, Int)] = List()
    val termOrientations: List[(Int, Int)] = List()
    // utility functions
    val f_orients = (x: List[Int], y: List[Int], z: List[Int]) => if (x == y) x else z.filter(p => !y.contains(p))
    val f_holds = (x: List[(String, String)], y: List[(String, String)]) => x.filter(p => !y.contains(p))
    val f_thresholds = (x: List[Int], y: List[Int]) => x.filter(p => !y.contains(p))

  }

  class Interpretation(val p1: Person,
                       val p2: Person,
                       val annotation: List[String], val time: Int) {

    val narrative = (s"starttime($time)" :: p1.content) ++ p2.content

  }

  class Person(id: String,
               LLE: String,
               xCoord: Int,
               yCoord: Int,
               orientation: Int,
               visibility: String,
               time: String) {

    val content = List(s"happensAt($LLE($id),$time)", s"holdsAt($visibility($id),$time)",
      s"orientation($id,$orientation,$time)", s"coords($id,$xCoord,$yCoord,$time)")

  }

  class Object(id: String,LLE:String,xCoord: Int, yCoord: Int,visibility: String,time: String) {
    val content = List(s"happensAt($LLE($id),$time)", s"holdsAt($visibility($id),$time)",
       s"coords($id,$xCoord,$yCoord,$time)")
  }

  object Interval {

    def apply() = {
      new Interval(new DummyHLE, 0, ("", ""), ("", ""), 0.0)
    }

    def apply(HLE: HighLevelEvent, startTime: Int, startLLEs: (String, String),
              endLLEs: (String, String), percentageOfStronglyInitiated: Double) = {
      new Interval(HLE, startTime, startLLEs, endLLEs, percentageOfStronglyInitiated)
    }
  }

  @tailrec
  private def sampleN(N: Int, sampleFrom: List[Any], sample: List[Any]): List[Any] = {
    sample.length match {
      case N => sample
      case _ =>
        val newValue = Random.shuffle(sampleFrom).head
        val newSample =
          if (!sample.contains(newValue))
            sample :+ newValue
          else sample
        sampleN(N, sampleFrom, newSample)
    }
  }

  class Interval(val HLE: HighLevelEvent,
                 val startTime: Int,
                 val startLLEs: (String, String),
                 val endLLEs: (String, String),
                 val percentageOfStronglyInitiated: Double) extends TimeInterval{




    private val s = sampleN(2,ids.toList,List())
    private val (person1, person2) = (s(0).asInstanceOf[String],s(1).asInstanceOf[String])
    val _endTime = sampleN(1,List.range(minIntervalSpan,maxIntervalSpan),List()).head.asInstanceOf[Int]
    val endTime = startTime + _endTime
    private val init = createInterpretation(HLE, person1, person2, startTime.toString, "initiation", startLLEs)
    private val term = createInterpretation(HLE, person1, person2, endTime.toString, "termination", endLLEs)
    private val numOfStronglyInitiatedPoints =
      ((endTime - startTime) * percentageOfStronglyInitiated / 100.0).ceil.toInt
    val inner = createInnerExamples
    val interpretations = (init :: inner) :+ term

    private def createInnerExamples = {
      if (numOfStronglyInitiatedPoints > 1) {
        val strongPoints = sampleN(numOfStronglyInitiatedPoints, (startTime + 1 to endTime - 1).toList, List())
        val weakPoints = (startTime + 1 to endTime - 1).toList filter (p => !strongPoints.contains(p))
        val strongExamples = strongPoints map { p => createInterpretation(HLE, person1, person2, p.toString, "StrongIntervalPoint") }
        val weakExamples = weakPoints map { p => createInterpretation(HLE, person1, person2, p.toString, "weakIntervalPoint") }
        (strongExamples ++ weakExamples).sortBy(p => p.time)
      } else {
        List()
      }
    }

  }


  object DownTimeInterval {
    def apply() = {
      new DownTimeInterval(0,("",""))
    }
    def apply(startTime:Int,downTimeBehaviours: (String, String)) = {
      new DownTimeInterval(startTime,downTimeBehaviours)
    }

  }

  class DownTimeInterval(val startTime: Int, downTimeBehaviours: (String, String)) extends TimeInterval{
    private val s = sampleN(2,ids.toList,List())
    private val (person1, person2) = (s(0).asInstanceOf[String],s(1).asInstanceOf[String])
    val endTime = startTime + Random.nextInt(maxDownTimeIntervalSpan)+1 //add 1 to avoid sampling 0 and having the same start-end points
    val interpretations = (for (i <- startTime to endTime) yield
      createInterpretation(new DummyHLE,person1,person2,i.toString,"",downTimeBehaviours)).toList
  }




}

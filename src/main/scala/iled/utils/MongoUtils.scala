package iled.utils

import java.io.File

import com.mongodb.BasicDBObject
import com.mongodb.casbah.Imports._
import com.mongodb.casbah.MongoClient
import com.mongodb.casbah.commons.MongoDBObject
import iled.core.Core
import iled.core.noisyILED.old_stuff.ILEDHoeffdingNoSupportTermInit
import iled.globalValues.GlobalValues
import iled.parsers.ClausalLogicParser
import iled.structures.Examples.{ExampleBatch, Example}
import iled.structures.{Constant, Literal}
import jep.Jep
import scala.collection.mutable.ListBuffer
//import scala.util.parsing.combinator.Parsers.Parser
//import scala.util.parsing.combinator.Parsers.~
import scala.collection.immutable.SortedMap

/**
  * Created by nkatz on 11/8/15.
  */

trait MongoUtils {

  def ctmToMongo = {

    /*
     * Creates a new db from the old CTM Jason dump (the same that exists at estia).
     * Gets rid of various redundant stuff from the new database and uses the
     * identifiers for narrative and annotation that we use throughout the application
     * ("narrative" and "annotation"). The old db uses "pos" and "nar" respectively
     *
     */

    // connection to the old database
    val mongoClient1 = MongoClient()
    val collection1 = mongoClient1("ctm-old")("examples")
    // connection to the new database
    val mongoClient2 = MongoClient()
    val collection2 = mongoClient2("CTM")("examples")
    collection2.drop() //clear
    var accum = List[Example]()
    for (x <- collection1.find().sort(MongoDBObject("time" -> 1))) {
      val annot = x.asInstanceOf[BasicDBObject].get("pos").asInstanceOf[BasicDBList].toList.map(x => x.toString)
      val narrative = x.asInstanceOf[BasicDBObject].get("nar").asInstanceOf[BasicDBList].toList.map(x => x.toString)
      val prev = x.asInstanceOf[BasicDBObject].get("innert").asInstanceOf[BasicDBList].toList.map(x => x.toString)
      val time = x.asInstanceOf[BasicDBObject].get("example")
      accum = accum :+ new Example(annot = annot, nar = narrative, _time=time.toString)
      //val annotation = annot ++ prev

    }
    val _accum = accum.sortBy(x => x.time.toInt) // for some reason some things are unsorted in the original db
    accum = _accum
    for (x <- accum) {
      val entry = MongoDBObject("time" -> x.time.toInt) ++ ("annotation" -> x.annotation) ++ ("narrative" -> x.narrative)
      println(entry)
      collection2.insert(entry)
    }

    mongoClient1.close()
    mongoClient2.close()
  }



  /**
    * Helper container class for getDBsWithTimes()
    */

  case class DB(name: String) {
    var times: List[Int] = List()
  }

  /**
    * Helper method returns all DB names and the time points in every DB.
    */
  def getDBsWithTimes(): List[DB] = {
    val alldbs = new ListBuffer[DB]
    val dbs = Utils.getAllDBs()
    for (db <- dbs) {
      val database = DB(db)
      val mongoClient = MongoClient()
      val collection = mongoClient(db)("examples")
      for (x <- collection.find().sort(MongoDBObject("time" -> 1))) {
        val time = x.asInstanceOf[BasicDBObject].get("time").asInstanceOf[Int]
        database.times = database.times :+ time
      }
      database.times = database.times.sorted
      alldbs += database
      mongoClient.close()
    }
    alldbs.toList
  }


}


class Database(val name: String, val collectionName: String = "examples") {

  val mongoClient = MongoClient()

  private val _collection = mongoClient(name)(collectionName)

  val collection = this._collection

  var startTime = 0

  val endTime = collection.last.asInstanceOf[BasicDBObject].get("time").toString.toInt

  val isEmpty = this.collection.findOne() match {
    case Some(x) =>
      this.startTime = x.asInstanceOf[BasicDBObject].get("time").toString.toInt
      false
    case _ => true
  }

  val nonEmpty = if (this.isEmpty) false else true

  val size = this.collection.size

  def close() = this.mongoClient.close()

  def inspectDB(seeWhat: String = "time") = {
    for (x <- this.collection.find().sort(MongoDBObject("time" -> 1))) {
      val e = Example(x)
      seeWhat match {
        case "time" => println(e.time)
        case "annotation" => println(e.annotation)
        case "narrative" => println(e.narrative)
        case "example" => println(e)
      }

    }
  }

  /**
    * Retrieves a batch of examples from this database
    *
    * @param startTime start from example with time = startTime
    * @param howMany pack howMany examples in one batch
    * @return the batch as an Example instance and the time stamp of the last
    *         example in the bacth (used as start time for the next batch)
    */


  def getBatch1(startTime: Int, howMany: Int, usingWeakExmpls: Boolean = false): (Example,Int) = {
    //def batchQuery = this.collection.find("time" $gte startTime).limit(howMany)
    val _batch = this.collection.find("time" $gte startTime).sort(MongoDBObject("time" -> 1)).limit(howMany)
    val batch = _batch.toList
    val endTime = batch.last.asInstanceOf[BasicDBObject].get("time").toString
    (Example(batch.toList, usingWeakExmpls),endTime.toInt)
  }


  def getBatch(startTime: Int, howMany: Int, usingWeakExmpls: Boolean = true): (ExampleBatch,Int) = {
    val _batch = this.collection.find("time" $gte startTime).sort(MongoDBObject("time" -> 1)).limit(howMany)
    val x = _batch.toList
    val batch = if (x.isEmpty) {
      // We're out of data, so fetch more data from the beginning of the db
      val b = this.collection.find("time" $gte 0).sort(MongoDBObject("time" -> 1)).limit(howMany)
      b.toList
    } else { x }
    //val batch = _batch.toList
    val endTime = if (batch.nonEmpty) batch.last.asInstanceOf[BasicDBObject].get("time").toString.toInt else startTime
    (ExampleBatch(batch.toList, usingWeakExmpls), endTime.toInt)
  }


  /* startTime and endTime are respectively starting and ending points for an interval of
  *  the form (s,e). This method partitions the input interval into batches of a specified
  *  batch size (indicated by the howMany parameter) and returns a list of these batches.*/

  def getBatches(startTime: Int, endTime: Int, step: Int, howMany: Int, usingWeakExmpls: Boolean = true): List[ExampleBatch] = {
    val batches = new ListBuffer[ExampleBatch]
    var start = startTime
    while ( (start <= endTime - step) && (start < this.endTime) ) {
      //println(s"start: $start")
      //println(s"end: $endTime")
      val _batch =
        this.collection.find("time" $gte start - 2*step $lte endTime + 2*step).sort(MongoDBObject("time" -> 1)).limit(howMany)
      val batch = _batch.toList
      batches += ExampleBatch(batch.toList, usingWeakExmpls)
      start = batch.last.asInstanceOf[BasicDBObject].get("time").toString.toInt
    }
    batches.toList
  }

}


/* Stuff for CAVIAR experiments */

object CaviarUtils {

  def main(args: Array[String]): Unit = {
    val db = new Database(Core.fromDB)
    println(findIntervals(db, "fighting"))
  }

  def findIntervals(DB: Database, hle: String) = {
    /*
    def getTimes = {
      var times = List[Int]()
      for (x <- DB.collection.find()) {
        val e = Example(x)
        times = times :+ e.time.toInt
      }
      times
    }
    val getFirst = DB.collection.findOne()
    */
    def samePair(x: (String,String), y:(String,String)) = y._1 == x._2 && y._2 == x._1
    var pairs = List[(String,String)]()
    def idPair(e: Example, hle: String) = {}
    var previous = Example()
    for (x <- DB.collection.find().sort(MongoDBObject("time" -> 1))) {
      val e = Example(x)
      // set previous to e before exiting the iteration
    }

  }

  /**
    *
    * This method generates the database with examples as pairs. The schema of the DB is:
    * exampleId: an integer id of the example
    * time: the time stamp of the example
    * inertia: contains the example WITH inertia annotation
    * noInertia: contains the example WITHOUT inertia annotation
    *
    * inertia and noInertia fields carry an DBObject with the regular example schema (time, narrativem annotation)
    *
    * This used the CAVIAR_Real_FixedBorders DB and in general it reads from a DB where examples are stored
    * separately, each as a different entry.
    */

  def createPairsDB: Unit = {
    def mergeExmplesWithInnertia(e1:Example, e2:Example) = {
      val annotation = e1.annotation ++ e2.annotation
      val narrative = e1.narrative ++ e2.narrative
      val time = e1.time
      new Example(annot = annotation, nar = narrative, _time = time)
    }
    def mergeExamplesNoInertia(e1: Example, e2: Example) = {
      // see the comments in mergeExample function from ILEDNoiseTollerant to see what keepAtom does
      val keepAtom = (atom: String, annotation: List[String]) => {
        val fluent = Literal.toLiteral(atom).terms.head.tostring // the fluent is the first term
        annotation forall (x => !x.contains(fluent))
      }
      val time = e1.time
      val narrative = e1.narrative ++ e2.narrative
      val annotation = e1.annotation.filter(x => keepAtom(x,e2.annotation)) ++ e2.annotation
      new Example(annot = annotation, nar = narrative, _time = time)
    }
    def examplesToDBObject(inertia: Example, noInertia: Example, id: String) = {
      val first = MongoDBObject("time" -> inertia.time) ++ ("annotation" -> inertia.annotation) ++ ("narrative" -> inertia.narrative)
      val second = MongoDBObject("time" -> noInertia.time) ++ ("annotation" -> noInertia.annotation) ++ ("narrative" -> noInertia.narrative)
      MongoDBObject("exampleId" -> id.toInt, "time" -> inertia.time.toInt, "noInertia" -> second, "inertia" -> first)
    }

    val DB = new Database("CAVIAR_Real_FixedBorders", "examples")
    val dataIterator = DB.collection.find().sort(MongoDBObject("time" -> 1))
    val data = new ListBuffer[Example]
    while(dataIterator.hasNext) {
      val x = Example(dataIterator.next())
      data += x
    }
    val mongoClient = MongoClient()
    mongoClient.dropDatabase("CAVIAR_Real_FixedBorders_AsPairs") // clear in any case
    val collection = mongoClient("CAVIAR_Real_FixedBorders_AsPairs")("examples")
    data.toList.sliding(2).foldLeft(1){ (z,x) =>
      val withInertia = mergeExmplesWithInnertia(x.head, x.tail.head)
      val withoutInertia = mergeExamplesNoInertia(x.head, x.tail.head)
      val entry = examplesToDBObject(withInertia, withoutInertia, z.toString)
      collection.insert(entry)
      println(z)
      z + 1
    }
  }


  //val dataIterator = DB.collection.find().sort(MongoDBObject("exampleId" -> 1))
  def getDataAsChunks(DB: Database, chunkSize: Int, targetClass: String, withInertia: Boolean): Iterator[Exmpl] = {
    def mergeExmpl(in: List[Exmpl]) = {
      val time = in.head.time
      val id = in.head.id
      val merged = in.foldLeft(Example()){ (x,newExmpl) =>
        val accum = x
        val annotation =
          if (! withInertia && targetClass == "initiated") {
            // This is for kearning without inertia, splitting the initiatedAt
            // from the terminatedAt part.
            // We need to get examples without inertia, to make sure that in each
            // chunk, the first example is not covered by inertia (if positive).
            // In detail, the cases are as folows:
            // ----------------------------------------
            // CASE 1: Learning the initiatedAt part.
            // -----------------------------------------
            // CASE 1.1: The first example in the input list is a positive
            // with annotation of the form (holdsAt(F,T),holdsAt(F,T+1)), i.e.
            // holdsAt at T+1 are explained by inertia. We do not want that, so
            // we omit prior annotation (holdsAt(F,T)) by getting
            // newExmpl.exmplNoInertia.annotation
            // Case 1.2: The first example in the input list is a positive
            // with annotation of the form (not holdsAt(F,T),holdsAt(F,T+1)), i.e.
            // it'sinitiated at T. Then getting newExmpl.exmplNoInertia.annotation
            // gives the correct supervision
            // -------------------------------------
            // CASE 2: Learning the terminatedAt part
            // -------------------------------------
            // In this case we always want the prior annotation (terminated is
            // always learnt WITH inertia, therefore in this case we are at the else part below)
            // In general, for all other examples (other than the first one in the chunk)
            // getting the annotation with or without prior supervision does not matter,
            // since the examples are merged, so annotation "slides" from the one to the next
            // and is correct for the whole chunk.
            accum.annotation ++ newExmpl.exmplNoInertia.annotation.distinct
          } else {
            accum.annotation ++ newExmpl.exmplWithInertia.annotation.distinct
          }
        val narrative = accum.narrative ++ newExmpl.exmplWithInertia.narrative.distinct
        new Example(annot = annotation, nar = narrative, _time = time)
      }
      new Exmpl(_id = id, exampleWithInertia = merged)
    }

    val dataIterator = DB.collection.find().sort(MongoDBObject("time" -> 1))

    val accum = new ListBuffer[Exmpl]
    while (dataIterator.hasNext) {
      val newExample = dataIterator.next()
      val e = new Exmpl(newExample)
      accum += e
    }
    val chunked = accum.toList.sliding(chunkSize, chunkSize - 1)
    chunked map { x => mergeExmpl(x) }
  }


  /**
    * Find the positives and negative intervals in the database for the particular HLE
    *
    * @param HLE
    */

  /*

  Positive-negative intervals for moving

  nothing: (680,2560), length: 48
  moving: (2520,5800), length: 83
  nothing: (5760,24480), length: 469
  moving: (24440,27360), length: 74
  nothing: (27320,460680), length: 10835
  moving: (460640,464200), length: 90
  nothing: (464160,547440), length: 2083
  moving: (547400,559280), length: 298
  nothing: (559240,565840), length: 166
  moving: (565800,568120), length: 59
  nothing: (568080,786280), length: 5456
  moving: (786240,791880), length: 142
  nothing: (791840,797160), length: 134
  moving: (797120,800440), length: 84
  nothing: (800400,814920), length: 364
  moving: (814880,829480), length: 366
  nothing: (829440,843600), length: 355
  moving: (843560,849440), length: 148
  nothing: (849400,872040), length: 567
  moving: (872000,884560), length: 315
  nothing: (884520,885520), length: 26
  moving: (885480,892520), length: 177
  nothing: (892480,1077680), length: 4631

   */

  object Interval {
    def apply(HLE: String, start: Int) = {
      new Interval(HLE,start,0)
    }
  }

  case class Interval(val HLE: String, val startPoint: Int, var endPoint: Int) {
    val step = 40
    //var endPoint = 0
    def length = List.range(startPoint,endPoint+step, step).length
  }


  /**
    *
    * @param HLE the HLE we're currently learning
    * @return a tuple containing the list of positive and negative intervals for the HLE respectively
    */

  def getPositiveNegativeIntervals(DBName: String, HLE: String):  (List[Interval], List[Interval]) = {
    import scala.collection.mutable.Stack
    val step = 40
    var lastTime = 0
    val intervals = Stack[Interval]()
    val negativeIntervals = Stack[Interval]()
    var holdsPreviousely = false
    val closeInterval = (i: Stack[Interval], e: Example, what: String) => {
      val lastInterval = i.pop()
      // give some "air" around the positive intervals (hence +step)
      lastInterval.endPoint = if (what == "positive") e.time.toInt + step else e.time.toInt // This is wrong we should have "e.time.toInt-step" here
      i.push(lastInterval)
    }
    val startNewInterval = (i: Stack[Interval], e: Example, what: String) => {
      // give some "air" around the positive intervals (hence -step)
      if (what == "positive") i.push( Interval(HLE, e.time.toInt - step) ) else i.push( Interval("nothing", e.time.toInt) )
    }
    val closeLastInterval = (i: Stack[Interval]) => {
      val last = i.pop()
      if (last.endPoint == 0) last.endPoint = lastTime
      i.push(last)
    }

    val DB = new Database(DBName, "examples")
    val dataIterator = DB.collection.find().sort(MongoDBObject("time" -> 1))
    while(dataIterator.hasNext) {
      val x = dataIterator.next()
      val e = Example(x)
      val annotation = e.annotation

      if(annotation.exists(x => x.contains(HLE))) {
        if (! holdsPreviousely) {
          // close the last negative interval
          closeInterval(negativeIntervals, e, "negative")
          // start a new positive interval
          startNewInterval(intervals, e, "positive")
          holdsPreviousely = true
        }
      } else {
        if (holdsPreviousely) {
          // close the last positive interval
          closeInterval(intervals, e, "positive")
          // and start a new negative interval
          startNewInterval(negativeIntervals, e, "negative")
          holdsPreviousely = false
        } else {
          // start a new negative interval if on is not already being populated
          if (negativeIntervals.isEmpty) {
            startNewInterval(negativeIntervals, e, "negative")
          }
        }
      }
      lastTime = e.time.toInt // remember the last time to close the last interval in the stacks
    }
    closeLastInterval(negativeIntervals)
    closeLastInterval(intervals)
    intervals.toList.reverse foreach (x => println(s"${x.HLE}: (${x.startPoint},${x.endPoint}), length: ${x.length}")  )
    negativeIntervals.toList.reverse foreach (x => println(s"${x.HLE}: (${x.startPoint},${x.endPoint}), length: ${x.length}")  )
    // The average length of positive intervals
    val averagePositiveLenth = intervals.foldLeft(0.0)(_ + _.length) / intervals.length
    println(s"Average positive length: ${averagePositiveLenth.ceil}")
    // The total length of negative intervals
    val totalNegativeLength = negativeIntervals.foldLeft(0.0)(_ + _.length)
    println(s"Total negative length: $totalNegativeLength")
    // The total length of positive intervals
    val totalPositiveLength = intervals.foldLeft(0.0)(_ + _.length)
    println(s"Total positive length: $totalPositiveLength")
    // 90% of negatives will be used for training:
    val trainingNegativesNumber = ((90.0/100) * totalNegativeLength).toInt
    println(s"90% of negatives (training set size) is ${(90.0/100) * totalNegativeLength}")
    println(s"So negatives' testing set size is ${totalNegativeLength - trainingNegativesNumber}")
    (intervals.toList.reverse, negativeIntervals.toList.reverse)
  }

  /*
    If withChunking=true (default behaviour) then the intervals are chunked according to chunkSize.
    This is used in order to create the training set. To create the testing set however, intervals
    must not be chunked to ensure that inference with inertia works properly and tp/fp/fn are counted
    correctly. So for testing set generation withChunking=false.

    The withChunking parameter is simply passed to getDataFromInterval method that handles chunking or
    the lack thereof.
   */
  def getDataFromIntervals(DB: Database, HLE: String, i: List[Interval], chunkSize: Int, withChunking: Boolean = true): Iterator[Exmpl] = {
    val out  = i.foldLeft(Iterator[Exmpl]()){ (x,y) =>
      val z = getDataFromInterval(DB, HLE, y, chunkSize, withChunking)
      x ++ z
    }// simply merge iterators without producing them
    out
  }

  def getDataFromInterval(DB: Database, HLE: String, i: Interval, chunkSize: Int, withChunking: Boolean = true): Iterator[Exmpl] = {
    val startTime = i.startPoint
    val endTime = i.endPoint
    val batch = DB.collection.find("time" $gte startTime $lte endTime).sort(MongoDBObject("time" -> 1))
    val examples = batch.map(x => Example(x)) toList
    val HLExmpls = examples map { x =>
      val a = x.annotation filter (_.contains(HLE))
      new Example(annot = a, nar = x.narrative, _time = x.time)
    }

    //val slideStep = if (withChunking) chunkSize - 1 else HLExmpls.length // won't be chunk in this case
    //val chunked = HLExmpls.sliding(chunkSize, chunkSize - 1)
    val chunked = if (withChunking) HLExmpls.sliding(chunkSize, chunkSize-1) else HLExmpls.sliding(HLExmpls.length) // won't be chunked in the else case
    /*
     * We need no worry about removing prior annotation from the examples, since in any case inertia is not used during learning.
     * Even if a pair is passed where in both times there is positive annotation, the first positive example will be covered by
     * the initalTime axiom, while the second positive will be covered by abduction (no inertia).
     */

    /*
    val out =
      if (withChunking) {
        chunked map { x =>
          val merged = x.foldLeft(Example()) { (z, y) =>
            new Example(annot = z.annotation ++ y.annotation, nar = z.narrative ++ y.narrative, _time = x.head.time)
          }
          new Exmpl(_id = merged.time, exampleWithInertia = merged)
        }
      } else {
        HLExmpls map (x => new Exmpl(_id = x.time, exampleWithInertia = x))
      }
    */

    val out =

        chunked map { x =>
          val merged = x.foldLeft(Example()) { (z, y) =>
            new Example(annot = z.annotation ++ y.annotation, nar = z.narrative ++ y.narrative, _time = x.head.time)
          }
          new Exmpl(_id = merged.time, exampleWithInertia = merged)
        }

    out
  }


  /**
    * Uses the hand-crafted rules to perform inference on CAVIAR narrative and
    * produce noise-free annotation fot complex events. The noise-free data is
    * stored in a DB to use for experiment.
    *
    * "/home/nkatz/dev/ILED/datasets/Caviar/meetingHandCrafted.lp" path to meeting
    * "/home/nkatz/dev/ILED/datasets/Caviar/movingHandCrafted.lp"  path to moving
    *
    *
    * Call this method like that:
    *
    * iled.utils.CaviarUtils.generateCleanData("meeting","/home/nkatz/dev/ILED/datasets/Caviar/meetingHandCrafted.lp")
    *
    * @param HLE
    *
    *
    */

  def generateCleanData(HLE: String, handCraftedRulesPath: String): Unit = {
    val CaviarDB = "CAVIAR_Real_FixedBorders"
    val newDB = s"CAVIAR_${HLE}_CLEAN"

    val mongoClient = MongoClient()
    mongoClient.dropDatabase(newDB)
    val collection = mongoClient(newDB)("examples")

    val gl = new GlobalValues
    val (handCraftedRules,show) = HLE match {
      case "meeting" =>
        (handCraftedRulesPath,s"\n#show.\n#show holdsAt($HLE(X,Y),T):holdsAt($HLE(X,Y),T).\n")
      case "moving" =>
        (handCraftedRulesPath,s"\n#show.\n#show holdsAt($HLE(X,Y),T):holdsAt($HLE(X,Y),T).\n")
    }

    val file = Utils.getTempFile("generate",".lp")
    val jep = new Jep()


    val db = new Database(CaviarDB,"examples")
    db.collection.find().sort(MongoDBObject("time" -> 1)).foldLeft(List[String]()){ (priorAnnotation, newExmpl) =>
      val e = Example(newExmpl)

      if (e.time == "766600") {
        val stop = "stop"
      }

      val narrative = e.narrativeASP
      val in  = narrative ++ priorAnnotation.map(x => x+".") ++ List(s"time(${e.time.toInt+40}).")
      val content = in.mkString("\n") + gl.INCLUDE_BK(gl.BK_WHOLE_EC) + gl.INCLUDE_BK(handCraftedRules) + show
      Utils.writeLine(content,file.getCanonicalPath,"overwrite")
      val out = ASP.solve(task = Core.INFERENCE, aspInputFile = file, jep=jep)

      val prior =
        if (out.nonEmpty)  {
          out.head.atoms.map( x => (Literal.toLiteral(x).terms(1).tostring,x) ).filter(z => z._1 == e.time).map(_._2)
        } else {
          Nil
        }

      val next =
        if (out.nonEmpty)  {
          out.head.atoms.map( x => (Literal.toLiteral(x).terms(1).tostring,x) ).filter(z => z._1 == (e.time.toInt+40).toString).map(_._2)
        } else {
          Nil
        }

      val entry = MongoDBObject("time" -> e.time.toInt) ++ ("annotation" -> prior) ++ ("narrative" -> e.narrative)
      println(entry)
      collection.insert(entry)
      next
    }
    jep.close()
  }




  def copyCAVIAR = {
    val CaviarDB = new Database("CAVIAR_Real_FixedBorders")
    val idPattern = "id[0-9]+".r
    val originalIds = List("id0", "id4", "id5", "id1", "id2", "id3", "id6", "id7", "id8", "id9")
    val sort = (ids: List[String]) => ids.sortBy(z => z.split("id")(1).toInt)
    val getFirstLastIndex = (ids: List[String]) => {
      val s = sort(ids)//.last.split("id")(1).toInt
      val first = s.head.split("id")(1).toInt
      val last = s.last.split("id")(1).toInt
      (first,last)
    }
    def replaceAll = (s: String, map: Map[String,String]) => {
      val ids = idPattern.findAllIn(s)
      val toLit = if (!s.contains(".")) Literal.toLiteral(s) else Literal.toLiteral(s.split("\\.")(0))
      //ids.foldLeft(s){ (x,id) => x.replaceFirst(id,map(id)) }
      ids.foldLeft(toLit){ (x,id) => x.replace(Constant(id),Constant(map(id))) }.tostring
    }
    var lastIndex = getFirstLastIndex(originalIds)._2
    for (count <- 1 to 9) {
      lastIndex += 1
      val extraIds = (lastIndex to lastIndex+9).map(index => s"id$index").toList
      val map = (sort(originalIds) zip sort(extraIds)).toMap
      val indexes = getFirstLastIndex(extraIds)
      lastIndex = indexes._2

      val newDB = s"CAVIAR_id${indexes._1}_id${indexes._2}"
      val mongoClient = MongoClient()
      mongoClient.dropDatabase(newDB)
      val collection = mongoClient(newDB)("examples")

      CaviarDB.collection.find().sort(MongoDBObject("time" -> 1)).foldLeft(()) { (_,newExmpl) =>
        val e = Example(newExmpl)
        val narrative = e.narrativeASP map (x => replaceAll(x, map))
        val annotation = e.annotation map (x => replaceAll(x, map))
        val entry = MongoDBObject("time" -> e.time.toInt) ++ ("annotation" -> annotation) ++ ("narrative" -> narrative)
        println(entry)
        collection.insert(entry)
      }
    }
  }

  def mergeCaviarCopies = {
    val mongoClient = MongoClient()
    val allCopies = mongoClient.databaseNames.filter(x => x.contains("CAVIAR_id")).toList :+ "CAVIAR_Real_FixedBorders" //.sortBy { x => x.split("-")(1).toInt }
    val DBs = allCopies.map(name => new Database(name))
    // get all times to use them later as queries for merging
    val times = DBs.head.collection.find().sort(MongoDBObject("time" -> 1)).foldLeft(List[Int]()) { (t, newExmpl) =>
      t :+ Example(newExmpl).time.toInt
    }

    val newDB = s"CAVIAR-MERGED-COPIES"
    mongoClient.dropDatabase(newDB)
    val collection = mongoClient(newDB)("examples")

    times.foldLeft(List[Example]()) { (accum, time) =>
      val query = MongoDBObject("time" -> time)
      val e = Example.mergeExamples(DBs.map(db => db.collection.findOne(query).get).map(z => Example(z)))
      val entry = MongoDBObject("time" -> time) ++ ("annotation" -> e.annotation) ++ ("narrative" -> e.narrative)
      println(entry)
      collection.insert(entry)
      accum :+ e
    }


    mongoClient.close()

  }



}






class Exmpl(e: DBObject = DBObject(), _id: String = "", exampleWithInertia: Example = Example()) {
  val time = if (e != DBObject()) e.asInstanceOf[BasicDBObject].get("time").toString else exampleWithInertia.time
  val id = if (e != DBObject()) e.asInstanceOf[BasicDBObject].get("exampleId").toString else _id
  val exmplWithInertia = if (e != DBObject()) Example(e.asInstanceOf[BasicDBObject].get("inertia").asInstanceOf[BasicDBObject]) else exampleWithInertia
  val exmplNoInertia = if (e != DBObject()) Example(e.asInstanceOf[BasicDBObject].get("noInertia").asInstanceOf[BasicDBObject]) else Example()
}




/*
Example({ "_id" : { "$oid" : "56d58f08e4b0842fb35754c8"} , "time" : 1077520 , "annotation" : [ ] ,
 "narrative" : [ "orientation(id1,128,1077520)" , "happensAt(disappear(id1),1077520)" , "orientation(id3,164,1077520)" , "holdsAt(visible(id3),1077520)" ,
  "orientation(id4,166,1077520)" , "holdsAt(visible(id4),1077520)" , "happensAt(running(id1),1077520)" , "coords(id1,77,60,1077520)" , "happensAt(walking(id3),1077520)" ,
   "coords(id3,98,201,1077520)" , "happensAt(inactive(id4),1077520)" , "coords(id4,82,206,1077520)"]},,List(),List(),,false,false,List(),List())

 */


/**
  * Created by nkatz on 2/5/16.
  *
  *
  * This parses the CAVIAR dataset, see further notes below
  */

object ParseCAVIAR extends ClausalLogicParser{

  /**
    * The number of training interpretations for each video is
    * the number of distinct time points in that video times
    * the 2-combinations of distinct ids.
    *
    * @param path
    */
  def countInterpretations(path: String) = {
    val d = new File(path)
    val idPattern = "id[0-9]+".r
    val innerFolders = d.listFiles.sortBy(_.getName.split("-")(0).toInt)
    var totalSize = 0
    for (f <- innerFolders) {
      println(s"Video ${f.getCanonicalPath}")
      val files = f.listFiles.filter(x => movementOnly.exists(p => x.getName.contains(p)))
      val contents =
        (for (f <- files)
          yield scala.io.Source.fromFile(f).getLines().filter(p => !p.startsWith("%"))).toList.flatten.mkString.replaceAll("\\s","").split("\\.").toList
      val parsed = contents.flatMap(x => parseAll(caviarParser(0),x).getOrElse(List(""))).filter(_!="").asInstanceOf[List[Atom]]
      //println(parsed map (_.atoms))
      val allAtoms = parsed flatMap (_.atoms) map (x => Literal.toLiteral(x))
      val times = allAtoms.map(_.terms.reverse.head.tostring).distinct.length
      //println(times.length)
      val ids = parsed.flatMap(_.atoms).flatMap(z => idPattern.findAllIn(z).toList).distinct.length
      val size = if (ids > 1) (times * iled.utils.Utils.combinations(ids,2)).toInt else times
      println(s"current video size: $size")
      totalSize = totalSize + size
    }
    println(s"Total size: $totalSize")
  }

  // We'll maintain two versions of CAVIAR: The first will be the corrected one (where examples at "borderlines"
  // where a fluent changes its value, have been corrected -- pushed a frame forward for initiation, add extra
  // annotated frame for termination). This has happened everywhere (consulting the narrative at the same time),
  // except in cases of more than two persons participating in an HLE (e.g. happensAt( moving( grp_ID0, [ id0, id2, id3 ]), 13440).)
  // The corrected version of CAVIAR is under /dev/CAVIAR-abrupt-corrected-borderlines.
  // The second version is the original one, nothing has been tweaked. It is located under /dev/CAVIAR-abrupt-original

  def run(path: String) = {
    val mongoClient = MongoClient()
    mongoClient.dropDatabase("CAVIAR_Real_FixedBorders")
    val collection = mongoClient("CAVIAR_Real_FixedBorders")("examples")

    val d = new File(path)
    val innerFolders = d.listFiles.sortBy(_.getName.split("-")(0).toInt)
    var lastTime = 0
    for (f <- innerFolders) {
      println(s"Parsing video ${f.getCanonicalPath}")
      val files = f.listFiles.filter(x => dataFileNames.exists(p => x.getName.contains(p)))
      val contents =
        (for (f <- files)
          yield scala.io.Source.fromFile(f).getLines().filter(p => !p.startsWith("%"))).toList.flatten.mkString.replaceAll("\\s","").split("\\.").toList

      val parsed = contents.flatMap(x => parseAll(caviarParser(lastTime),x).getOrElse(List(""))).filter(_!="").asInstanceOf[List[Atom]]
      val atoms = SortedMap[Int, List[Atom]]() ++ parsed.groupBy(_.time.toInt)
      for ( (k,v) <- atoms ) {
        val narrative = v.filter(x => !x.annotationAtom).flatMap(z => z.atoms)
        val annotation = v.filter(x => x.annotationAtom).flatMap(z => z.atoms)
        val entry = MongoDBObject("time" -> k) ++ ("annotation" -> annotation) ++ ("narrative" -> narrative)
        collection.insert(entry)
        //println(s"inserted $entry")
      }
      lastTime = atoms.keySet.toList.reverse.head+40 // the last time point
    }
  }

  val hleMapping = Map("moving" -> "moving", "fighting" -> "fighting", "leaving_object" -> "leavingObject", "interacting" -> "meeting")
  val correctedCaviarPath = "/home/nkatz/dev/CAVIAR-abrupt-corrected-borderlines"
  val originalCaviarPath = "/home/nkatz/dev/CAVIAR-abrupt-original"
  val dataFileNames = List("AppearenceIndv","MovementIndv","SituationGrp")
  val movementOnly = List("MovementIndv")

  def word: Parser[String] = """[A-Za-z0-9_]*""".r ^^ { x => x }
  def person: Parser[Person] = "id" ~ number ^^ { case x ~ y => new Person(x + y) }
  def persons: Parser[List[Person]] = "[" ~> repsep(person, ",") <~ "]"
  def time: Parser[String] = number
  def orientationValue: Parser[String] = number
  def appearanceValue: Parser[String] = word
  def coordinates: Parser[(String, String)] = "(" ~ number ~ "," ~ number ~ ")" ^^ { case "(" ~ x ~ "," ~ y ~ ")" => (x, y) }
  def meeting: Parser[String] = "interacting"
  def moving: Parser[String] = "moving"
  def fighting: Parser[String] = "fighting"
  def leavingObject: Parser[String] = "leaving_object"
  def walking: Parser[String] = "walking"
  def active: Parser[String] = "active"
  def inactive: Parser[String] = "inactive"
  def running: Parser[String] = "running"
  def abrupt: Parser[String] = "abrupt"
  def happens: Parser[String] = "happensAt"
  def holds: Parser[String] = "holdsAt"
  def orientation: Parser[String] = "orientation"
  def appearance: Parser[String] = "appearance"
  def coords: Parser[String] = "coord"

  def annotationParser(pastTime: Int): Parser[AnnotationAtom] =
    happens ~ "(" ~ (meeting | moving | fighting | leavingObject) ~ "(" ~ word ~ "," ~ persons ~ ")" ~ "," ~ time ~ ")" ^^ {
      case _~"("~x~"("~_~","~y~")"~","~z~")" => new AnnotationAtom(x, y, (z.toInt+pastTime).toString)
    }
  def lleParser(pastTime: Int): Parser[NarrativeAtom] = happens ~ "(" ~ (walking | active | inactive | running | abrupt) ~ "(" ~ person ~ ")" ~ "," ~ time ~ ")" ^^ {
    case _~"("~lle~"("~p~")"~","~t~")" => new NarrativeAtom(what = lle, id = p.id, time = (t.toInt+pastTime).toString)
  }
  def orientationParser(pastTime: Int): Parser[NarrativeAtom] = holds ~ "(" ~ orientation ~ "(" ~ person ~ ")" ~ "=" ~ number ~ "," ~ time ~ ")" ^^ {
    case _~"("~_~"("~p~")"~"="~v~","~t~")" => new NarrativeAtom(what = "orientation", id = p.id, orientation = v, time = (t.toInt+pastTime).toString)
  }
  def appearanceParser(pastTime: Int): Parser[NarrativeAtom] = holds ~ "(" ~ appearance ~ "(" ~ person ~ ")" ~ "=" ~ word ~ "," ~ time ~ ")" ^^ {
    case _~"("~_~"("~p~")"~"="~v~","~t~")" => new NarrativeAtom(what = "appearance", id = p.id, appearance = v, time = (t.toInt+pastTime).toString)
  }
  def coordsParser(pastTime: Int): Parser[NarrativeAtom] = holds ~ "(" ~ coords ~ "(" ~ person ~ ")" ~ "=" ~ coordinates ~ "," ~ time ~ ")" ^^ {
    case _~"("~_~"("~p~")"~"="~c~","~t~")" => new NarrativeAtom(what = "coord", id = p.id, xcoord = c._1, ycoord = c._2, time = (t.toInt+pastTime).toString)
  }



  //def groupCoordParser: Parser[String] =

  def caviarAtomParser(pastTime: Int): Parser[Atom] = annotationParser(pastTime)|lleParser(pastTime)|orientationParser(pastTime)|appearanceParser(pastTime)|coordsParser(pastTime)
  def caviarParser(pastTime: Int): Parser[List[Atom]] = rep(caviarAtomParser(pastTime))
  class Person(val id: String)



  trait Atom {
    val annotationAtom: Boolean
    val atoms: List[String]
    val time: String
  }

  class AnnotationAtom(val HLE: String, val persons: List[Person], val time: String) extends Atom{
    val annotationAtom = true
    val atoms =
      if (HLE == "leaving_object") {
        List(s"holdsAt(${hleMapping(HLE)}(${persons.head.id},${persons(1).id}),$time)")
      } else {
        persons.toSet.subsets(2).flatMap(y => for (z <- y.toList.permutations) yield s"holdsAt(${hleMapping(HLE)}(${z.head.id},${z(1).id}),$time)").toList
      }
  }

  // what is either an LLE, or orientation, appearance, coord
  class NarrativeAtom(val what: String = "none", val id: String, val xcoord: String = "none",
                      val ycoord: String = "none", val orientation: String = "none",
                      val appearance: String = "none", val time: String) extends Atom{

    val annotationAtom = false
    val atoms = what match {
      case ("walking" | "active" | "inactive" | "running" | "abrupt") => List(s"happensAt($what($id),$time)")
      case "coord" => List(s"coords($id,$xcoord,$ycoord,$time)")
      case "appearance" => appearance match {
        case "appear" | "disappear" => List(s"happensAt($appearance($id),$time)")
        case _ => List(s"holdsAt($appearance($id),$time)")
      }
      case "orientation" => List(s"orientation($id,$orientation,$time)")
    }
  }

}


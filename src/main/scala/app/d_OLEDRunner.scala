package app

import java.util.UUID

import akka.actor.{ActorSystem, Props}
import oled.whole_caviar_data.MeetingTrainingDistributed
import oled_distributed.{Dispatcher, TopLevelActor, Utils}
import utils.DataUtils.{DataAsIntervals, Interval}
import utils.Exmpl


/**
  * Created by nkatz on 2/13/17.
  */

class InputParameters(val entryPath: String, val delta: Double, val postPruningThreshold: Double,
                      val minSeenExmpls: Int, val specializationDepth: Int,
                      val breakTiesThreshold: Double, val repeatFor: Int, val chunkSize: Int,
                      val processBatchBeforeMailBox: Int, val onlinePruning: Boolean,
                      val withPostPruning: Boolean, val withInertia: Boolean, val targetHLE: String,
                      val compressNewRules: Boolean, val minTpsRequired: Int)

object d_OLEDRunner extends App {

  Globals.glvalues("distributed") = "true"

  val WITH_JEP = false
  Globals.glvalues("with-jep") = WITH_JEP.toString

  val entryPath = "/home/nkatz/dev/iled/datasets/Caviar/meeting"
  //val entryPath = "/home/nkatz/dev/OLED/iled/datasets/Caviar/meeting"
  //val delta = 0.00001
  val delta = 0.000001
  val postPruningThreshold = 0.9
  val minSeenExmpls = 10000
  val specializationDepth = 1
  val breakTiesThreshold = 0.005
  val repeatFor = 10
  val chunkSize = 50  // learn from chunks instead of pairs (to speed things up)
  val processBatchBeforeMailBox = 1 // Each node processes a small batch before re-checking its mailbox for new messages
  val onlinePruning = true
  val withPostPruning = true
  val withInertia = false
  val HLE = "meeting"
  //val HLE = "moving"

  // This could be useful but its buggy.
  // It's true (intended) purpose id shown in the code snippet within the
  // 'case nc: NewClauses' case in Node.scala. But it needs some thinking
  // Currently, if this flag is on, I simply drop any new clause (upon its generation)
  // if a clause with the same bottom clause already exists in the current theory of the node.
  // The idea with this is as follows:
  // If we generate a new version of an existing rule (which is practically what we'll do
  // we allow to create new rules from already tried BC's), then we are (at best) trying
  // to cover an example which has already been rejected by specializing a previous version
  // of the rule that we're going to create. Does it worth it? should we simply abandon such
  // examples and see what we get?
  val compressNewRules = true

  val minTpsRequired = 0 // set to 0 for no restriction

  Globals.glvalues("specializationDepth") = specializationDepth.toString

  /*
  * There are two ways to run this. The first is by providing a list of separate database names,
  * where each database contains a fragment of the data, as in
  * val databases = List("db81","db82","db83","db84","db85","db86","db87","db88")
  *
  * The other way to run it is providing a list of DataAsIntervals objects and the name of a database
  * that holds the data. Each DataAsIntervals object consists of a training set and a testing set, in the
  * form of a list of (disjoint) time intervals, e.g.
  *
  * training_set = List( Interval(10, 100), Interval(200, 500), Interval(600, 1000) )
  * testing_set =  List( Interval(100, 150), Interval(2000, 3000) )
  *
  * The training/testing data will be fetched from the database at runtime, based on their time stamps
  * (using the respective time intervals). One processing node per element of the input List[DataAsIntervals]
  * will be generated at runtime, each handling a part of the dataset.
  *
  * */

  /*
   * Set this to List[String]() to run with intervals
   */
  //var databases = List("Big1", "Big2")
  //var databases = List("Test1", "Test2", "Test3", "Test4")
  //var databases = List("81","82","83","84","85","86","87","88")
  //var databases = List("CAVIAR_Real_FixedBorders") // test to try just a single db
  var databases = List[String]()

  // If true, each DataAsIntervals training set is stored into a separate db.
  val intervalsToDBs = false

  /*
  * Set this to List[DataAsIntervals]() to run with databases
  * */
  //val intervals = List[DataAsIntervals]()
  //val intervals = MeetingTrainingDistributed.TwoFoldSplit.meetTrainingSet7
  //val intervals = MeetingTrainingDistributed.FourFoldSplit.meetTrainingSet7
  val intervals = MeetingTrainingDistributed.EightFoldSplit.meetTrainingSet7
  // A master db from where data are supposed to be fetched is needed in case we're using intervals
  val fromDB = "CAVIAR_Real_FixedBorders"

  if (databases.isEmpty && intervals.isEmpty) throw new RuntimeException("No input Received.")
  if (databases.nonEmpty && intervals.nonEmpty) throw new RuntimeException("You can either pass a list of db names, or a list of DataAsIntervals objects.")

  val params = new InputParameters(entryPath, delta, postPruningThreshold, minSeenExmpls, specializationDepth,
    breakTiesThreshold, repeatFor, chunkSize, processBatchBeforeMailBox,
    onlinePruning, withPostPruning, withInertia, HLE, compressNewRules, minTpsRequired)

  val NodeActorNames =
    if (databases.nonEmpty) databases map (x => s"Node-${UUID.randomUUID}")
    else intervals map (x => s"Node-${UUID.randomUUID}")

  val dataFunction: (String, String, Int, DataAsIntervals) => Iterator[Exmpl] = Utils.getDataFromDB

  val message = "go"

  // Start the actor system
  val system = ActorSystem("distributed-oled")
  if (databases.nonEmpty) {
    system.actorOf(Props( new Dispatcher(databases=databases, getDataFunction=dataFunction, inputParams=params) ), name = "TopLevelDispatcher") ! message
  } else {
    if (intervalsToDBs) {
      val _params =
        // Chunksize = 1, it's already chunked
        new InputParameters(entryPath, delta, postPruningThreshold,
          minSeenExmpls, specializationDepth, breakTiesThreshold,
          repeatFor, 1, processBatchBeforeMailBox, onlinePruning,
          withPostPruning, withInertia, HLE, compressNewRules, minTpsRequired)

      val dbs = intervals.map(i => oled_distributed.Utils.intervalsToDB(fromDB, i, HLE, chunkSize))
      databases = dbs
      system.actorOf(Props( new Dispatcher(databases=databases, getDataFunction=dataFunction, inputParams=_params) ), name = "TopLevelDispatcher") ! message
    } else {
      system.actorOf(Props( new Dispatcher(intervals=intervals, masterDB=fromDB, getDataFunction=dataFunction, inputParams=params) ), name = "TopLevelDispatcher") ! message
    }
  }





}

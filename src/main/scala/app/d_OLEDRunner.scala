package app

import java.util.UUID

import akka.actor.{Props, ActorSystem}
import oled_distributed.{Dispatcher, Utils, TopLevelActor}
import utils.Exmpl


/**
  * Created by nkatz on 2/13/17.
  */

class InputParameters(val entryPath: String, val delta: Double, val postPruningThreshold: Double,
                      val minSeenExmpls: Int, val specializationDepth: Int,
                      val breakTiesThreshold: Double, val repeatFor: Int, val chunkSize: Int,
                      val processBatchBeforeMailBox: Int, val onlinePruning: Boolean,
                      val withPostPruning: Boolean, val withInertia: Boolean, val targetHLE: String)

object d_OLEDRunner extends App {

  Globals.glvalues("distributed") = "true"

  val WITH_JEP = false
  Globals.glvalues("with-jep") = WITH_JEP.toString

  val entryPath = "/home/nkatz/dev/ILED/datasets/Caviar/meeting"
  //val entryPath = "/home/nkatz/dev/OLED/iled/datasets/Caviar/meeting"
  val delta = 0.00001
  val postPruningThreshold = 0.6
  val minSeenExmpls = 1000
  val specializationDepth = 1
  val breakTiesThreshold = 0.05
  val repeatFor = 2
  val chunkSize = 10  // learn from chunks instead of pairs (to speed things up)
  val processBatchBeforeMailBox = 1 // Each node processes a small batch before re-checking its mailbox for new messages
  val onlinePruning = false
  val withPostPruning = true
  val withInertia = false
  val HLE = "meeting"
  //val HLE = "moving"

  Globals.glvalues("specializationDepth") = specializationDepth.toString

  //val databases = List("Big1", "Big2")
  //val databases = List("Test1", "Test2", "Test3", "Test4")
  val databases = List("81","82","83","84","85","86","87","88")

  val params = new InputParameters(entryPath, delta, postPruningThreshold, minSeenExmpls, specializationDepth,
    breakTiesThreshold, repeatFor, chunkSize, processBatchBeforeMailBox, onlinePruning, withPostPruning, withInertia, HLE)

  // start
  val system = ActorSystem("distributed-oled")

  //val globals = dbs map (db => new Globals(entryPath, db))

  val NodeActorNames = databases map (x => s"Node-${UUID.randomUUID}")

  val dataFunction: (String, String, Int) => Iterator[Exmpl] = Utils.getDataFromDB

  system.actorOf(Props( new Dispatcher(databases, dataFunction, params) ), name = "TopLevelDispatcher") ! "go"




}

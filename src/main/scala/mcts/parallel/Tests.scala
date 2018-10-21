package mcts.parallel

import akka.actor.{ActorSystem, Props}
import app.runners.MLNDataHandler.MLNDataOptions
import app.runutils.Globals
import mcts.HillClimbing.{constructBottomTheory, generateChildrenNodes, getData}
import mcts.RootNode


/**
  * Created by nkatz on 9/22/17.
  */

object Tests extends App {


  Globals.glvalues("perfect-fit") = "false"

  val foldPath = "/home/nkatz/dev/CAVIAR_MLN/CAVIAR_MLN/move/fold_2"

  val chunkSize = 50

  val opts = new MLNDataOptions(foldPath, chunkSize)

  val globals = new Globals("/home/nkatz/dev/iled/datasets/CaviarMLN", "")

  val bottomTheory = constructBottomTheory(getData(opts), globals)

  val rootNode = RootNode()

  val newTheories = generateChildrenNodes(rootNode.theory, bottomTheory, getData(opts), globals)

  val system = ActorSystem("ActorSystem")

  val scorer = system.actorOf(Props(new ScorerMaster(globals, opts, getData)))

  scorer ! newTheories

}

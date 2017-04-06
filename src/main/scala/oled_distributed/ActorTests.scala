package oled_distributed

import akka.actor.{Props, ActorSystem, Actor}
import com.mongodb.casbah.MongoClient
import logic.{Clause, Theory, Literal}

/**
  * Created by nkatz on 2/22/17.
  */


object TestRunner2 extends App {

  val mc = MongoClient()
  val collection = mc("Big1")("examples")
  collection.find().foreach(x => println(x))

}

object TestRunner1 extends App {

  val x = new Clause(head = Literal(functor = "p").asPosLiteral, body = Nil).tostring
  println(x)

}

object MessageTest extends App {

  val system = ActorSystem("ActorTestSystem")

  val actor = system.actorOf(Props( new ActorTest ), name = "x")

  actor ! "goAgain"
  actor ! "other"

  class ActorTest extends Actor {
    def receive= {
      case "goAgain" =>
        println("Starting new iteration")
        1 to 10000 foreach println
        self ! "goAgain"

      case "other" =>
        println("Now processing another message")
        Thread.sleep(5000)
    }
  }
}


object BecomeTest extends App {

  case class ClauseExpanded(expandedClause: String)
  case class ClauseNotExpanded(parentClause: String)
  case class ClauseExpansionRequest(clause: String)
  case class NewClauseArrived(newClause: String)
  case class ProcessNewBatch(newBatch: String)

  val system = ActorSystem("ActorTestSystem")
  val actor = system.actorOf(Props( new StatefulActor ), name = "x")

  actor ! "go"
  // send over a new clause
  actor ! NewClauseArrived("p :- q")
  // send a clause expansion request
  actor ! ClauseExpansionRequest("p :- q")
  // send another new clause. This should be received but not processed yet,
  // since our waiting state does not handle new NewClauseArrived messages
  actor ! NewClauseArrived("p :- q1")
  // send a ClauseExpanded reply. This should be processed and the actor should subsequently switch to normal state
  actor ! ClauseExpanded("p :- q")
  // send some more new arrivals...
  actor ! NewClauseArrived("p :- q2")
  actor ! NewClauseArrived("p :- q3")
  // now you should see the new arrivals in normal state



  class StatefulActor extends Actor {

    import context._

    def waitUntilExpandedState: Receive = {
      // This is received from another node
      case c: ClauseExpanded =>
        println("Received a ClauseExpanded msg while in waitUntilExpandedState")
        println(s"The clause ${c.expandedClause} was expanded, replacing parent clause with new in current theory")
        Thread.sleep(5000) // wait a bit to see if other types of message have been received
        become(normalState)
        self ! ProcessNewBatch("new")


      // This is received from another node
      case c: ClauseNotExpanded =>
        println("Received a ClauseNotExpanded msg while in waitUntilExpandedState")
        println("The clause was not expanded.")
        Thread.sleep(5000) // wait a bit to see if other types of message have been received
        become(normalState)
    }

    def normalState: Receive = {
      // This is received from another node
      case x: ClauseExpansionRequest =>
        println(s"Received a clause expansion request for ${x.clause}. Sending clause stats to sender and switching to waitUntilExpandedState")
        become(waitUntilExpandedState)

      // This is received from another node
      case x: NewClauseArrived =>
        println(s"Received a new clause ${x.newClause}, adding the clause to the current theory")

      case x: ProcessNewBatch => println("Processing a new batch")
    }

    def receive = {
      case "go" =>
        println("Starting")
        become(normalState) // start at normal state and listen to incoming messages
    }

  }

}



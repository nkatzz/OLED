package data_handling

import app.Globals
import com.mongodb.casbah.MongoClient
import jep.Jep
import logic.Examples.Example

/**
  * Created by nkatz on 5/25/17.
  */

object XDistsDraft extends App {

  val program =
    """
      |
      |#script (lua)
      |function axisdist(a,b)
      |   z = a - b
      |   res = math.abs(z)
      |   return res
      |end
      |#end.
      |
      |
      |person(X):-
 |    happensAt(walking(X),_).
 |person(X):-
 |    happensAt(running(X),_).
 |person(X):-
 |    happensAt(active(X),_).
 |person(X):-
 |    happensAt(inactive(X),_).
 |person(X):-
 |    happensAt(abrupt(X),_).
 |person(X):-
 |    happensAt(appear(X),_).
 |person(X):-
 |    happensAt(disappear(X),_).
 |person(X):-
 |    orientation(X,_,_).
 |person(X):-
 |    coords(X,_,_,_).
 |
 |

 |time(X):-
 |    happensAt(walking(_),X).
 |time(X):-
 |    happensAt(active(_),X).
 |time(X):-
 |    happensAt(inactive(_),X).
 |time(X):-
 |    happensAt(abrupt(_),X).
 |time(X):-
 |    happensAt(running(_),X).
 |time(X):-
 |    happensAt(appear(_),X).
 |time(X):-
 |    happensAt(disappear(_),X).
 |time(X):-
 |    orientation(_,_,X).
 |time(X):-
 |    coords(_,_,_,X).
 |

      |
      |timeWeCare(X,Y,T) :- not example(holdsAt(moving(X,Y),T)), person(X), person(Y), time(T), X != Y.
      |
      |xdistWeCare(X,Y,D,T) :-
      |    timeWeCare(X,Y,T),
      |    coords(X,X1,_,T),
      |    coords(Y,X2,_,T),
      |    D = @axisdist(X1,X2).
      |
      |
      |#show xdistWeCare/4.
      |
    """.stripMargin

  val dbName = "caviar"
  val mongoClient = MongoClient()
  val collection = mongoClient(dbName)("examples")

  val jep = new Jep()

  collection.find().foreach{ x =>
    val e = Example(x)
    //if (e.annotation.nonEmpty) println(e.time)
    val all = (e.annotation.map(x => s"example($x).") ++ e.narrative.map(x => s"$x.")).mkString("\n")


    if (e.annotation.exists(x => x.contains("moving"))){
      val t = "stop"
    }

    val f = utils.Utils.getTempFile("isConsistent",".lp",deleteOnExit = true)
    val content =  all + "\n" + program
    utils.Utils.writeToFile(f, "append")(p => List(content) foreach p.println)
    val path = f.getCanonicalPath

    val answerSet = utils.ASP.solve(task = Globals.SCORE_RULES, aspInputFile = new java.io.File(path), jep=jep)
    if (answerSet.nonEmpty) println(answerSet)
  }


  /*
   val all = e + include + exmplCountRules + markedProgram + show
    val f = Utils.getTempFile("isConsistent",".lp",deleteOnExit = true)
    Utils.writeToFile(f, "append")(p => List(all) foreach p.println)
    val path = f.getCanonicalPath
    val answerSet = ASP.solve(task = Globals.SCORE_RULES, aspInputFile = new File(path), jep=jep)
   */


//s"\n\n#include " + "\""+file+"\".\n"

}

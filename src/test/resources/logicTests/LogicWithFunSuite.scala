package logicTests


import org.scalatest._
import iled.structures._
import iled.structures.Modes._
import iled.utils.Utils
import iled.utils.Hausdorff 

class LogicWithFunSuite extends FunSuite {

   
   /*
   test("Variabilization of a mode declaration should replace all placemarkers with fresh variables") {
      val modehtest1 = "modeh(initiatedAt(moving(+person,+person),+time))"
      val modebtest1 = "modeb(happensAt(walking(+person),+time))"
      val x = "modeh(test(p(+x,+y,q(-x,#z,r(+o,-o)),w(q(+a,-b))),+time))"
      val y = "modeh(test(p(+x,+y,q(-x,#z,r(+o,-o)),w(q(+a,-b))),+time))"
      val t = new ModesParserTester()
      assert(t.testModes(modehtest1, t.mhParser, t.varbedStr) == "initiatedAt(moving(X0,X1),X2)")
   }

   test("Invoking head on an empty Set should produce NoSuchElementException") {
      intercept[NoSuchElementException] {
         Set.empty.head
      }
   }

   
   test("Equality of terms"){
      assert( Variable("X") == Variable("X") )
      assert( Variable("X") != Variable("Y") )
      assert(Literal("test",List(Constant("a"),Variable("Y"))) == Literal("test",List(Constant("a"),Variable("Y"))))
      assert(Literal("test",List(Constant("a"),Variable("Y"))) != Literal("test",List(Constant("b"),Variable("Y"))))
   }
   
   test("Skolemization of a clause") {
      val clause = "p(X,Y) :- q(Z,r(1,2,T),T),r(A,B,C,2,3,a,e,A,B)"
      val toClause = Utils.objectFactory.getClause(clause)
      val (skol,map) = (toClause.skolemise._1.tostring,toClause.skolemise._2) 
      println(skol)
      println(map)
   }
   
   test("Get the variables from a literal"){
      val lit = Utils.objectFactory.getLiteral("not p(X,Y,1,q(X,Z,Y,a,T),R,R,T)")
      println(lit.getVars)
   }
   
   test("Get the variables from a clause"){
      val clause = "p(X,Y) :- q(Z,r(1,2,T),T),r(A,B,C,2,3,a,e,A,B)"
      val toClause = Utils.objectFactory.getClause(clause)
      println(toClause.getVars)
   }
   */
   
   test("theta-subsumption"){
      // Same clauses (true)
      val pair1 = ("p(X,Y) :- q(Z,r(1,2,T),T),r(A,B,C,2,3,a,e,A,B)","p(X,Y) :- q(Z,r(1,2,T),T),r(A,B,C,2,3,a,e,A,B)")
      // subsumer is shorter (true)
      val pair2 = ("p(X,Y) :- r(A,B,C,2,3,a,e,A,B)","p(X,Y) :- q(Z,r(1,2,T),T),r(A,B,C,2,3,a,e,A,B)")
      // clauses differ in constants (false)
      val pair3 = ("p(X,Y) :- q(Z,r(1,2,T),T),r(A,B,C,1,3,a,e,A,B)","p(X,Y) :- q(Z,r(1,2,T),T),r(A,B,C,2,3,a,e,A,B)")
      // Longer subsumes shorter (true)
      val pair4 = ("q(X):-p(X,Y),p(Y,X)","q(A):-p(A,A)")
      // subsumer is superset (false)
      val pair5 = ("p(X,Y) :- q(Z,r(1,2,T),T),r(A,B,C,2,3,a,e,A,B)","p(X,Y) :- r(A,B,C,2,3,a,e,A,B)")
      // clauses are irrelevant (false)
      val pair6 = ("terminatedAt(moving(X0,X1),X2) :- happensAt(walking(X0),X2),happensAt(walking(X1),X2),distLessThan(X0,X1,40,X2),distLessThan(X0,X1,34,X2),distLessThan(X0,X1,27,X2),distLessThan(X0,X1,24,X2),distLessThan(X0,X1,25,X2),distLessThan(X1,X0,40,X2),distLessThan(X1,X0,34,X2),distLessThan(X1,X0,27,X2),distLessThan(X1,X0,24,X2),distLessThan(X1,X0,25,X2),orientFar(X0,X1,45,X2),orientFar(X1,X0,45,X2),holdsAt(visible(X0),X2),holdsAt(visible(X1),X2)",
                   "initiatedAt(meeting(X0,X1),X2) :- happensAt(walking(X1),X2),happensAt(walking(X0),X2),distLessThan(X1,X0,40,X2),distLessThan(X1,X0,34,X2),distLessThan(X1,X0,27,X2),distLessThan(X1,X0,24,X2),distLessThan(X1,X0,25,X2),distLessThan(X0,X1,40,X2),distLessThan(X0,X1,34,X2),distLessThan(X0,X1,27,X2),distLessThan(X0,X1,24,X2),distLessThan(X0,X1,25,X2),orientFar(X1,X0,45,X2),orientFar(X0,X1,45,X2),holdsAt(visible(X1),X2),holdsAt(visible(X0),X2)")
      // first clause misses some body atoms (true)
      val pair7 = ("terminatedAt(moving(X0,X1),X2) :- happensAt(walking(X0),X2),happensAt(walking(X1),X2),distLessThan(X0,X1,34,X2),distLessThan(X0,X1,27,X2),distLessThan(X0,X1,24,X2),distLessThan(X0,X1,25,X2),distLessThan(X1,X0,40,X2),distLessThan(X1,X0,27,X2),distLessThan(X1,X0,24,X2),orientFar(X0,X1,45,X2),orientFar(X1,X0,45,X2),holdsAt(visible(X0),X2),holdsAt(visible(X1),X2)",
                    "terminatedAt(moving(X0,X1),X2) :- happensAt(walking(X0),X2),happensAt(walking(X1),X2),distLessThan(X0,X1,40,X2),distLessThan(X0,X1,34,X2),distLessThan(X0,X1,27,X2),distLessThan(X0,X1,24,X2),distLessThan(X0,X1,25,X2),distLessThan(X1,X0,40,X2),distLessThan(X1,X0,34,X2),distLessThan(X1,X0,27,X2),distLessThan(X1,X0,24,X2),distLessThan(X1,X0,25,X2),orientFar(X0,X1,45,X2),orientFar(X1,X0,45,X2),holdsAt(visible(X0),X2),holdsAt(visible(X1),X2)")
      // variable renaming (X0 -> A in the 2nd clause) (true)
      val pair8 = ("initiatedAt(meeting(X0,X1),X2) :- happensAt(walking(X1),X2),happensAt(walking(X0),X2),distLessThan(X1,X0,40,X2),distLessThan(X1,X0,34,X2),distLessThan(X1,X0,27,X2),distLessThan(X1,X0,24,X2),distLessThan(X1,X0,25,X2),distLessThan(X0,X1,40,X2),distLessThan(X0,X1,34,X2),distLessThan(X0,X1,27,X2),distLessThan(X0,X1,24,X2),distLessThan(X0,X1,25,X2),orientFar(X1,X0,45,X2),orientFar(X0,X1,45,X2),holdsAt(visible(X1),X2),holdsAt(visible(X0),X2)",
                   "initiatedAt(meeting(A,X1),X2) :- happensAt(walking(X1),X2),happensAt(walking(A),X2),distLessThan(X1,A,40,X2),distLessThan(X1,A,34,X2),distLessThan(X1,A,27,X2),distLessThan(X1,A,24,X2),distLessThan(X1,A,25,X2),distLessThan(A,X1,40,X2),distLessThan(A,X1,34,X2),distLessThan(A,X1,27,X2),distLessThan(A,X1,24,X2),distLessThan(A,X1,25,X2),orientFar(X1,A,45,X2),orientFar(A,X1,45,X2),holdsAt(visible(X1),X2),holdsAt(visible(A),X2)")              
                    
      val pair9 = ("p(X,Y,Z,W,S,R,T) :- a(1,2,4,X,S),b(1),q(X,Y),q(X,Y),q(X,Y),q(X,Y),q(X,Y),q(X,Y),q(X,Y),q(X,Y),q(X,Y),q(X,Y),q(X,Y),q(X,Y),q(X,Y),q(X,Y),q(X,Y),q(X,Y),q(X,Y),q(X,Y),q(X,Y),q(X,Y),q(X,Y),q(X,Y),q(X,Y),q(X,Y),q(X,Y),q(X,Y),q(X,Y),q(X,Y),q(X,Y),q(X,Y),q(X,Y),q(X,Y),q(X,Y),q(X,Y),q(X,Y),q(X,Y)",
                   "p(X,Y,Z,W,S,R,T) :- a(1,2,4,X,S),b(1),q(X,Y),q(X,Y),q(X,Y),q(X,Y),q(X,Y),q(X,Y),q(X,Y),q(X,Y),q(X,Y),q(X,Y),q(X,Y),q(X,Y),q(X,Y),q(X,Y),q(X,Y),q(X,Y),q(X,Y),q(X,Y),q(X,Y),q(X,Y),q(X,Y),q(X,Y),q(X,Y),q(X,Y),q(X,Y),q(X,Y),q(X,Y),q(X,Y),q(X,Y),q(X,Y),q(X,Y),q(X,Y),q(X,Y),q(X,Y),q(X,Y),q(X,Y)")             
                   
                  
      assert(subs(pairToClauses(pair1)))
      assert(subs(pairToClauses(pair2)))
      assert(!subs(pairToClauses(pair3)))
      assert(subs(pairToClauses(pair4)))
      assert(!subs(pairToClauses(pair5)))
      assert(!subs(pairToClauses(pair6)))
      assert(subs(pairToClauses(pair7)))
      assert(subs(pairToClauses(pair8)))
      
      //println(subs(pairToClauses(pair9)))             
                   
      def subs(x: Tuple2[Clause,Clause]) = {
         x._1.thetaSubsumes(x._2)
      }             
      
      def pairToClauses(clauses: Tuple2[String,String]) = {
         (Clause.toClause(clauses._1),Clause.toClause(clauses._2))
      }
      
   }
   
   test("Get terms that correspond to variables") {
      val lit1 = Literal.toLiteral("happensAt(walking(id1),100)", "")
      val lit2 = Literal.toLiteral("initiatedAt(fighting(di1,id2),1000)", "")
      val lit3 = Literal.toLiteral("distLessThan(id1,id2,45,1000)", "")
      println(lit1.getTermsThatCorrespondToVars)
      println(lit2.getTermsThatCorrespondToVars)
      println(lit3.getTermsThatCorrespondToVars)
   }
   
   
   /*
   test("Replacing terms in literal") {
      val lit = Utils.objectFactory.getLiteral("test(a,X,p(t,Zes),1,2,q(a,R,T,X))", "")
      val replit1 = lit.replace(Constant("a"),Constant("nikos"))
      println(replit1.tostring)
      val replit2 = lit.replace(Variable("X"),Constant("skolem1"))
      println(replit2.tostring)
      val replit3 = lit.replace(Literal("p",List(Constant("t"),Variable("Zes"))),Constant("skolem2"))
      println(replit3.tostring)
      val replit4 = lit.replaceAll(Map(Variable("X")->Constant("skolem1"),Constant("a")->Constant("skolem2")))
      println(replit4.tostring)
   }
   */
   
   test("Hausdorff Distances") {
      val ex = List("holdsAt(visible(c),c)", "holdsAt(visible(c),c)", "holdsAt(visible(c),c)", "happensAt(walking(c),c)", "happensAt(walking(c),c)", "happensAt(walking(c),c)", "distLessThan(c,c,40,c)", "distLessThan(c,c,34,c)", "distLessThan(c,c,27,c)", "distLessThan(c,c,24,c)", "distLessThan(c,c,25,c)", "distLessThan(c,c,40,c)", "distLessThan(c,c,34,c)", "distLessThan(c,c,27,c)", "distLessThan(c,c,24,c)", "distLessThan(c,c,25,c)", "distMoreThan(c,c,40,c)", "distMoreThan(c,c,34,c)", "distMoreThan(c,c,27,c)", "distMoreThan(c,c,24,c)", "distMoreThan(c,c,25,c)", "distMoreThan(c,c,40,c)", "distMoreThan(c,c,34,c)", "distMoreThan(c,c,27,c)", "distMoreThan(c,c,24,c)", "distMoreThan(c,c,25,c)", "distMoreThan(c,c,40,c)", "distMoreThan(c,c,34,c)", "distMoreThan(c,c,27,c)", "distMoreThan(c,c,24,c)", "distMoreThan(c,c,25,c)", "distMoreThan(c,c,40,c)", "distMoreThan(c,c,34,c)", "distMoreThan(c,c,27,c)", "distMoreThan(c,c,24,c)", "distMoreThan(c,c,25,c)", "orientClose(c,c,45,c)", "orientClose(c,c,45,c)", "orientFar(c,c,45,c)", "orientFar(c,c,45,c)", "orientFar(c,c,45,c)", "orientFar(c,c,45,c)")
      val neg = List("holdsAt(visible(c),c)", "holdsAt(visible(c),c)", "holdsAt(visible(c),c)", "happensAt(walking(c),c)", "happensAt(walking(c),c)", "happensAt(walking(c),c)", "distLessThan(c,c,40,c)", "distLessThan(c,c,34,c)", "distLessThan(c,c,27,c)", "distLessThan(c,c,24,c)", "distLessThan(c,c,25,c)", "distLessThan(c,c,40,c)", "distLessThan(c,c,34,c)", "distLessThan(c,c,27,c)", "distLessThan(c,c,24,c)", "distLessThan(c,c,25,c)", "distMoreThan(c,c,40,c)", "distMoreThan(c,c,34,c)", "distMoreThan(c,c,27,c)", "distMoreThan(c,c,24,c)", "distMoreThan(c,c,25,c)", "distMoreThan(c,c,40,c)", "distMoreThan(c,c,34,c)", "distMoreThan(c,c,27,c)", "distMoreThan(c,c,24,c)", "distMoreThan(c,c,25,c)", "distMoreThan(c,c,40,c)", "distMoreThan(c,c,34,c)", "distMoreThan(c,c,27,c)", "distMoreThan(c,c,24,c)", "distMoreThan(c,c,25,c)", "distMoreThan(c,c,40,c)", "distMoreThan(c,c,34,c)", "distMoreThan(c,c,27,c)", "distMoreThan(c,c,24,c)", "distMoreThan(c,c,25,c)", "orientClose(c,c,45,c)", "orientClose(c,c,45,c)", "orientFar(c,c,45,c)", "orientFar(c,c,45,c)", "orientFar(c,c,45,c)", "orientFar(c,c,45,c)")
      val pos = List("holdsAt(visible(c),c)", "holdsAt(visible(c),c)", "holdsAt(visible(c),c)", "happensAt(walking(c),c)", "happensAt(walking(c),c)", "happensAt(walking(c),c)", "distLessThan(c,c,40,c)", "distLessThan(c,c,34,c)", "distLessThan(c,c,27,c)", "distLessThan(c,c,24,c)", "distLessThan(c,c,25,c)", "distLessThan(c,c,40,c)", "distLessThan(c,c,34,c)", "distLessThan(c,c,27,c)", "distLessThan(c,c,24,c)", "distLessThan(c,c,25,c)", "distMoreThan(c,c,40,c)", "distMoreThan(c,c,34,c)", "distMoreThan(c,c,27,c)", "distMoreThan(c,c,24,c)", "distMoreThan(c,c,25,c)", "distMoreThan(c,c,40,c)", "distMoreThan(c,c,34,c)", "distMoreThan(c,c,27,c)", "distMoreThan(c,c,24,c)", "distMoreThan(c,c,25,c)", "distMoreThan(c,c,40,c)", "distMoreThan(c,c,34,c)", "distMoreThan(c,c,27,c)", "distMoreThan(c,c,24,c)", "distMoreThan(c,c,25,c)", "distMoreThan(c,c,40,c)", "distMoreThan(c,c,34,c)", "distMoreThan(c,c,27,c)", "distMoreThan(c,c,24,c)", "distMoreThan(c,c,25,c)", "orientClose(c,c,45,c)", "orientClose(c,c,45,c)", "orientFar(c,c,45,c)", "orientFar(c,c,45,c)", "orientFar(c,c,45,c)", "orientFar(c,c,45,c)")
      println("Hausdorff dist of ex form neg is "+ Hausdorff.exmplHausdrfDist(ex,neg))
      println("Hausdorff dist of ex form pos is "+ Hausdorff.exmplHausdrfDist(ex,pos))
   }
   
   test("XHAIL reading from DB") {
      iled.core.Xhail.runXhail(fromDB = "CAVIAR-01-Walk1", inputDirectory = "")
   }
   
}



class ModesParserTester extends iled.parsers.ModesParser {

      val mhParser = modeh
      val mbParser = modeb
      val tostr = (x: ModeAtom) => x.tostring
      val varbed = (x: ModeAtom) => x.varbed
      val varbedStr = (x: ModeAtom) => x.varbed.tostring
      val varr = (x: ModeAtom) => x.varbed

      val x = getParseResult(parseModes(mhParser, "modeh(initiatedAt(moving(+person,+person),+time))"))

      //println(x.varbed().tostring)
      //println(x.varbed().typePreds)

      val y = getParseResult(parseModes(mhParser, "modeh(test(p(+x,+y,q(-x,#z,r(+o,-o)),w(q(+a,-b))),+time))"))
      //val y = getParseResult(parseModes(mhParser, "modeh(test(p(+x,+y,q(-x,#z,r(+o,-o)))))"))
      //println(y.tostring)
      //println(y)
      //println(y.varbed().tostring)
      //println(y.varbed().typePreds)      
      /*
       * To use this, simply write similar functions and local values for the parsers you need and pass them.
       * 
       */

      def testModes(s: String, parser: Parser[ModeAtom], f: ModeAtom => Any): Any = {
         val x = getParseResult(parseModes(parser, s))
         f(x)
      }

   }
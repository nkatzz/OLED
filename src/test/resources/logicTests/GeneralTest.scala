package logicTests

import com.mongodb.casbah.MongoClient
import iled.parsers.ModesParser
import iled.structures.Examples.Example
import iled.structures.Modes.ModeAtom
import iled.structures._


object GeneralTest extends ModesParser {

   def main(args: Array[String]) {




   }


   def generateQueries(abduced: Literal, interms: List[Expression]): List[(List[String], ModeAtom)] = {




      List((List[String](),ModeAtom("",List())))
   }

   def testUseSplits {
      val c1 = " initiatedAt(meeting(X0,X1),X2) :- happensAt(walking(X1),X2),happensAt(walking(X0),X2)"
      val c1Clause = getParseResult(parse(clause, c1)).asInstanceOf[Clause]
      val ss11 = " initiatedAt(meeting(X0,X1),X2) :- happensAt(running(X3),X2),happensAt(abrupt(X5),X2),distLessThan(X0,X1,34,X2),orientFar(X1,X0,45,X2)"
      val ss11Clause = getParseResult(parse(clause, ss11)).asInstanceOf[Clause]
      val ss12 = " initiatedAt(meeting(X0,X1),X2) :- flunc(a,R),flanc(Z,0),a(1,2,3),t(a,e(1,2,3),Z)"
      val ss12Clause = getParseResult(parse(clause, ss12)).asInstanceOf[Clause]
      c1Clause.addToSupport(List(ss11Clause, ss12Clause))
      val c2 = "initiatedAt(fighting(X0,X1),X2) :- happensAt(abrupt(X1),X2),happensAt(abrupt(X0),X2)"
      val c2Clause = getParseResult(parse(clause, c2)).asInstanceOf[Clause]
      val ss21 = "initiatedAt(fighting(X0,X1),X2) :- test(1,2,3,4), test2(X1,X2,a(4,5,6))"
      val ss21Clause = getParseResult(parse(clause, ss21)).asInstanceOf[Clause]
      val ss22 = "initiatedAt(fighting(X0,X1),X2) :- s(1,2,3,4), q(X8,X5,a(4,5,6)), p(happensAt(walking(X12),X23))"
      val ss22Clause = getParseResult(parse(clause, ss22)).asInstanceOf[Clause]
      c2Clause.addToSupport(List(ss21Clause, ss22Clause))
      val theory = Theory(List(c1Clause, c2Clause))
      val (a, b) = theory.use_2_split
      a.clauses.foreach { x => println(x.tostring) }
      b.foreach(x => println(x._1 + " -> " + x._2.tostring))
      val (a1, b1, j) = theory.use_3_spilt_one
      a1.clauses.foreach { x => println(x.tostring) }
      b1.foreach(x => println(x._1 + " -> " + x._2.tostring))
      println(j)
   }

   
   def testTheorySplits {
      
   }
   
   
}
package logicTests


import org.scalatest._
import iled.structures._

class Logic extends FlatSpec with Matchers {

   "Variabilization of a mode declaration" should "replace all placemarkers with fresh variables" in {

      val modehtest1 = "modeh(initiatedAt(moving(+person,+person),+time))"
      val modebtest1 = "modeb(happensAt(walking(+person),+time))"
      val x = "modeh(test(p(+x,+y,q(-x,#z,r(+o,-o)),w(q(+a,-b))),+time))"
      val y = "modeh(test(p(+x,+y,q(-x,#z,r(+o,-o)),w(q(+a,-b))),+time))"
      val t = new ModesParserTester()

      t.testModes(modehtest1, t.mhParser, t.tostr) should be("initiatedAt(moving(+person,+person),+time)")
      //t.testModes(modebtest1, t.mbParser, t.tostr)

      t.testModes(modehtest1, t.mhParser, t.varbedStr) should be("initiatedAt(moving(X0,X1),X2)")

      //t.testModes(modehtest1, t.mhParser, t.varbed.typePreds) should be ("initiatedAt(moving(X0,X1),X2)")
      //t.testModes(modehtest1, t.mhParser, t.varr)

   }

   

}
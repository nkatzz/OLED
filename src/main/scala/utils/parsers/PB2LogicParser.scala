package utils.parsers

import com.typesafe.scalalogging.LazyLogging
import logic.{Constant, Expression, Literal, Variable}
import org.parboiled2._
import scala.util.{Failure, Success, Try}

object PB2LogicParser extends LazyLogging {

  /*
  *
  * TODO
  *
  * Fix whitespace!!
  * Currently parsing fails even with the slightest
  * whitespace in the logical expressions.
  *
  * */

  def parseClause(c: String, debug: Boolean = false): Expression = {
    val parser = new PB2LogicParser(c)
    val result = parser.Clause.run()
    getParserResult(result, parser, debug)
  }

  def parseAtom(a: String, debug: Boolean = false): Expression = {
    val parser = new PB2LogicParser(a)
    val result = parser.Atom.run()
    getParserResult(result, parser, debug)
  }

  private def getParserResult(result: Try[Expression], parser: PB2LogicParser, debug: Boolean = false) = {
    val out = result match {
      case Success(x) =>
        if (debug) {
          logger.info("\n"+x.tostring)
          Some(x)
        } else Some(x)
      case Failure(e: ParseError) => logger.error(parser.formatError(e)) ; None
      case Failure(e: Throwable) => throw e
    }
    out match {
      case Some(x) => x
      case _ => throw new RuntimeException
    }
  }



}

final class PB2LogicParser(val input: ParserInput) extends Parser {

  case class ExpressionList(elems: List[Expression])

  def Clause = rule {
    Atom ~ " :- " ~ BodyLiterals ~ optional(".") ~ EOI ~> ( (x, y) =>
      logic.Clause(head = x, body = y.elems.map(_.asInstanceOf[Literal])) )
  }

  def Atom = rule {
    Funct ~ InnerTerms ~ optional(".") ~> ((x, y) => Literal(functor = x, terms = y.elems)) |
      "not " ~ Funct ~ InnerTerms ~ optional(".") ~> ((x, y) => Literal(functor = x, terms = y.elems, isNAF = true))
  }

  private def Term: Rule1[Expression] = rule { Atom | Const | Var }

  private def BodyLiterals = rule { oneOrMore(Atom).separatedBy(",") ~> ( x => ExpressionList(x.toList)) }

  private def InnerTerms = rule { "(" ~ oneOrMore(Term).separatedBy(",") ~ ")" ~> ( x => ExpressionList(x.toList)) }

  private def Funct = rule { capture(LowerCaseString) ~> ((x: String) => x) }

  private def Var = rule { capture(UpperCaseString) ~> ((x: String) => Variable(x)) }

  private def Const = rule {
    (capture(LowerCaseString) ~> ((x: String) => Constant(x))) |
      (capture(Integer) ~> (x => Constant(x))) |
      (capture(optional('"') ~ LowerCaseString ~ optional('"')) ~> ((x: String) => Constant(x))) |
      (capture('"' ~ UpperCaseString ~ '"') ~> ((x: String) => Constant(x)))
  }

  private def LowerCaseString = rule { CharPredicate.LowerAlpha ~ zeroOrMore(CharPredicate.AlphaNum | "_") }
  private def Integer = rule { oneOrMore(CharPredicate.Digit) }
  private def UpperCaseString = rule { CharPredicate.UpperAlpha ~ zeroOrMore(CharPredicate.AlphaNum | "_") }

}

object TestRunner extends App {
  PB2LogicParser.parseAtom("initiatedAt(meeting(X0,X1,45),1,Z,Petryb,a(p(2,3,z(23,g,f,ert(sdjfskj,Xkjsh))),1),oo(12,23,E))", debug=true)
  // with a final "."
  PB2LogicParser.parseAtom("initiatedAt(meeting(X0,X1,45),1,Z,Petryb,a(p(2,3,z(23,g,f,ert(sdjfskj,Xkjsh))),1),oo(12,23,E)).", debug=true)
  // The Atom parser succeeds in the first match, e.g here it parses the (garbage) expression since it matches the head.
  // I'll have to do something with EOI to disallow that
  PB2LogicParser.parseAtom("initiatedAt(meeting(X0,X1,45),1,Z,Petryb,a(p(2,3,z(23,g,f,ert(sdjfskj,Xkjsh))),1),oo(12,23,E)). :- sdfsdfsfsdfsdf", debug=true)
  // negation
  PB2LogicParser.parseAtom("not initiatedAt(meeting(X0,X1,45),1,Z,Petryb,a(p(2,3,z(23,g,f,ert(sdjfskj,Xkjsh))),1),oo(12,23,E))", debug=true)
  // negation with final "."
  PB2LogicParser.parseAtom("not initiatedAt(meeting(X0,X1,45),1,Z,Petryb,a(p(2,3,z(23,g,f,ert(sdjfskj,Xkjsh))),1),oo(12,23,E)).", debug=true)
  // clause
  PB2LogicParser.parseClause("initiatedAt(meeting(X0,X1,45),1,Z,Petryb,a(p(2,3,z(23,g,f,ert(sdjfskj,Xkjsh))),1),oo(12,23,E)) :- happensAt(walking(X,34,p(a(s,2),Z)),T,Or),close(1,2,3,4,Yt)", debug=true)
  // clause with final "."
  PB2LogicParser.parseClause("initiatedAt(meeting(X0,X1,45),1,Z,Petryb,a(p(2,3,z(23,g,f,ert(sdjfskj,Xkjsh))),1),oo(12,23,E)) :- happensAt(walking(X,34,p(a(s,2),Z)),T,Or),close(1,2,3,4,Yt)", debug=true)
  // clause with NAF in the body
  PB2LogicParser.parseClause("initiatedAt(meeting(X0,X1,45),1,Z,Petryb,a(p(2,3,z(23,g,f,ert(sdjfskj,Xkjsh))),1),oo(12,23,E)) :- not happensAt(walking(X,34,p(a(s,2),Z)),T,Or),close(1,2,3,4,Yt),not happens(a(X,R,T,z(a,23,4)))", debug=true)


  val mlnTest = PB2LogicParser.parseAtom("initiatedAt(meeting(a(k,l,m),b,45),c,a,d)", debug=true)
  val a = Literal.toMLNFlat(mlnTest.asInstanceOf[Literal])
  println(a.tostring)

  val mlnTest1 = PB2LogicParser.parseAtom("initiatedAt(c,A,d)", debug=true)
  val b = Literal.toMLNFlat(mlnTest1.asInstanceOf[Literal])
  println(b.tostring)
}




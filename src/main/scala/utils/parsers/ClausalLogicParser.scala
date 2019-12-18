/*
 * Copyright (C) 2016  Nikos Katzouris
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

package utils.parsers

import logic.Exceptions._
import logic._

import scala.util.parsing.combinator.JavaTokenParsers

object Test1 extends App {
  val p = new ClausalLogicParser
  val x = p.parse(p.literal, "initiatedAt(meeting(a(p,test(c,X,59,e(xmymz))),X,45),T)").getOrElse(throw new RuntimeException)
  println(x.tostring)
}

class ClausalLogicParser extends JavaTokenParsers {

  def lowerCaseIdent: Parser[String] = """[a-z][a-zA-Z0-9_]*""".r
  def upperCaseIdent: Parser[String] = """[A-Z][a-zA-Z0-9_]*""".r
  def anyWord: Parser[String] = """[A-Za-z0-9_,()+-\\#]*""".r ^^ { x => x }
  def anyWord1: Parser[String] = """[A-Za-z0-9_()+-\\#]*""".r ^^ { x => x } // no ','
  def quoted: Parser[String] = "\"" ~ anyWord ~ "\"" ^^ { case "\"" ~ x ~ "\"" => "\"" + x + "\"" } | "\'" ~ anyWord ~ "\'" ^^ { case "\'" ~ x ~ "\'" => "\'" + x + "\'" }
  def naf: Parser[String] = "not " ~ rep("\\s+") ^^ { _ => "not" }
  def iff: Parser[String] = rep("\\s+") ~ ":-" ~ rep("\\s+") ^^ { _ => ":-" }
  def number: Parser[String] = floatingPointNumber
  def quotedNumber: Parser[String] = "\"" ~ floatingPointNumber ~ "\"" ^^ { case "\"" ~ x ~ "\"" => "\"" + x + "\"" }
  def variable: Parser[Expression] = upperCaseIdent ^^ { x => Variable(x) }
  def constant: Parser[Expression] = (lowerCaseIdent | quoted) ^^ { x => Constant(x) } | (number | quotedNumber) ^^ { x => Constant(x) }
  def term: Parser[Expression] = literal | variable | constant
  def innerTerms: Parser[List[Expression]] = "(" ~> repsep(term, ",") <~ ")"
  def literal: Parser[Literal] = (
    naf ~ lowerCaseIdent ~ innerTerms ^^ { case naf ~ functor ~ inner => Literal(predSymbol = functor, terms = inner, isNAF = true) }
    | lowerCaseIdent ~ innerTerms ^^ { case functor ~ inner => Literal(predSymbol = functor, terms = inner) })
  def atom: Parser[PosLiteral] = lowerCaseIdent ~ innerTerms ^^ { case functor ~ inner => PosLiteral(functor = functor, terms = inner) }
  def clauseHead: Parser[PosLiteral] = atom
  def clauseBody: Parser[List[Literal]] = repsep(literal, ",")
  def clause: Parser[Clause] = clauseHead ~ iff ~ clauseBody ^^ { case head ~ iff ~ body => Clause(head, body) }

  def parseOutput(parser: Parser[Expression], expression: String): Expression = {
    getParseResult(parse(parser, expression))
  }

  def parse(parser: Parser[Expression], expression: String): Option[Expression] = {
    parseAll(parser, expression) match {
      case Success(result, _) => Some(result)
      case f => None
    }
  }

  def getParseResult(x: Option[Expression]): Expression = x match {
    case Some(y) => y
    case _ => throw new MyParsingException(x.toString)
  }

}

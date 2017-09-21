package utils.parsers

import logic.Exceptions._
import logic._

import scala.util.parsing.combinator.JavaTokenParsers


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
      naf ~ lowerCaseIdent ~ innerTerms ^^ { case naf ~ functor ~ inner => Literal(functor = functor, terms = inner, isNAF = true) }
      | lowerCaseIdent ~ innerTerms ^^ { case functor ~ inner => Literal(functor = functor, terms = inner, isNAF = false) })
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
         case f                  => None
      }
   }

   def getParseResult(x: Option[Expression]): Expression = x match {
      case Some(y) => y
      case _       => throw new MyParsingException(x.toString)
   }

}
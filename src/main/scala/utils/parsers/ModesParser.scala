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

import com.typesafe.scalalogging.LazyLogging
import logic.Exceptions._
import logic.Modes._
import logic._

import scala.util.parsing.combinator.JavaTokenParsers

final class ModesParser extends JavaTokenParsers with LazyLogging {

   def lowerCaseIdent: Parser[String] = """[a-z][a-zA-Z0-9_]*""".r
   def upperCaseIdent: Parser[String] = """[A-Z][a-zA-Z0-9_]*""".r
   def num: Parser[String] = """[0-9]*""".r
   def innerPositionTerms: Parser[List[String]] = "(" ~> repsep(num, ",") <~ ")"
   def naf: Parser[String] = "not " ~ rep("\\s+") ^^ { _ => "not" }
   def mh: Parser[String] = "modeh" ^^ { x => x }
   def mb: Parser[String] = "modeb" ^^ { x => x }
   def ep: Parser[String] = "examplePattern" ^^ { x => x }
   def ip: Parser[String] = "inputPredicate" ^^ { x => x }
   def cp: Parser[String] = "comparisonPredicate" ^^ { x => x }
   def posplmrk: Parser[PosPlmrk] = "+" ~ lowerCaseIdent ^^ { case "+" ~ x => PosPlmrk(x) }
   def negplmrk: Parser[NegPlmrk] = "-" ~ lowerCaseIdent ^^ { case "-" ~ x => NegPlmrk(x) }
   def constplmrk: Parser[ConstPlmrk] = "#" ~ lowerCaseIdent ^^ { case "#" ~ x => ConstPlmrk(x) }
   def placemarker: Parser[Expression] = (posplmrk | negplmrk | constplmrk) ^^ { x => x }
   def inner: Parser[List[Expression]] = "(" ~> repsep(modeAtom | placemarker, ",") <~ ")"
   //def modeAtom: Parser[ModeAtom] = lowerCaseIdent ~ inner ^^ { case x ~ y => new ModeAtom(x.toString, y) }

   def modeAtom: Parser[ModeAtom] =
      (  naf ~ lowerCaseIdent ~ inner ^^ { case not ~ x ~ y => ModeAtom(functor = x.toString, args = y, isNAF = true) }
       | lowerCaseIdent ~ inner ^^ { case x ~ y => ModeAtom(functor = x.toString, args = y) }  )

   def comparisonTermPositionIdentifier: Parser[List[Int]] = "comparison_term_position" ~ innerPositionTerms ^^ {
     case "comparison_term_position" ~ innerPositionTerms => innerPositionTerms.map(_.toInt)
   }

   def modeh: Parser[ModeAtom] = mh ~ "(" ~ modeAtom ~ (")"|").") ^^ { case mh ~ "(" ~ m ~ (")"|").") => m }
   def modeb: Parser[ModeAtom] = mb ~ "(" ~ modeAtom ~ (")"|").") ^^ { case mb ~ "(" ~ m ~ (")"|").") => m }
   def mode: Parser[ModeAtom] = modeh | modeb
   def exmplPattern: Parser[ModeAtom] = ep ~ "(" ~ modeAtom ~ (")"|").") ^^ { case ep ~ "(" ~ m ~ (")"|").") => m }
   def inputPred: Parser[ModeAtom] = ip ~ "(" ~ modeAtom ~ (")"|").") ^^ { case ep ~ "(" ~ m ~ (")"|").") => m }
   def compPred: Parser[ModeAtom] =
     cp ~ "(" ~ modeAtom ~ "," ~ ("lessThan" | "greaterThan") ~ "," ~ comparisonTermPositionIdentifier ~ (")"|").") ^^ {
       case cp ~ "(" ~ m ~ "," ~ compTerm ~ "," ~ comparisonTermPositionIdentifier ~ (")"|").") =>
         m.compRelation = compTerm
         m.comparisonTermPosition = comparisonTermPositionIdentifier
         m
     }

   def parseModes(parser: Parser[ModeAtom], expression: String): Option[ModeAtom] = {
      parseAll(parser, expression) match {
         case Success(x, _) => Some(x)
         case Failure(msg, _) =>
            logger.error("FAILURE: " + msg)
            logger.error("while parsing "+expression)
            None
         case Error(msg, _) => println("ERROR: " + msg); None
         //case _ => None
      }
   }

   def getParseResult(x: Option[ModeAtom]): ModeAtom = x match {
      case Some(y) => y
      case _       => throw new MyParsingException
   }
}

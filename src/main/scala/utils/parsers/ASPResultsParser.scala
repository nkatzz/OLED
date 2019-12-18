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

class ASPResultsParser extends ClausalLogicParser {

  def aspResult: Parser[List[String]] = repsep(literal, "") ^^ { case x => for (y <- x) yield y.tostring }

  def parseASP(parser: Parser[Any], expression: String): Option[Any] = {
    parseAll(parser, expression) match {
      case Success(result, _) => Some(result)
      case Failure(msg, _) =>
        println("FAILURE: " + msg); None
      case Error(msg, _) => println("ERROR: " + msg); None
    }
  }

  def parsed(x: Option[Any]): Boolean = x match {
    case Some(y) => true
    case _ => false
  }

  def getResult(x: Option[Any]): Any = x match {
    case Some(y) => y
    case _ => false
  }

}

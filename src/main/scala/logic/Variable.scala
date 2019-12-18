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

package logic

/** A variable is any term that starts with an upper-case letter */

case class Variable(override val name: String, inOrOutVar: String = "", override val _type: String = "") extends Expression {
  require(name.toCharArray()(0).isUpper) // else throws an IllegalArgumentException
  override def tostring = name
  override def tostringQuote = if (inOrOutVar == "-" || inOrOutVar == "#") "\"" + name + "\"" else name
  def asLiteral = Literal(predSymbol = name)
}

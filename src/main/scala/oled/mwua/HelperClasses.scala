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

package oled.mwua

import app.runutils.RunningOptions
import logic.{Clause, Literal, Theory}

/**
  * Created by nkatz at 9/2/2019
  */
object HelperClasses {

  // Label can be "true", "false" or "unknown".
  class AtomTobePredicted(val atom: String, val fluent: String, val atomParsed: Literal,  val time: Int,
                          val initiatedBy: Vector[String], val terminatedBy: Vector[String], val label: String = "unknown")
}

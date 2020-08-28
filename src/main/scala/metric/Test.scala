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

package metric

import lomrf.logic.{AtomicFormula, Constant, Variable}

/**
  * Created by nkatz on 18/12/19.
  */
object Test extends App {

  val metric = AtomMetric(HungarianMatcher)

  val d = metric.distance(
    IndexedSeq(AtomicFormula("A", Vector(Variable("x"), Constant("R")))),
    IndexedSeq(AtomicFormula("B", Vector(Variable("y"), Constant("E"))))
  )

  println(d)
}

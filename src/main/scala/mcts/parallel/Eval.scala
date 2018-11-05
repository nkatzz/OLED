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

package mcts.parallel

import java.util.UUID
import app.runutils.Globals
import logic.Examples.Example
import logic.{Literal, Theory}
import utils.{ASP, Utils}

/**
  * Created by nkatz on 9/22/17.
  */

object Eval {

  def crossVal(t: Theory, data: Iterator[Example], globals: Globals) = {
    while (data.hasNext) {
      val e = data.next()
      val s = t.clauses.map(x => x.withTypePreds(globals).tostring).mkString("\n")
      evaluateTheory(t, s, e, globals)
    }
  }

  def evaluateTheory(theory: Theory, stringTheory: String, e: Example, globals: Globals): Unit = {
    val varbedExmplPatterns = globals.EXAMPLE_PATTERNS_AS_STRINGS
    val coverageConstr = s"${globals.TPS_RULES}\n${globals.FPS_RULES}\n${globals.FNS_RULES}"
    val show = globals.SHOW_TPS_ARITY_1 + globals.SHOW_FPS_ARITY_1 + globals.SHOW_FNS_ARITY_1
    val ex = e.tostring
    val program = ex + globals.INCLUDE_BK(globals.BK_CROSSVAL) + stringTheory + coverageConstr + show
    val f = Utils.getTempFile(s"eval-theory-${UUID.randomUUID().toString}-${System.currentTimeMillis()}", ".lp")
    Utils.writeLine(program, f.getCanonicalPath, "overwrite")
    val answerSet = ASP.solve(task = Globals.INFERENCE, aspInputFile = f)
    if (answerSet.nonEmpty) {
      val atoms = answerSet.head.atoms
      atoms.foreach { a=>
        val lit = Literal.parse(a)
        val inner = lit.terms.head
        lit.functor match {
          case "tps" => theory.tps += inner.tostring
          case "fps" => theory.fps += inner.tostring
          case "fns" => theory.fns += inner.tostring
        }
      }
    }
  }

}

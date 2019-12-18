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

package woled

import logic.{Clause, Theory}

/**
 * Created by nkatz at 13/10/19
 */

object Pruning {

  def naivePruningStrategy(rules: Vector[Clause], threshold: Double, logger: org.slf4j.Logger) = {
    val (drop, keep) = rules.partition(rule => rule.body.size >= 3 && rule.precision < threshold)
    logger.info(s"\nDropped rules:\n${Theory(drop.toList).showWithStats}")
    keep
  }

  def oldestFirst() = {

  }

  def worstFirst() = {

  }

  /* Combination of the two */
  def oldestWorst() = {

  }

}

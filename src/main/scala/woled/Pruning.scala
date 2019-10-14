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

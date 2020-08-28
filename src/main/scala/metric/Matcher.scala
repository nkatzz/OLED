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

import breeze.optimize.linear.KuhnMunkres

/**
  * Matcher is any object that solves an assignment problem. The problem consists of finding
  * a maximum cost matching (or a minimum cost perfect matching) in a bipartite graph. The input
  * graph is usually represented as a cost matrix. Zero values define the absence of edges.
  *
  * === General Formulation ===
  *
  * Each problem instance has a number of agents and a number of tasks. Any agent can be assigned to
  * any task, incurring a cost that may vary depending on the agent-task assignment. It is required
  * that all tasks are assigned to exactly one agent in such a way that the total cost is minimized.
  * In case the numbers of agents and tasks are equal and the total cost of the assignment for all tasks
  * is equal to the sum of the costs for each agent then the problem is called the linear assignment problem.
  *
  * @see https://en.wikipedia.org/wiki/Assignment_problem
  */
trait Matcher extends (Seq[Seq[Double]] => (Array[Int], Double))

/**
  * The Hungarian matcher is a combinatorial optimization algorithm that solves the assignment problem in
  * polynomial time O(n&#94;3).
  *
  * @see https://en.wikipedia.org/wiki/Hungarian_algorithm
  */
object HungarianMatcher extends Matcher {

  /**
    * It solves the assignment problem for the given cost matrix. The cost
    * matrix represents the costs for each edge in the graph.
    *
    * @param costMatrix the bipartite graph cost matrix
    * @return the cost of the optimal assignment
    */
  override def apply(costMatrix: Seq[Seq[Double]]): (Array[Int], Double) = {
    val unmatched = math.abs(costMatrix.length - costMatrix.head.length)
    val maxDimension = math.max(costMatrix.length, costMatrix.head.length)

    KuhnMunkres.extractMatching(costMatrix) match {
      case (matches, cost) => matches.toArray -> (cost + unmatched) / maxDimension
    }
  }
}

/**
  * The Hausdorff matcher is based on the Hausdorff distance. The Hausdorff distance is the longest distance
  * you can be forced to travel by an adversary that chooses a point in one set, from where you then must travel
  * to the other set. In other words, it is the greatest of all the distances from a point in one set to the
  * closest point in another set.
  *
  * @note The Hausdorff matcher can be used for solving the assignment problem, but the solution is not
  *       guaranteed to be the optimal one. Moreover, the matching is not guaranteed to be one to one.
  * @see https://en.wikipedia.org/wiki/Hausdorff_distance
  *      Distance Between Herbrand Interpretations: A Measure for Approximations
  *      to a Target Concept (1997)
  */
object HausdorffMatcher extends Matcher {

  /**
    * It solves the assignment problem for a given cost matrix. The cost
    * matrix represents the costs for each edge in the graph.
    *
    * @param costMatrix the bipartite graph cost matrix
    * @return the cost of the assignment
    */
  override def apply(costMatrix: Seq[Seq[Double]]): (Array[Int], Double) =
    Array.empty[Int] -> math.max(costMatrix.map(_.min).max, costMatrix.transpose.map(_.min).max)
}

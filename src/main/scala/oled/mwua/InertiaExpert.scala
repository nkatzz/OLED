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

class InertiaExpert {

  private var weightMemory: scala.collection.mutable.Map[String, Double] = scala.collection.mutable.Map[String, Double]()

  def knowsAbout(fluent: String) = {
    weightMemory.keySet.contains(fluent)
  }

  def forget(fluent: String) = {
    weightMemory -= fluent
  }

  val decayingFactor = 1.0 //0.05

  def getWeight(fluent: String) = {
    if (weightMemory.keySet.contains(fluent)) weightMemory(fluent) * decayingFactor else 0.0
  }

  def updateWeight(fluent: String, newWeight: Double) = {
    weightMemory += (fluent -> newWeight)
  }

  def clear() = {
    weightMemory = scala.collection.mutable.Map[String, Double]()
  }

  def getMemory() = weightMemory

}

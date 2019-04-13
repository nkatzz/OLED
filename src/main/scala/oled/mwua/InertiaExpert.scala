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

  def getMemory() = weightMemory




}

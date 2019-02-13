package oled.mwua

class InertiaExpert {

  private var weightMemory: scala.collection.mutable.Map[String, Double] = scala.collection.mutable.Map[String, Double]()

  def knowsAbout(fluent: String) = {
    weightMemory.keySet.contains(fluent)
  }

  def forget(fluent: String) = {
    weightMemory -= fluent
  }

  def getWeight(fluent: String) = {
    if (weightMemory.keySet.contains(fluent)) weightMemory(fluent) else 0.0
  }

  def updateWeight(fluent: String, newWeight: Double) = {
    weightMemory += (fluent -> newWeight)
  }




}

package oled.winnow

object InertiaFluent {
  def apply(): InertiaFluent = {
    InertiaFluent("", 0, 0.0)
  }
}
case class InertiaFluent(fluentAtom: String, time: Int, var weight: Double) {

  def increaseWeight() = {
    val newWeight = this.weight * Math.pow(Math.E, 1.0)
    this.weight = if (newWeight.isPosInfinity) this.weight else newWeight
  }

  def decreaseWeight() = {
    val newWeight = this.weight * Math.pow(Math.E, -1.0)
    this.weight = newWeight
  }

}

class InertiaExpert {

  private var Atoms = Set[InertiaFluent]()

}

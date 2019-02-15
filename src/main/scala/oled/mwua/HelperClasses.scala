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

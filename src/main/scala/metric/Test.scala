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

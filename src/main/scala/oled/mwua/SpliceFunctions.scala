package oled.mwua

import app.runutils.RunningOptions
import logic.{Clause, Literal}
import logic.Examples.Example
import oled.mwua.AuxFuncs.{computeRuleGroundings, marked}
import oled.mwua.HelperClasses.{AtomTobePredicted}
import utils.Utils

object SpliceFunctions {



  def process(batch: Example,
              trueLabels: Set[String],
              spliceLabels: Map[String, String],
              inps: RunningOptions,
              stateHandler: StateHandler,
              testHandCrafted: Boolean = false) = {

    val (sortedAtomsToBePredicted, markedProgram, markedMap, groundingsMap, _runTimeVars) =
      groundEnsemble(batch, trueLabels, spliceLabels, inps, stateHandler, testHandCrafted)

    sortedAtomsToBePredicted foreach { atom =>

      val prediction = predict(atom, stateHandler, markedMap)

      /* HERE I SHOULD SEND THE PREDICTION TO SPLICE. */
      if (atom.label == "unknown") {
        // call splice
      } else {

      }

    }

  }




  /* Generate groundings of the rules currently in the ensemble. */

  def groundEnsemble(batch: Example,
                     trueLabels: Set[String],
                     spliceLabels: Map[String, String],
                     inps: RunningOptions,
                     stateHandler: StateHandler,
                     testHandCrafted: Boolean = false) = {

    val ensemble = stateHandler.ensemble
    val merged = ensemble.merged(inps, testHandCrafted)
    val _marked = marked(merged.clauses.toVector, inps.globals)
    val markedProgram = _marked._1
    val markedMap = _marked._2
    val e = (batch.annotationASP ++ batch.narrativeASP).mkString("\n")
    val trueAtoms = batch.annotation.toSet
    val groundingsMapTimed = Utils.time { computeRuleGroundings(inps, markedProgram, markedMap, e, trueAtoms) }
    val groundingsMap = groundingsMapTimed._1
    val groundingsTime = groundingsMapTimed._2
    stateHandler.updateGrndsTimes(groundingsTime)
    val sortedAtomsToBePredicted = sortGroundingsByTime(groundingsMap, trueLabels, spliceLabels)
    (sortedAtomsToBePredicted, markedProgram, markedMap, groundingsMap, stateHandler)
  }


  // givenLabels are the real annotation given in the case of full supervision. sliceLabels are the labels
  // received by splice in case of partial supervision.
  def sortGroundingsByTime(groundingsMap: scala.collection.mutable.Map[String, (scala.Vector[String], scala.Vector[String])],
                           givenLabels: Set[String] = Set[String](),
                           sliceLabels: Map[String, String] = Map[String, String]()) = {

    val objs = groundingsMap.foldLeft(Vector[AtomTobePredicted]()) { (accum, mapEntry) =>
      val (atom, initBy, termBy) = (mapEntry._1, mapEntry._2._1, mapEntry._2._2)
      val parsed = Literal.parse(atom)
      val time = parsed.terms.tail.head.name.toInt
      val fluent = parsed.terms.head.tostring
      val label =
        if(sliceLabels.isEmpty) {
          if (givenLabels.contains(atom)) "true" else "false"
        } else {
          sliceLabels(atom)
        }
      val obj = new AtomTobePredicted(atom, fluent, time, initBy, termBy, label)

      accum :+ obj
    }
    objs.sortBy(x => x.time)
  }








  //inertiaExpert = scala.collection.mutable.Map[String, Double]()

  /* Make a prediction on the current atom */
  def predict(a: AtomTobePredicted, stateHanlder: StateHandler, markedMap: Map[String, Clause]) = {

    val (currentAtom, currentTime, awakeInit, awakeTerm, currentFluent) = (a.atom, a.time, a.initiatedBy, a.terminatedBy, a.fluent)

    val inertiaExpertPrediction = stateHanlder.getInertiaExpertPrediction(currentFluent)


    val initWeightSum = if (awakeInit.nonEmpty) awakeInit.map(x => markedMap(x).w).sum else 0.0
    val termWeightSum = if (awakeTerm.nonEmpty) awakeTerm.map(x => markedMap(x).w).sum else 0.0

    val prediction = inertiaExpertPrediction + initWeightSum - termWeightSum

    prediction
  }

  def updateWeights() = {

  }

  def updateStructure() = {

  }

}

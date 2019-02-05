package oled.mwua

import app.runutils.RunningOptions
import logic.{Clause, Literal}
import logic.Examples.Example
import oled.mwua.AuxFuncs.{computeRuleGroundings, marked}
import utils.Utils

object SpliceFunctions {

  def groundEnsemble(ensemble: RuleEnsemble, batch: Example, inps: RunningOptions, runTimeVars: RuntimeVars, testHandCrafted: Boolean = false) = {

    val merged = ensemble.merged(inps, testHandCrafted)

    val _marked = marked(merged.clauses.toVector, inps.globals)
    val markedProgram = _marked._1
    val markedMap = _marked._2
    val e = (batch.annotationASP ++ batch.narrativeASP).mkString("\n")

    val trueAtoms = batch.annotation.toSet

    val groundingsMapTimed = Utils.time{
      computeRuleGroundings(inps, markedProgram, markedMap, e, trueAtoms)
    }

    val groundingsMap = groundingsMapTimed._1
    val groundingsTime = groundingsMapTimed._2

    runTimeVars.updateGrndsTimes(groundingsTime)

    (markedProgram, markedMap, groundingsMap, runTimeVars)
  }

  def predict(currentInput: ((String, Int), (scala.Vector[String], scala.Vector[String]))) = {

    val (currentAtom, currentTime) = (currentInput._1._1, currentInput._1._2)
    val (awakeInit, awakeTerm) = (currentInput._2._1, currentInput._2._2)

    val parsed = Literal.parse(currentAtom)
    val currentFluent = parsed.terms.head.tostring

    /*
    // only updates weights when we're not running in test mode.
    val prediction =
      predictAndUpdate(currentAtom, currentFluent,
        initiatedBy, terminatedBy, markedMap, testOnly, trueAtoms, batch)
    */

  }

  def updateWeights() = {

  }

  def updateStructure() = {

  }

}

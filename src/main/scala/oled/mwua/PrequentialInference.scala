package oled.mwua

import app.runutils.RunningOptions
import logic.Clause
import logic.Examples.Example
import oled.mwua.ExpertAdviceFunctions.{getFeedback, ground, groundEnsemble, predict, predictHedge, sortGroundingsByTime}
import oled.mwua.HelperClasses.AtomTobePredicted

class PrequentialInference(val batch: Example,
                           val inps: RunningOptions,
                           val stateHandler: StateHandler,
                           val trueAtoms: Set[String],
                           val hedgePredictionThreshold: Double,
                           val testingMode: Boolean = false,
                           val streaming: Boolean = false,
                           val feedBackGap: Int = 0,
                           val withInertia: Boolean = true) {

  private val withInputTheory = testingMode
  private var batchError = 0
  private var batchFPs = 0
  private var batchFNs = 0
  private var batchAtoms = 0
  private var atomCounter = 0 //used for feedback gap
  private var alreadyProcessedAtoms = Set.empty[String]
  private var finishedBatch = false

  val weightUpdateStrategy = "hedge" // this needs to become a global parameter

  def predictAndUpdate() = {

    while(!finishedBatch) {
      val (markedProgram, markedMap, groundingsMap, times, sortedAtomsToBePredicted, orderedTimes) =
        ground(batch, inps, stateHandler, withInputTheory, streaming)



    }

  }

  def predict(atom: AtomTobePredicted, markedMap: Map[String, Clause]) = {
    val currentAtom = atom.atom
    if (!alreadyProcessedAtoms.contains(currentAtom)) {
      if (feedBackGap != 0) atomCounter += 1 // used for experiments with the feedback gap
      alreadyProcessedAtoms = alreadyProcessedAtoms + currentAtom
      batchAtoms += 1
      stateHandler.totalNumberOfRounds += 1

      var prediction = 0.0
      var inertiaExpertPrediction = 0.0
      var initWeightSum = 0.0
      var termWeightSum = 0.0
      var totalWeight = 0.0
      var selected = ""

      val (_prediction, _inertiaExpertPrediction, _initWeightSum, _termWeightSum) =
        /*
        if (weightUpdateStrategy == "winnow") predict(atom, stateHandler, markedMap)
        else predictHedge(atom, stateHandler, markedMap, withInertia)
        //else ClassicSleepingExpertsHedge.predictHedge_NO_INERTIA(atom, stateHandler, markedMap)
        */
        predictHedge(atom, stateHandler, markedMap, withInertia)

      prediction = _prediction
      inertiaExpertPrediction = _inertiaExpertPrediction
      initWeightSum = _initWeightSum
      termWeightSum = _termWeightSum

      val feedback = getFeedback(atom,  Map[String, Double](), None, None, trueAtoms)
      val predictedLabel = if (prediction >= hedgePredictionThreshold) "true" else "false"

    }
  }



}



package oled.mwua

import java.io.{File, PrintWriter}

import scala.util.{Failure, Random, Success}
import app.runutils.{Globals, RunningOptions}
import com.typesafe.scalalogging.{LazyLogging, Logger}
import logic.{Clause, Constant, Literal, Theory}
import logic.Examples.Example
import lomrf.logic.{AtomSignature, EvidenceAtom, TRUE}
import lomrf.mln.model.Evidence
import oled.functions.SingleCoreOLEDFunctions
import oled.mwua
import oled.mwua.AuxFuncs._
import oled.mwua.HelperClasses.AtomTobePredicted
import utils.Utils
import xhail.Xhail

import scala.util.control.Breaks._

object ExpertAdviceFunctions extends LazyLogging {



  // The "batch" argument contains only the evidence atoms here. The annotation atoms found in the batch
  // are passed-in via the "trueAtoms" argument.
  def process(batch: Example,
              trueAtoms: Set[String],
              inps: RunningOptions,
              stateHandler: StateHandler,
              trueLabels: Set[String],
              learningRate: Double,
              epsilon: Double, // used for the randomized version
              randomizedPrediction: Boolean,
              batchCounter: Int,
              percentOfMistakesBeforeSpecialize: Int,
              specializeAllAwakeOnMistake: Boolean,
              receiveFeedbackBias: Double,
              conservativeRuleGeneration: Boolean = true,
              weightUpdateStrategy: String = "winnow", // this is either 'hedge' or 'winnow',
              withInertia: Boolean = true,
              feedBackGap: Int = 0,
              splice: Option[Map[String, Double] => (Set[EvidenceAtom], Evidence)] = None,
              mapper: Option[Set[EvidenceAtom] => Vector[String]] = None,
              incompleteTrueAtoms: Option[Set[String]] = None,
              inputTheory: Option[List[Clause]] = None) = {

    //========================================
    //stateHandler.ensemble.removeZeroWeights
    //========================================


    if(batch.annotation.nonEmpty) {
      val stop = "stop"
    }

    val streaming = true //true
    var batchError = 0
    var batchFPs = 0
    var batchFNs = 0
    var batchAtoms = 0
    var atomCounter = 0 //used for feedback gap
    val hedgePredictionThreshold = 0.5  //hedgePredictionThreshold = Globals.hedgePredictionThreshold
    var withInputTheory = inputTheory.isDefined

    /* TEST PHASE ONLY (NO WEIGHT/STRUCTURE UPDATE) */
    /*
    val isTestPhase = receiveFeedbackBias == 0.0
    if (isTestPhase) {
      setFinalTestingRules(stateHandler, streaming)
      withInputTheory = true
    }
    */

    var spliceInput = Map.empty[String, Double]
    stateHandler.batchCounter = batchCounter

    var alreadyProcessedAtoms = Set.empty[String]
    var finishedBatch = false

    val predictAndUpdateTimed = Utils.time {
      while(!finishedBatch) {

        val (markedProgram, markedMap, groundingsMap, times, sortedAtomsToBePredicted, orderedTimes) =
          ground(batch, inps, stateHandler, withInputTheory, streaming)

        breakable {
          sortedAtomsToBePredicted foreach { atom =>
            val currentAtom = atom.atom

            if (!alreadyProcessedAtoms.contains(currentAtom)) {

              if (feedBackGap != 0) atomCounter += 1 // used for experiments with the feedback gap

              // this should be placed at the end of the iteration. In this way, if
              // a break actual occurs due to structure update we'll retrain on the
              // current example to update weights of the new/revised rules and handle the
              // inertia buffer (e.g. an FN turns into a TP after the addition of an
              // initiation rule, the inertia expert now remembers the atom).
              // UPDATE: Not a very good idea! Why insisting on correcting a particular mistake?
              // After all, the provided feedback/label may be wrong!
              alreadyProcessedAtoms = alreadyProcessedAtoms + currentAtom
              batchAtoms += 1
              stateHandler.totalNumberOfRounds += 1
              //-------------------
              // MAKE A PREDICTION
              //-------------------
              var prediction = 0.0
              var inertiaExpertPrediction = 0.0
              var initWeightSum = 0.0
              var termWeightSum = 0.0
              var totalWeight = 0.0
              var selected = ""

              // The randomized prediction case has not been carried over to the PrequentialInference class.
              // You need to modify the logic there to add it.
              if (!randomizedPrediction) {

                val (_prediction, _inertiaExpertPrediction, _initWeightSum, _termWeightSum) =
                  if (weightUpdateStrategy == "winnow") predict(atom, stateHandler, markedMap)
                  else predictHedge(atom, stateHandler, markedMap, withInertia)
                  //else ClassicSleepingExpertsHedge.predictHedge_NO_INERTIA(atom, stateHandler, markedMap)

                prediction = _prediction
                inertiaExpertPrediction = _inertiaExpertPrediction
                initWeightSum = _initWeightSum
                termWeightSum = _termWeightSum

              } else {
                val (_prediction_, _totalWeight, _selected) = predictRandomized(atom, stateHandler, markedMap)
                prediction = _prediction_
                totalWeight = _totalWeight
                selected = _selected
              }

              // This has not been carried over to the PrequentialInference class.
              // You need to modify the logic there to add it.
              if(splice.isDefined) {
                val time = atom.atomParsed.terms.tail.head.name
                val eventAtom = atom.atomParsed.terms.head.asInstanceOf[Literal]
                val eventPred = eventAtom.functor.capitalize
                val eventArgs = eventAtom.terms.map(x => x.name.capitalize).mkString(",")
                val out = s"$eventPred($eventArgs,$time)"

                if (weightUpdateStrategy == "winnow") {
                  // Rescale the prediction to [0,1]. If I is the interval (range) of the prediction, then
                  // rescaledPrediction = (prediction - min(I))/(max(I) - min(I)),
                  // where min(I) = -termWeightSum
                  // max(I) = initWeightSum + inertiaExpertPrediction
                  val _rescaledPrediction = (prediction - (-termWeightSum))/((initWeightSum + inertiaExpertPrediction) - (-termWeightSum))
                  val rescaledPrediction = {
                    if (_rescaledPrediction.isNaN) 0.0
                    else if (prediction <= 0) (-1) * _rescaledPrediction else _rescaledPrediction
                  }
                  //println(out, rescaledPrediction)
                  spliceInput += (out -> rescaledPrediction)
                } else {
                  // with Hedge predictions are already in [0,1]
                  spliceInput += (out -> prediction)
                }
              }

              // This has not been carried over to the PrequentialInference class.
              // You need to modify the logic there to add it.
              val feedback =
                if (incompleteTrueAtoms.isDefined) {
                  getFeedback(atom, spliceInput, splice, mapper, incompleteTrueAtoms.get)
                } else {
                  getFeedback(atom, spliceInput, splice, mapper, trueAtoms)
                }

              // The Winnow case has not been carried over to the PrequentialInference class.
              // You need to modify the logic there to add it.
              val predictedLabel =
                if (weightUpdateStrategy == "winnow") {
                  if (prediction > 0) "true" else "false"
                } else {
                  //if (prediction > 0) "true" else "false"
                  if (prediction >= hedgePredictionThreshold) "true" else "false"
                }


              // this is required in the randomized prediction case because the inertiaExpertPrediction variable
              // is passed to weight update function and the inertia prediction is necessary for calculating the mistake probability.
              if (selected == "inertia") inertiaExpertPrediction = prediction

              if (incompleteTrueAtoms.isDefined) { // incomplete, splice etc

                if (trueAtoms.contains(atom.atom) && prediction > 0) { // TP
                  stateHandler.totalTPs += 1
                } else if (!trueAtoms.contains(atom.atom) && prediction > 0) { // FP
                  stateHandler.totalFPs += 1
                  batchError += 1
                  batchFPs += 1
                } else if (!trueAtoms.contains(atom.atom) && prediction <= 0) { // TN
                  stateHandler.totalTNs += 1
                } else { //FN
                  stateHandler.totalFNs += 1
                  batchError += 1
                  batchFNs += 1
                }

              } else { // full annotation

                if (predictedLabel != feedback) {
                  batchError += 1
                  if (is_FP_mistake(predictedLabel, feedback)) {
                    batchFPs += 1
                    stateHandler.totalFPs += 1
                    if (selected == "inertia") {
                      logger.info(s"Inertia FP prediction for fluent ${atom.fluent}. " +
                        s"Inertia weight: ${stateHandler.inertiaExpert.getWeight(atom.fluent)}")
                    }
                    //logger.info(s"\nFP mistake for atom $currentAtom. " +
                    //  s"Init w. sum: $initWeightSum, term w. sum: $termWeightSum, inert: $inertiaExpertPrediction")
                  }
                  if (is_FN_mistake(predictedLabel, feedback)) {
                    batchFNs += 1
                    stateHandler.totalFNs += 1
                    //logger.info(s"\nFN mistake for atom $currentAtom. " +
                    //  s"Init w. sum: $initWeightSum, term w. sum: $termWeightSum, inert: $inertiaExpertPrediction")
                  }
                } else {
                  if (predictedLabel == "true") stateHandler.totalTPs += 1 else stateHandler.totalTNs += 1
                }

              }

              // Handles whether we receive feedback or not.
              val update = {
                ///*
                if (receiveFeedbackBias == 1.0) {
                  true
                } else {
                  val p = Math.random()
                  if (p <= receiveFeedbackBias) true else false
                }
                //*/

                /*
                if (atomCounter < feedBackGap) {
                  false
                } else if (atomCounter == feedBackGap) {
                  atomCounter = 0
                  true
                } else throw new RuntimeException("Problem with atom counter for feedback gap...")
                */
              }

              var generateNewRuleFlag = false

              if (update) {
                stateHandler.receivedFeedback += 1
                if (!randomizedPrediction) {
                  // in Winnow mode this updates weights only after a mistake (in the TP case it just updates the inertia expert)

                  ///*
                  generateNewRuleFlag = updateWeights(atom, prediction, inertiaExpertPrediction, initWeightSum,
                    termWeightSum, predictedLabel, markedMap, feedback, stateHandler, learningRate, weightUpdateStrategy, withInertia)
                  // */

                  /*
                  ClassicSleepingExpertsHedge.updateWeights_NO_INERTIA(atom, prediction, inertiaExpertPrediction, initWeightSum,
                    termWeightSum, predictedLabel, markedMap, feedback, stateHandler, learningRate, weightUpdateStrategy)
                  */

                  // Do not normalize when splice is used, to allow for larger confidence values
                  /*
                  if (randomizedPrediction) { //weightUpdateStrategy == "winnow" //randomizedPrediction //splice.isEmpty
                    stateHandler.normalizeWeights( (atom.initiatedBy ++ atom.terminatedBy).map(x => markedMap(x)), atom.fluent )
                  }
                  */

                } else {
                  updateWeightsRandomized(atom, prediction, inertiaExpertPrediction,
                    predictedLabel, feedback, stateHandler, epsilon, markedMap, totalWeight)
                }

                if (predictedLabel != feedback) {
                  if (!withInputTheory) { // we only update structure when an input theory is given
                    // Shouldn't we update the weights on newly generated rules here?
                    // Of course it's just 1 example, no big deal, but still...

                    /*
                    val structureUpdate_? = updateStructure_NEW(atom, markedMap, predictedLabel, feedback, batch,
                      currentAtom, inps, Logger(this.getClass).underlying, stateHandler, percentOfMistakesBeforeSpecialize,
                      randomizedPrediction, selected, specializeAllAwakeOnMistake, conservativeRuleGeneration)
                    */

                    ///*
                    val previousTime = if (streaming) orderedTimes( orderedTimes.indexOf(atom.time) -1 ) else 0
                    val structureUpdate_? =
                      ClassicSleepingExpertsHedge.updateStructure_NEW_HEDGE(atom, previousTime, markedMap, predictedLabel,
                        feedback, batch, currentAtom, inps, Logger(this.getClass).underlying, stateHandler,
                        percentOfMistakesBeforeSpecialize, randomizedPrediction, selected, specializeAllAwakeOnMistake,
                        conservativeRuleGeneration, generateNewRuleFlag)
                    //*/

                    if (structureUpdate_?) break
                  }
                }
              } else {

                /*===============================================================================================================*/
                /* This is all helper/test code for updating weights after mini-batch prediction from all mistakes cumulatively. */
                /*============================================ Test-helper code start ===========================================*/

                val delayedUpdate = new DelayedUpdate(atom, prediction, inertiaExpertPrediction,
                  initWeightSum, termWeightSum, predictedLabel, markedMap, feedback, stateHandler,
                  learningRate, weightUpdateStrategy, withInertia, orderedTimes)

                stateHandler.delayedUpdates = stateHandler.delayedUpdates :+ delayedUpdate

                /* ============================================Test-helper code end =============================================*/
                /*===============================================================================================================*/

                // Here we do not receive feedback. We need to handle inertia here.
                // Note that if feedback is received inertia is handled at the updateWeights method.
                // If the prediction is TP (TN) the fluent is added (removed) from the inertia memory.
                // If the prediction is incorrect, then in the FP case the fluent is added to the inertia
                // memory (although it's mistake) so things are fair and then we try to correct the mistake
                // in subsequent rounds if it persists. In the FN case we reduce the inertia weight (if non-zero)
                // for the erroneously predicted fluent. Here we are in the case where no feedback is received
                // (so its purely testing and/or providing an inference service), so we need to handle inertia.
                /*
                if (predictedLabel == "true") {
                  if (!stateHandler.inertiaExpert.knowsAbout(atom.fluent))
                    stateHandler.inertiaExpert.updateWeight(atom.fluent, prediction)
                } else {
                  if (!stateHandler.inertiaExpert.knowsAbout(atom.fluent))
                    stateHandler.inertiaExpert.forget(atom.fluent)
                }
                */
              }

              // Not a very good idea to do this here (comment, last line). The reason to do it here is to
              // force the system to correct this particular mistake (it will be added
              // to the alreadySeenAtoms only if the prediction on this atom is correct).
              // But Why insisting on correcting a particular mistake?
              // After all, the provided feedback/label may be wrong!
              //alreadyProcessedAtoms = alreadyProcessedAtoms + currentAtom
            }
          }
          finishedBatch = true
          stateHandler.perBatchError = stateHandler.perBatchError :+ batchError
          stateHandler.updateRunningF1Score


          //=======================================================
          //=======================================================
          //=======================================================
          //*******************************************************

          // I need to get this to work
          //stateHandler.pruneUnderPerformingRules(0.0000000000001)

          //*******************************************************
          //=======================================================
          //=======================================================
          //=======================================================

          // Try to specialize all rules currently in the ensemble
          // Just to be on the safe side filter out rules with no refinements
          // (i.e. rules that have "reached" their bottom rule).

          if (!withInputTheory) { // Update structure only when we are learning from scratch
            ///*
            val expandedInit =
              SingleCoreOLEDFunctions.expandRules(Theory(stateHandler.ensemble.initiationRules.filter(x => x.refinements.nonEmpty)),
                inps, Logger(this.getClass).underlying)

            val expandedTerm =
              SingleCoreOLEDFunctions.expandRules(Theory(stateHandler.ensemble.terminationRules.filter(x => x.refinements.nonEmpty)),
                inps, Logger(this.getClass).underlying)

            stateHandler.ensemble.initiationRules = expandedInit._1.clauses
            stateHandler.ensemble.terminationRules = expandedTerm._1.clauses
            //*/
          }
        }
      }
    }

    //println(s"Batch processing time: ${predictAndUpdateTimed._2}")

    //logger.info(s"Batch processing time: ${predictAndUpdateTimed._2}")
    if (batchError > 0) {
      logger.info(s"*** Batch #$batchCounter Total mistakes: ${batchFPs+batchFNs} " +
        s"(FPs: $batchFPs | FNs: $batchFNs). Total batch atoms: $batchAtoms ***")
    }
    // This is only needed for a dirty hack at Learner_NEW...
    batchError
  }



  def setFinalTestingRules(stateHandler: StateHandler, streaming: Boolean) = {
    val weightThreshold = 1.0 //0.00001 //1.1 // // 0.0
    val (goodInit, goodTerm) = getFinalRulesDefault(stateHandler, weightThreshold)
    stateHandler.ensemble.initiationRules = goodInit
    stateHandler.ensemble.terminationRules = goodTerm
    if (!streaming) {
      logger.info(s"Testing with Initiation:\n${Theory(stateHandler.ensemble.initiationRules.sortBy(x => -x.w_pos)).showWithStats}")
      logger.info(s"Testing with Termination:\n${Theory(stateHandler.ensemble.terminationRules.sortBy(x => -x.w_pos)).showWithStats}")
    }
  }


  def getFinalRulesDefault(s: StateHandler, weightThreshold: Double) = {

    val isGood = (r: Clause) => r.w_pos > weightThreshold //r.w_pos >= weightThreshold

    def getGoodRules(x: List[Clause], threshold: Double) = {
      x.foldLeft(List.empty[Clause]) { (accum, rule) =>
        if (isGood(rule)) accum :+ rule else accum
      }
    }

    val init = getGoodRules(s.ensemble.initiationRules, weightThreshold)
    val term = getGoodRules(s.ensemble.terminationRules, weightThreshold)

    //val init = (s.ensemble.initiationRules ++ s.ensemble.initiationRules.flatMap(x => x.refinements :+ x.supportSet.clauses.head)).filter(x => x.body.nonEmpty)
    //val term = (s.ensemble.terminationRules ++ s.ensemble.terminationRules.flatMap(x => x.refinements :+ x.supportSet.clauses.head)).filter(x => x.body.nonEmpty)

    (init, term)

  }


  def updateWeightsRandomized(atom: AtomTobePredicted, prediction: Double,
                              inertiaExpertPrediction: Double, predictedLabel: String,
                              feedback: String, stateHandler: StateHandler, epsilon:Double,
                              markedMap: Map[String, Clause], totalWeight: Double) = {


    def weightNoInfinity(prev: Double, _new: Double) = {
      if (_new.isPosInfinity) prev else _new
    }

    def getMistakeProbability(incorrectExperts: Vector[Clause], isInertiaCorrect: Boolean) = {
      // The algorithm's probability of making a mistake is the sum, for all awake
      // experts, of the each expert's h_i selection probability (h_i/totalAwakeWeight)
      // times 1 (if the expert is incorrect) or 0 (if the expert is correct). Since
      // correct experts do not contribute to the sum we only take into account the incorrect ones.
      if (isInertiaCorrect) {
        incorrectExperts.map(i => i.w_pos/totalWeight.toDouble).sum
      } else {
        incorrectExperts.map(i => i.w_pos/totalWeight.toDouble).sum + inertiaExpertPrediction/totalWeight.toDouble
      }

    }

    def updateRulesWeights(correctExperts: Vector[String], incorrectExperts: Vector[String], isInertiaCorrect: Boolean) = {

      val inc = incorrectExperts.map(x => markedMap(x))
      val mistakeProbability = getMistakeProbability(inc, isInertiaCorrect)
      val correctExponent = mistakeProbability/(1+epsilon)
      //val inCorrectExponent = mistakeProbability/((1+epsilon) - 1)
      val inCorrectExponent = mistakeProbability/(1+epsilon) - 1

      correctExperts foreach { x =>
        val rule = markedMap(x)
        rule.w_pos = weightNoInfinity(rule.w_pos, rule.w_pos * Math.pow(1+epsilon, correctExponent))
      }

      incorrectExperts foreach { x =>
        val rule = markedMap(x)
        rule.w_pos = weightNoInfinity(rule.w_pos, rule.w_pos * Math.pow(1+epsilon, inCorrectExponent))
      }

      if (inertiaExpertPrediction > 0) {
        if (isInertiaCorrect) {
          stateHandler.inertiaExpert.updateWeight(atom.fluent,
            weightNoInfinity(inertiaExpertPrediction, inertiaExpertPrediction * Math.pow(1+epsilon, correctExponent)) )
        } else {
          stateHandler.inertiaExpert.updateWeight(atom.fluent,
            weightNoInfinity(inertiaExpertPrediction, inertiaExpertPrediction * Math.pow(1+epsilon, inCorrectExponent)) )
        }
      }
    }

    val nonFiringInitRules =
      markedMap.filter(x =>
        x._2.head.functor.contains("initiated") && !atom.initiatedBy.toSet.contains(x._1))

    val nonFiringTermRules =
      markedMap.filter(x =>
        x._2.head.functor.contains("terminated") && !atom.terminatedBy.toSet.contains(x._1))

    if (is_TP(predictedLabel, feedback)) {

      if (!stateHandler.inertiaExpert.knowsAbout(atom.fluent)) {
        //logger.info(s"ADDING ${atom.fluent} TO THE INERTIA EXPERT ")
        stateHandler.inertiaExpert.updateWeight(atom.fluent, prediction)
        // Prediction should be positive here (we predicted holds). Check this, just to be on the safe side...
        if (prediction <= 0.0) {
          throw new RuntimeException(s"TP atom with prediction =< 0. " +
            s"At batch: ${stateHandler.batchCounter}, while predicting for atom ${atom.atom}")
        }
      }

      // Awake initiation rules are correct, awake termination rules are incorrect and inertia is correct
      updateRulesWeights(atom.initiatedBy, atom.terminatedBy, true)

      updateRulesScore("TP", atom.initiatedBy.map(x => markedMap(x)), nonFiringInitRules.values.toVector,
        atom.terminatedBy.map(x => markedMap(x)), nonFiringTermRules.values.toVector)

    }

    if (is_FP_mistake(predictedLabel, feedback)) {
      // Awake initiation rules are incorrect, awake termination rules are correct and inertia is incorrect
      updateRulesWeights(atom.terminatedBy,atom.initiatedBy, false)

      updateRulesScore("FP", atom.initiatedBy.map(x => markedMap(x)), nonFiringInitRules.values.toVector,
        atom.terminatedBy.map(x => markedMap(x)), nonFiringTermRules.values.toVector)

      // This is clearly wrong... But the randomized version does not work anyway.
      if (stateHandler.inertiaExpert.knowsAbout(atom.fluent)) stateHandler.inertiaExpert.forget(atom.fluent)

    }

    if (is_FN_mistake(predictedLabel, feedback)) {
      // Awake initiation rules are correct, awake termination rules are incorrect and inertia is incorrect
      updateRulesWeights(atom.initiatedBy, atom.terminatedBy, true)

      updateRulesScore("FN", atom.initiatedBy.map(x => markedMap(x)), nonFiringInitRules.values.toVector,
        atom.terminatedBy.map(x => markedMap(x)), nonFiringTermRules.values.toVector)
    }

    if (is_TN(predictedLabel, feedback)) { // TN
      // Awake initiation rules are incorrect, awake termination rules are correct and inertia is incorrect
      updateRulesWeights(atom.terminatedBy,atom.initiatedBy, false)

      updateRulesScore("TN", atom.initiatedBy.map(x => markedMap(x)), nonFiringInitRules.values.toVector,
        atom.terminatedBy.map(x => markedMap(x)), nonFiringTermRules.values.toVector)

      if (inertiaExpertPrediction > 0.0) stateHandler.inertiaExpert.forget(atom.fluent)

    }
  }


  def updateWeights(atom: AtomTobePredicted, prediction: Double, inertiaExpertPrediction: Double,
                    initWeightSum: Double, termWeightSum: Double, predictedLabel: String,
                    markedMap: Map[String, Clause], feedback: String, stateHandler: StateHandler,
                    learningRate: Double, weightUpdateStrategy: String, withInertia: Boolean = true) = {

    var generateNewRule = false

    val currentFluent = atom.fluent

    val hedge = weightUpdateStrategy == "hedge"

    val getTotalWeight = (x: Vector[Clause]) => x.map(x => x.w_pos).sum

    // These Include empty-bodied rules. Only for score (default, foilgain) update. Used for Hoeffding tests only
    val _awakeInitRules = atom.initiatedBy.map(x => markedMap(x))
    val _awakeTermRules = atom.terminatedBy.map(x => markedMap(x))

    val awakeInitRules = _awakeInitRules.filter(x => x.body.nonEmpty) // exclude empty-bodied rules from prediction and weight normalization
    val awakeTermRules = _awakeTermRules.filter(x => x.body.nonEmpty) // exclude empty-bodied rules from prediction and weight normalization

    // Create a map with the rules and their current weight. We'll use it
    // to show the weight updates in the case of Hedge (this is for debugging).
    var initRulesMap = scala.collection.mutable.Map.empty[Int, (String, Double)]
    var termRulesMap = scala.collection.mutable.Map.empty[Int, (String, Double)]
    awakeInitRules.foreach(x => initRulesMap += (x.## -> (x.tostring, x.w_pos) ) )
    awakeTermRules.foreach(x => termRulesMap += (x.## -> (x.tostring, x.w_pos) ) )

    val totalWeightBeforeUpdate =
      if (withInertia) {
        inertiaExpertPrediction + initWeightSum + termWeightSum
      } else {
        initWeightSum + termWeightSum
      }


    def getSleeping(what: String) = {
      val awake = if (what == "initiated") atom.initiatedBy.toSet else atom.terminatedBy.toSet
      markedMap.filter(x => x._2.head.functor.contains(what) && !awake.contains(x._1))
    }

    val sleepingInitRules = getSleeping("initiated")
    val sleepingTermRules = getSleeping("terminated")

    def updateScore(what: String) = {
      updateRulesScore(what, _awakeInitRules, sleepingInitRules.values.toVector, _awakeTermRules, sleepingTermRules.values.toVector)
    }

    var outcome = ""

    if (is_TP(predictedLabel, feedback)) {
      outcome = "TP"
      if (hedge) {
        // Reduce the weights of termination rules.
        reduceWeights(atom.terminatedBy, markedMap, learningRate, "hedge")

        // Inertia is correct here, so if the inertia expert knows about the
        // current fluent we leave the weight unchanged. If not (i.e. the fluent is
        // initiated for the first time) we deal with that after all weights of other
        // experts have been updated. We then set the inertia weight of this fluent
        // to the weighted sum of the experts for this fluent (the actual prediction value).
        // If we update the weight of the inertia expert before normalization
        // then we have that the total weight of the ensemble after
        // the update // W_{t+1} might be larger than the total weight before the update (W_t).
        // This breaks things up (W_{t+1} is always smaller since erroneous rules are penalized,
        // while the weight of correct ones is left unchanged). This has the effect of actually
        // increasing the weight of correct rules after the normalization:
        // W = W_{t}/W_{t+1} > 1, since W_{t} > W_{t+1} => w_{i,t+1} = W * w_{i,t} > w_{i,t},
        // where w_{i,t} is the weight of the i-th expert (which is unchanged is the expert is correct).
        // If this is messed-up, we'll end-up reducing the weight of correct experts after each round.

      } else {
        // Winnow
        val holdsWeight = inertiaExpertPrediction + initWeightSum
        stateHandler.inertiaExpert.updateWeight(currentFluent, holdsWeight)
      }

    } else if (is_FP_mistake(predictedLabel, feedback)) {
      outcome = "FP"

      if (!hedge) {
        reduceWeights(atom.initiatedBy, markedMap, learningRate)
        increaseWeights(atom.terminatedBy, markedMap, learningRate)
      } else {
        reduceWeights(atom.initiatedBy, markedMap, learningRate, "hedge")
      }

      if (inertiaExpertPrediction > 0.0) {
        val newWeight =
          if (!hedge) inertiaExpertPrediction * Math.pow(Math.E, (-1.0) * learningRate)
          else inertiaExpertPrediction * learningRate
        stateHandler.inertiaExpert.updateWeight(currentFluent, newWeight)
      } else {
        // Remember this since it was recognized. This is the correct approach,
        // since we're doing sequential prediction. If we make an FP mistake at the
        // next round we reduce the inertia weight and add termination rules.
        //stateHandler.inertiaExpert.updateWeight(currentFluent, prediction)

        // I don't think so... this doesn't make much sense. Do nothing
      }

    } else if (is_FN_mistake(predictedLabel, feedback)) {
      outcome = "FN"
      if (!hedge) {
        increaseWeights(atom.initiatedBy, markedMap, learningRate)
        reduceWeights(atom.terminatedBy, markedMap, learningRate)

        // In Winnow, we also promote the inertia expert if it knows something for the current fluent
        if (inertiaExpertPrediction > 0.0) {
          val newWeight = inertiaExpertPrediction * Math.pow(Math.E, 1.0 * learningRate)
          //newWeight = if (newWeight.isPosInfinity) stateHandler.inertiaExpert.getWeight(currentFluent) else newWeight
          stateHandler.inertiaExpert.updateWeight(currentFluent, newWeight)
        }

      } else {
        reduceWeights(atom.terminatedBy, markedMap, learningRate, "hedge")
        // In Hedge, even if inertia predicts here it is correct, so we leave its weight unchanged.
      }

    } else { // TN
      outcome = "TN"
      stateHandler.inertiaExpert.forget(currentFluent)
      if (hedge) reduceWeights(atom.initiatedBy, markedMap, learningRate, "hedge")

    }

    updateScore(outcome)

    if (hedge) {
      // Re-normalize weights of awake experts

      val totalInitWeightPrevious = initRulesMap.map(x => x._2._2).sum
      val totalTermWeightPrevious = termRulesMap.map(x => x._2._2).sum

      val initWeightsAfterUpdatesMap = awakeInitRules.map(x => (x.##, x.w_pos))
      val termWeightsAfterUpdatesMap = awakeTermRules.map(x => (x.##, x.w_pos))
      val allRulesWeightsAfterUpdatesMap = (initWeightsAfterUpdatesMap ++ termWeightsAfterUpdatesMap).toMap

      val totalInitWeightAfterWeightsUpdate = getTotalWeight(awakeInitRules) // the updates have already taken place
      val totalTermWeightAfterWeightsUpdate  = getTotalWeight(awakeTermRules) // the updates have already taken place

      val inertAfterWeightUpdate = stateHandler.inertiaExpert.getWeight(currentFluent)
      val totalWeightAfterUpdate =
        if (withInertia) {
          inertAfterWeightUpdate + totalInitWeightAfterWeightsUpdate + totalTermWeightAfterWeightsUpdate
        } else {
          totalInitWeightAfterWeightsUpdate + totalTermWeightAfterWeightsUpdate
        }

      val mult = totalWeightBeforeUpdate/totalWeightAfterUpdate

      val updateWeight = (rule: Clause, y: Double) => rule.w_pos = y

      if (!mult.isNaN) {
        awakeInitRules.foreach(x =>  updateWeight(x, mult * x.w_pos ) )
        awakeTermRules.foreach(x =>  updateWeight(x, mult * x.w_pos ) )
        if (stateHandler.inertiaExpert.knowsAbout(currentFluent)) {
          stateHandler.inertiaExpert.updateWeight(currentFluent, mult * inertAfterWeightUpdate)
        }
      }

      // after normalization
      val totalInitWeightAfterNormalization = getTotalWeight(awakeInitRules)
      val totalTermWeightAfterNormalization = getTotalWeight(awakeTermRules)
      val inertAfterNormalization = stateHandler.inertiaExpert.getWeight(currentFluent)

      val totalEnsembleWeightBefore = totalWeightBeforeUpdate
      val totalEnsembleWeightAfterUpdates =
        if (withInertia) {
          totalInitWeightAfterWeightsUpdate + totalTermWeightAfterWeightsUpdate + inertAfterWeightUpdate
        } else {
          totalTermWeightAfterWeightsUpdate + inertAfterWeightUpdate
        }

      val totalEnsembleWeightAfterNormalization =
        if (withInertia) {
          totalInitWeightAfterNormalization + totalTermWeightAfterNormalization + inertAfterNormalization
        } else {
          totalTermWeightAfterNormalization + inertAfterNormalization
        }

      // This is wrong. The total AWAKE weight of the ensemble is supposed to remain the same.
      // Differences are due to number precision of Doubles. It's the total initiation or termination
      // weight that is the key here. If the total initiation weight does not drop after an FP,
      // generate new termination rules. Similarly, if the total termination weight does not drop
      // after an FN, generate new initiation rules.
      //if (totalEnsembleWeightAfter - totalEnsembleWeightBefore <= Math.pow(10,-4)) generateNewRule = true

      if (outcome == "FP" && totalInitWeightAfterNormalization >= totalInitWeightPrevious) generateNewRule = true
      if (outcome == "FN" && totalTermWeightAfterNormalization >= totalTermWeightPrevious) generateNewRule = true

      /* DEBUGGING INFO */
      def debuggingInfo(x: Vector[Clause], what: String) = {
        x foreach { rule =>
          val entry = if (what == "initiated") initRulesMap(rule.##) else termRulesMap(rule.##)
          println(s"weight prev/after update/after normalization: ${entry._2}/${allRulesWeightsAfterUpdatesMap(rule.##)}/${rule.w_pos} (tps,fps,fns): (${rule.tps},${rule.fps},${rule.fns})\n${entry._1}")
        }
      }

      if (outcome == "FP") { //|| outcome == "FN"
        println("======================================================================")
        println(s"prediction: $prediction, actual: $outcome, fluent: $currentFluent")
        if (withInertia) {
          println(s"Inertia before|after weights update|after normalization: $inertiaExpertPrediction|$inertAfterWeightUpdate|$inertAfterNormalization")
        }
        println(s"Total init before|after weights update & normalization: $totalInitWeightPrevious|$totalInitWeightAfterWeightsUpdate|$totalInitWeightAfterNormalization")
        println(s"Total term before|after weights update & normalization: $totalTermWeightPrevious|$totalTermWeightAfterWeightsUpdate|$totalTermWeightAfterNormalization")
        //println("AWAKE INIT:")
        //debuggingInfo(awakeInitRules, "initiated")
        //println("AWAKE TERM:")
        //debuggingInfo(awakeTermRules, "terminated")
        println(s"total weight before/after updates/normalization: $totalEnsembleWeightBefore/$totalEnsembleWeightAfterUpdates/$totalEnsembleWeightAfterNormalization equal: ${totalEnsembleWeightBefore == totalEnsembleWeightAfterNormalization}")
        println("======================================================================")
      }



      if (outcome == "TP") { // || outcome == "FP" // should we do this for FP as well (remember the fluent)? NO! MESSES THINGS UP. Generates wrong termination rules
        // If we recognized the fluent successfully during at this round, remember it
        if (!stateHandler.inertiaExpert.knowsAbout(currentFluent)) {
          stateHandler.inertiaExpert.updateWeight(currentFluent, prediction)
        }
      }
    }
    generateNewRule
  }










  def updateStructure_NEW(atom: AtomTobePredicted,
                          markedMap: Map[String, Clause],
                          predictedLabel: String,
                          feedback: String,
                          batch: Example,
                          currentAtom: String,
                          inps: RunningOptions,
                          logger: org.slf4j.Logger,
                          stateHandler: StateHandler,
                          percentOfMistakesBeforeSpecialize: Int,
                          randomizedPrediction: Boolean,
                          selected: String,
                          specializeAllAwakeOnMistake: Boolean,
                          conservativeRuleGeneration: Boolean) = {

    def getAwakeBottomRules(what: String) = {
      if (what == "initiatedAt") atom.initiatedBy.filter(x => markedMap(x).isBottomRule)
      else atom.terminatedBy.filter(x => markedMap(x).isBottomRule)
    }

    def splitAwakeAsleep(rulesToSplit: List[Clause], awakeIds: Set[String]) = {
      val rulesToSplitIds = rulesToSplit.map(_##).toSet
      val (topLevelAwakeRules, topLevelAsleepRules) = rulesToSplit.foldLeft(Vector.empty[Clause], Vector.empty[Clause]) { (x, rule) =>
        val isAwake = awakeIds.contains(rule.##.toString)
        val isTopLevel = rulesToSplitIds.contains(rule.##)
        if (isAwake) if (isTopLevel) (x._1 :+ rule, x._2) else (x._1, x._2) // then it's a refinement rule
        else if (isTopLevel) (x._1, x._2 :+ rule) else (x._1, x._2) // then it's a refinement rule
      }
      (topLevelAwakeRules, topLevelAsleepRules)
    }

    var updatedStructure = false

    if (is_FP_mistake(predictedLabel, feedback)) {
      val awakeBottomRules = getAwakeBottomRules("terminatedAt")
      // We don't have firing termination rules so we'll try to generate one.
      // If we're in conservative mode, we generate new rules only if none awake currently exists
      // Also, we are always conservative with termination rules. We generate new ones only if the FP
      // holds by inertia. Otherwise it doesn't make much sense.
      if (atom.terminatedBy.isEmpty) {
        // If we leave the if (stateHandler.inertiaExpert.knowsAbout(atom.fluent)) clause here
        // we get many more mistakes. On the other hand, it seems more reasonable to generate
        // termination rules only when the fluent holds by inertia... (don't know what to do)
        if (stateHandler.inertiaExpert.knowsAbout(atom.fluent)) {
          updatedStructure = generateNewRule(batch, currentAtom, inps, "FP", logger, stateHandler, "terminatedAt", 1.0)
        }
      } else {
        if (!conservativeRuleGeneration) {
          // If we are not in conservative mode we try to generate new termination rules even if awake termination
          // rules already exist. We only do so if the current example has not already been compressed into an existing
          // bottom rule.
          if (awakeBottomRules.isEmpty && stateHandler.inertiaExpert.knowsAbout(atom.fluent)) {
            updatedStructure = generateNewRule_1(batch, currentAtom, inps, logger, stateHandler, "terminatedAt", 1.0)
          }
        }
      }
      // Also, in the case of an FP mistake we try to specialize awake initiation rules.
      if (atom.initiatedBy.nonEmpty) {
        // We are doing this after each batch
        /*
        val (topLevelAwakeRules, topLevelAsleepRules) = splitAwakeAsleep(stateHandler.ensemble.initiationRules, atom.initiatedBy.toSet)
        val expandedInit = SingleCoreOLEDFunctions.
          expandRules(Theory(topLevelAwakeRules.toList.filter(x => x.refinements.nonEmpty)), inps, logger)
        if (expandedInit._2) {
          stateHandler.ensemble.initiationRules = expandedInit._1.clauses ++ topLevelAsleepRules
          updatedStructure = true
        }
        */
      }
    }

    if (is_FN_mistake(predictedLabel, feedback)) {
      val awakeBottomRules = getAwakeBottomRules("initiatedAt")
      if (atom.initiatedBy.isEmpty) {
        // We don't have firing initiation rules. Generate one.
        updatedStructure = generateNewRule(batch, currentAtom, inps, "FN", logger, stateHandler, "initiatedAt", 1.0)
      } else {
        if (!conservativeRuleGeneration) {
          // If we are not in conservative mode we try to generate new initiation rules even if awake initiation
          // rules already exist. We only do so if the current example has not already been compressed into an existing
          // bottom rule.
          if (awakeBottomRules.isEmpty) {
            updatedStructure = generateNewRule_1(batch, currentAtom, inps, logger, stateHandler, "initiatedAt", 1.0)
          }
        }
      }
      // Also, in the case of an FP mistake we try to specialize awake termination rules.
      if (atom.terminatedBy.nonEmpty) {
        // We are doing this after each batch
        /*
        val (topLevelAwakeRules, topLevelAsleepRules) = splitAwakeAsleep(stateHandler.ensemble.terminationRules, atom.terminatedBy.toSet)
        val expandedInit = SingleCoreOLEDFunctions.
          expandRules(Theory(topLevelAwakeRules.toList.filter(x => x.refinements.nonEmpty)), inps, logger)
        if (expandedInit._2) {
          stateHandler.ensemble.terminationRules = expandedInit._1.clauses ++ topLevelAsleepRules
          updatedStructure = true
        }
        */
      }
    }
    updatedStructure
  }












  def updateStructure(atom: AtomTobePredicted, markedMap: Map[String, Clause],
                      predictedLabel: String, feedback: String, batch: Example,
                      currentAtom: String, inps: RunningOptions,
                      logger: org.slf4j.Logger, stateHandler: StateHandler,
                      percentOfMistakesBeforeSpecialize: Int, randomizedPrediction: Boolean,
                      selected: String, specializeAllAwakeOnMistake: Boolean) = {

    if (is_FP_mistake(predictedLabel, feedback)) {
      if (atom.terminatedBy.isEmpty) {
        // We don't have firing termination rules. Generate one.
        generateNewRule(batch, currentAtom, inps, "FP", logger, stateHandler, "terminatedAt", 1.0)

      } else {
        // We do have firing termination rules.
        // Specialize initiation rules.
        if (atom.initiatedBy.nonEmpty) {

          // This is for performing Hoeffding tests for awake initiation rules on FP mistakes, considering as
          // specialization candidates only the sleeping refinements (which can fix the current mistake).
          // It doesn't make much sense. Why specialize only on mistake?
          /*
          val (topLevelAwakeRules, topLevelAsleepRules) = stateHandler.ensemble.initiationRules.foldLeft(Vector.empty[Clause], Vector.empty[Clause]) { (x, rule) =>
            val isAwake = atom.initiatedBy.toSet.contains(rule.##.toString)
            val isTopLevel = stateHandler.ensemble.initiationRules.map(_##).toSet.contains(rule.##)

            if (isAwake) {
              if (isTopLevel) {
                (x._1 :+ rule, x._2)
              } else {
                (x._1, x._2) // then it's a refinement rule
              }
            } else {
              if (isTopLevel) {
                (x._1, x._2 :+ rule)
              } else {
                (x._1, x._2) // then it's a refinement rule
              }
            }
          }
          val specializationCandidates = topLevelAwakeRules

          val expanded = SingleCoreOLEDFunctions.expandRules(Theory(specializationCandidates.toList), inps, logger)
          val break_? = expanded._2
          if (break_?) {
            stateHandler.ensemble.initiationRules = topLevelAsleepRules.toList ++ expanded._1.clauses
            break
          }
          */


          // This is for expanding to a sleeping refinement immediately after an FP mistake. No sense in doing that.
          ///*
          val break_? = specialize(atom, stateHandler, markedMap, "initiated", inps,
            logger, percentOfMistakesBeforeSpecialize, "FP", randomizedPrediction, selected, specializeAllAwakeOnMistake)
          if (break_?) break
          //*/
        }
      }
    }
    if (is_FN_mistake(predictedLabel, feedback)) {
      if (atom.initiatedBy.isEmpty) {
        // We don't have firing initiation rules. Generate one.
        generateNewRule(batch, currentAtom, inps, "FN", logger, stateHandler, "initiatedAt", 1.0)

      } else {
        // We do have firing initiation rules.
        // Specialize termination rules.
        if (atom.terminatedBy.nonEmpty) {

          // This is for performing Hoeffding tests for awake termination rules  on FN mistakes, considering as
          // specialization candidates only the sleeping refinements (which can fix the current mistake).
          // It doesn't make much sense. Why specialize only on mistake?
          /*
          val (topLevelAwakeRules, topLevelAsleepRules) = stateHandler.ensemble.terminationRules.foldLeft(Vector.empty[Clause], Vector.empty[Clause]) { (x, rule) =>
            val isAwake = atom.terminatedBy.toSet.contains(rule.##.toString)
            val isTopLevel = stateHandler.ensemble.terminationRules.map(_##).toSet.contains(rule.##)

            if (isAwake) {
              if (isTopLevel) {
                (x._1 :+ rule, x._2)
              } else {
                (x._1, x._2) // then it's a refinement rule
              }
            } else {
              if (isTopLevel) {
                (x._1, x._2 :+ rule)
              } else {
                (x._1, x._2) // then it's a refinement rule
              }
            }
          }
          val specializationCandidates = topLevelAwakeRules
          val expanded = SingleCoreOLEDFunctions.expandRules(Theory(specializationCandidates.toList), inps, logger)
          val break_? = expanded._2
          if (break_?) {
            stateHandler.ensemble.terminationRules = topLevelAsleepRules.toList ++ expanded._1.clauses
            break
          }
          */

          // This is for expanding to a sleeping refinement immediately after an FP mistake. No sense in doing that.
          ///*
          val break_? = specialize(atom, stateHandler, markedMap, "terminated", inps,
            logger, percentOfMistakesBeforeSpecialize, "FN", randomizedPrediction, selected, specializeAllAwakeOnMistake)
          if (break_?) break
          //*/
        }
      }
    }
  }






  def updateStructure_STRONGLY_INIT(atom: AtomTobePredicted, markedMap: Map[String, Clause],
                      predictedLabel: String, feedback: String, batch: Example,
                      currentAtom: String, inps: RunningOptions,
                      logger: org.slf4j.Logger, stateHandler: StateHandler,
                      percentOfMistakesBeforeSpecialize: Int, randomizedPrediction: Boolean,
                      selected: String, specializeAllAwakeOnMistake: Boolean) = {

    if (is_FP_mistake(predictedLabel, feedback)) {
      // For an FP mistake we have the following cases:
      // 1. holdsAt wins because there are no awake termination rules (so we have awake initiation rules and/or inertia). Then:
      // 1.1 If the fluent already holds by inertia, generate new termination expert tree
      //     (it's hopeless to expect to fix the mistake by down-weighting initiation rules,
      //     since we need to terminate the fluent for it to be "forgotten" by the inertia expert).
      // 1.2. If the fluent does not already hold by inertia, then specialize an existing initiation rule (or all???). With that we remove
      //      an awake initiation expert, thus hoping to prevent this scenario from happening again in the future.
      //      NOTE: For this to work it is necessary for the inertia expert to remember a fluent only in the case of a TP
      //      (NOT an FP, otherwise we get back the problem of 1.1)
      // 2. holdsAt wins because the awake termination rules are out-scored. In that case let the weight demotion
      //    (for the awake init. rules and the inertia expert) and the weight promotion (for the awake term. rules) do the job.
      if (atom.terminatedBy.isEmpty) {
        if (stateHandler.inertiaExpert.knowsAbout(atom.fluent)) { // this is 1.1 from above
          generateNewRule(batch, currentAtom, inps, "FP", logger, stateHandler, "terminatedAt", 1.0)
        } else { // this is 1.2 from above.
          val break_? = specialize(atom, stateHandler, markedMap, "initiated", inps,
            logger, percentOfMistakesBeforeSpecialize, "FP", randomizedPrediction, selected, specializeAllAwakeOnMistake)
          if (break_?) break
        }

      } else { // this is 2 from above
        // We do have firing termination rules.
        // Specialize initiation rules.
        /*
        if (atom.initiatedBy.nonEmpty) {
          val break_? = specialize(atom, stateHandler, markedMap, "initiated", inps,
            logger, percentOfMistakesBeforeSpecialize, "FP", randomizedPrediction, selected, specializeAllAwakeOnMistake)
          if (break_?) break
        }
        */
      }
    }

    if (is_FN_mistake(predictedLabel, feedback)) {
      if (atom.initiatedBy.isEmpty) {
        // We don't have firing initiation rules. Generate one, if it does not hold by inertia.
        if (!stateHandler.inertiaExpert.knowsAbout(atom.fluent)) {
          generateNewRule(batch, currentAtom, inps, "FN", logger, stateHandler, "initiatedAt", 1.0)
        } else {
          // it holds by inertia, let the weights fix the problem
          val break_? = specialize(atom, stateHandler, markedMap, "terminated", inps,
            logger, percentOfMistakesBeforeSpecialize, "FN", randomizedPrediction, selected, specializeAllAwakeOnMistake)
          if (break_?) break
        }


      } else {
        // We do have firing initiation rules.
        // Specialize termination rules.
        ///*
        if (atom.terminatedBy.nonEmpty) {
          val break_? = specialize(atom, stateHandler, markedMap, "terminated", inps,
            logger, percentOfMistakesBeforeSpecialize, "FN", randomizedPrediction, selected, specializeAllAwakeOnMistake)
          if (break_?) break
        }
        //*/
      }
    }
  }


  def generateNewExpert_NEW(batch: Example, currentAtom: AtomTobePredicted, previousTimePoint: Int,
                            inps: RunningOptions, mistakeType: String, logger: org.slf4j.Logger,
                            stateHandler: StateHandler, what: String, totalWeight: Double,
                            removePastExperts: Boolean = false, otherAwakeExperts: Vector[Clause] = Vector.empty[Clause]) = {

    def isRedundant(newRule: Clause) = {
      val getAllBottomRules = (x: List[Clause]) => x.flatMap(y => y.supportSet.clauses)
      val allBottomRules = {
        if (newRule.head.functor.contains("initiated")) getAllBottomRules(stateHandler.ensemble.initiationRules)
        else getAllBottomRules(stateHandler.ensemble.terminationRules)
      }
      allBottomRules.exists(c => newRule.thetaSubsumes(c))
    }
    var generatedRule = false

    val newRule = {
      val headAtom = s"$what(${currentAtom.fluent},$previousTimePoint)"
      val xhailInput = Map("annotation" -> batch.annotationASP, "narrative" -> batch.narrativeASP)
      val bkFile = if (what == "initiatedAt") inps.globals.BK_INITIATED_ONLY else inps.globals.BK_TERMINATED_ONLY
      val aspFile: File = Utils.getTempFile("aspinput", ".lp")
      val (_, bcs) = Xhail.generateKernel(List(headAtom), examples = xhailInput, aspInputFile = aspFile, bkFile=bkFile, globals = inps.globals)

      aspFile.delete()

      val _bottomClause = bcs.head

      val topRule = {
        val c = Clause(head = _bottomClause.head, body = List())
        c.addToSupport(_bottomClause)
        c
      }

      val _newRule = {
        if (bcs.isEmpty) {
          Clause.empty
        } else {
          // newRule here is an empty-bodied rule along with the newly-generated bottom clause.
          // Populate the newRule's refinements.
          topRule.generateCandidateRefs(inps.globals, otherAwakeExperts)
          topRule.w_pos = totalWeight
          topRule.refinements.foreach(x => x.w_pos = totalWeight)
          topRule
        }
      }
      _newRule
    }

    if (!newRule.equals(Clause.empty)) {
      logger.info(s"Generated new $what rule in response to $mistakeType atom: ${currentAtom.atom}")
      //=========================================
      // Store the new rule in the state handler
      stateHandler.addRule(newRule)
      //=========================================
      generatedRule = true
    } else {
      logger.info(s"At batch ${stateHandler.batchCounter}: Failed to generate bottom rule from $mistakeType mistake with atom: $currentAtom")
    }
    generatedRule
  }


  def generateNewRule(batch: Example, currentAtom: String, inps: RunningOptions, mistakeType: String,
                      logger: org.slf4j.Logger, stateHandler: StateHandler,
                      what: String, totalWeight: Double, removePastExperts: Boolean = false,
                      otherAwakeExperts: Vector[Clause] = Vector.empty[Clause]) = {

    def isRedundant(newRule: Clause) = {
      val getAllBottomRules = (x: List[Clause]) => x.flatMap(y => y.supportSet.clauses)
      val allBottomRules = {
        if (newRule.head.functor.contains("initiated")) getAllBottomRules(stateHandler.ensemble.initiationRules)
        else getAllBottomRules(stateHandler.ensemble.terminationRules)
      }
      allBottomRules.exists(c => newRule.thetaSubsumes(c))
    }
    var generatedRule = false
    val newRule = generateNewExpert(batch, currentAtom, inps.globals, what, totalWeight, otherAwakeExperts)
    //if (!isRedundant(newRule)) {
      if (!newRule.equals(Clause.empty)) {
        logger.info(s"Generated new $what rule in response to $mistakeType atom: $currentAtom")
        stateHandler.addRule(newRule)
        generatedRule = true
      } else {
        logger.info(s"At batch ${stateHandler.batchCounter}: Failed to generate bottom rule from $mistakeType mistake with atom: $currentAtom")
      }
    //} else {
      //logger.info(s"At batch ${stateHandler.batchCounter}: Dropped redundant bottom rule.")
    //}

    generatedRule
  }

  def generateNewRule_1(batch: Example, currentAtom: String, inps: RunningOptions,
                      logger: org.slf4j.Logger, stateHandler: StateHandler,
                      what: String, totalWeight: Double, removePastExperts: Boolean = false) = {

    var generatedNewRule = false
    val newRule = generateNewExpert(batch, currentAtom, inps.globals, what, totalWeight)
    if (!newRule.equals(Clause.empty)) {
      logger.info(s"Generated new $what rule from atom: $currentAtom")
      stateHandler.addRule(newRule)
      generatedNewRule = true
    } else {
      logger.info(s"At batch ${stateHandler.batchCounter}: Failed to generate bottom rule from atom: $currentAtom")
    }
    generatedNewRule
  }

  def specialize(atom: AtomTobePredicted, stateHandler: StateHandler,
                 markedMap: Map[String, Clause], what: String, inps: RunningOptions,
                 logger: org.slf4j.Logger, percentOfMistakesBeforeSpecialize: Int,
                 mistakeType: String, randomizedPrediction: Boolean, selected: String, specializeAllAwakeOnMistake: Boolean) = {

    val topLevelRules = if (what == "initiated") stateHandler.ensemble.initiationRules else stateHandler.ensemble.terminationRules
    val awakeExperts = if (what == "initiated") atom.initiatedBy else atom.terminatedBy // This contains the refinements

    // This contains the refinements
    val nonFiringRules =
      markedMap.filter(x =>
        x._2.head.functor.contains(what) && !awakeExperts.toSet.contains(x._1))

    def getSleepingChildren(ruleToSpecialize: Clause) = {
      ruleToSpecialize.refinements.filter(r => nonFiringRules.keySet.contains(r.##.toString)).
        //filter(s => s.score > ruleToSpecialize.score).
        //filter(r => !allRules.exists(r1 => r1.thetaSubsumes(r) && r.thetaSubsumes(r1))).
        sortBy { x => (- x.w_pos, - x.score, x.body.length+1) }
    }

    def performSpecialization(ruleToSpecialize: (Clause, List[Clause])) = {
      val suitableRefs = ruleToSpecialize._2
      val bestRefinement = suitableRefs.head
      /*
      if (bestRefinement.w > ruleToSpecialize._1.w) {
        if (bestRefinement.refinements.isEmpty) bestRefinement.generateCandidateRefs(inps.globals)
        showInfo(ruleToSpecialize._1, bestRefinement, atom.atom, mistakeType)
        stateHandler.removeRule(ruleToSpecialize._1)
        stateHandler.addRule(bestRefinement)
      }
      */
      if (bestRefinement.refinements.isEmpty) bestRefinement.generateCandidateRefs(inps.globals)
      showInfo(ruleToSpecialize._1, bestRefinement, atom.atom, mistakeType)
      stateHandler.removeRule(ruleToSpecialize._1)
      stateHandler.addRule(bestRefinement)
    }

    val allIncorrectAwakeTopLevelRules = {
      topLevelRules.filter{ x =>
        val totalFPs = stateHandler.totalFPs
        val specialize = x.fps >= totalFPs * (percentOfMistakesBeforeSpecialize.toDouble/100)
        awakeExperts.toSet.contains(x.##.toString) && specialize
      }
    }

    val goAheads = allIncorrectAwakeTopLevelRules.map(x => (x, getSleepingChildren(x)) ).filter(x => x._2.nonEmpty)

    var performedSpecialization = false

    if (goAheads.nonEmpty) {
      if (specializeAllAwakeOnMistake) {
        goAheads foreach { ruleToSpecialize =>
          performedSpecialization = true
          performSpecialization(ruleToSpecialize)
        }
      } else {
        performedSpecialization = true
        // Pick only one at random
        val shuffled = scala.util.Random.shuffle(goAheads)
        if (!randomizedPrediction) {
          performSpecialization(shuffled.head)
        } else {
          performedSpecialization = true
          // Try to specialize the rule we predicted with, if not possible, specialize one at random
          val rule = goAheads.find(p => p._1.##.toString == selected) match {
            case Some(x) => x
            case _ => shuffled.head
          }
          performSpecialization(rule)
        }
      }
    } else {
      //logger.info("Could not perform specialization (no sleeping children found).")
    }

    performedSpecialization
  }

  def is_FP_mistake(predictedLabel: String, feedback: String) = {
    predictedLabel == "true" && feedback == "false"
  }

  def is_FN_mistake(predictedLabel: String, feedback: String) = {
    predictedLabel == "false" && feedback == "true"
  }

  def is_TP(predictedLabel: String, feedback: String) = {
    predictedLabel == "true" && feedback == "true"
  }

  def is_TN(predictedLabel: String, feedback: String) = {
    predictedLabel == "false" && feedback == "false"
  }

  def getFeedback(a: AtomTobePredicted,
                  predictions: Map[String, Double],
                  splice: Option[Map[String, Double] => (Set[EvidenceAtom], Evidence)] = None,
                  mapper: Option[Set[EvidenceAtom] => Vector[String]] = None,
                  labels: Set[String] = Set[String]()) = {
    // The prediction is sent to Splice here to receive the feedback.
    // If the atom is labelled Slice will return its true label, otherwise it will send
    // a Splice-predicted label for this atom.
    // To work in a stand-alone fashion (without Splice) and also test it we also use
    // the true labels here.
    if (splice.isDefined && mapper.isDefined) {
      val result = splice.get(predictions)
      ////
      val querySig = AtomSignature("Meet", 3)
      val atomDB = result._2.db(querySig)
      val atomIDF = atomDB.identity

      val seq = atomIDF.indices.flatMap { id =>
        atomIDF.decode(id) match {
          case Success(terms) if atomDB(id) == TRUE =>
            Some(EvidenceAtom.asTrue(s"${querySig.symbol}", terms.map(lomrf.logic.Constant).toVector))
          case Failure(exception) => throw exception
          case _                  => None
        }
      }
      /////
      val spliceTrueLabels = mapper.get(seq.toSet)
      val predictedAtomTruthValue = spliceTrueLabels.contains(a.atom).toString
      predictedAtomTruthValue
    } else {
      if (labels.contains(a.atom)) "true" else "false"
    }
  }


  private def showInfo(parent: Clause, child: Clause, currentAtom: String, mistakeType: String) = {

    logger.info(s"\nSpecialization in response to $mistakeType atom $currentAtom:\nRule (id: ${parent.##} | " +
      s"score: ${parent.score} | tps: ${parent.tps} fps: ${parent.fps} " +
      s"fns: ${parent.fns} | ExpertWeight: ${parent.w_pos} " +
      s"AvgExpertWeight: ${parent.avgWeight})\n${parent.tostring}\nwas refined to" +
      s"(id: ${child.##} | score: ${child.score} | tps: ${child.tps} fps: ${child.fps} fns: ${child.fns} | " +
      s"ExpertWeight: ${child.w_pos} AvgExpertWeight: ${child.avgWeight})\n${child.tostring}")

  }


  def ground(batch: Example,
             inps: RunningOptions,
             stateHandler: StateHandler,
             withInputTheory: Boolean = false,
             streaming: Boolean = false) = {
    val startTime = System.nanoTime()
    val (markedProgram, markedMap, groundingsMap, times) = groundEnsemble(batch, inps, stateHandler, withInputTheory, streaming)
    val sortedAtomsToBePredicted = sortGroundingsByTime(groundingsMap)
    val orderedTimes = (sortedAtomsToBePredicted.map(x => x.time) ++ times.toVector).sorted
    val endTime = System.nanoTime()
    println(s"Grounding time: ${(endTime - startTime)/1000000000.0}")
    (markedProgram, markedMap, groundingsMap, times, sortedAtomsToBePredicted, orderedTimes)
  }

  /* Generate groundings of the rules currently in the ensemble. */

  def groundEnsemble(batch: Example,
                     inps: RunningOptions,
                     stateHandler: StateHandler,
                     withInputTheory: Boolean = false,
                     streaming: Boolean = false) = {

    val ensemble = stateHandler.ensemble
    val merged = ensemble.merged(inps, withInputTheory)
    stateHandler.runningRulesNumber = stateHandler.runningRulesNumber :+ merged.size

    //println(s"Number of rules: ${merged.size}")
    //println(s"Predicting with:\n${merged.tostring}")

    val _marked = marked(merged.clauses.toVector, inps.globals)
    val markedProgram = _marked._1
    val markedMap = _marked._2

    //val e = (batch.annotationASP ++ batch.narrativeASP).mkString("\n")
    //val trueAtoms = batch.annotation.toSet

    //val trueAtoms = Set.empty[String]

    val e = batch.narrativeASP.mkString("\n")

    val groundingsMapTimed = Utils.time { computeRuleGroundings(inps, markedProgram, markedMap, e, streaming = streaming) }
    val groundingsMap = groundingsMapTimed._1._1
    val times = groundingsMapTimed._1._2
    val groundingsTime = groundingsMapTimed._2
    stateHandler.updateGrndsTimes(groundingsTime)

    (markedProgram, markedMap, groundingsMap, times)
  }


  // givenLabels are the real annotation given in the case of full supervision. sliceLabels are the labels
  // received by splice in case of partial supervision.
  def sortGroundingsByTime(groundingsMap: scala.collection.mutable.Map[String, (scala.Vector[String], scala.Vector[String])]//,
                           //givenLabels: Set[String] = Set[String](),
                           //sliceLabels: Map[String, String] = Map[String, String]()
                           ) = {

    val objs = groundingsMap.foldLeft(Vector[AtomTobePredicted]()) { (accum, mapEntry) =>
      val (atom, initBy, termBy) = (mapEntry._1, mapEntry._2._1, mapEntry._2._2)
      val parsed = Literal.parse(atom)
      val time = parsed.terms.tail.head.name.toInt
      val fluent = parsed.terms.head.tostring
      /*
      val label =
        if(sliceLabels.isEmpty) {
          if (givenLabels.contains(atom)) "true" else "false"
        } else {
          sliceLabels(atom)
        }
      */
      val obj = new AtomTobePredicted(atom, fluent, parsed, time, initBy, termBy)

      accum :+ obj
    }
    objs.sortBy(x => x.time)
  }


  /* Make a prediction on the current atom */
  def predict(a: AtomTobePredicted, stateHanlder: StateHandler, markedMap: Map[String, Clause]) = {

    val (currentAtom, currentTime, awakeInit, awakeTerm, currentFluent) = (a.atom, a.time, a.initiatedBy, a.terminatedBy, a.fluent)

    val inertiaExpertPrediction = stateHanlder.inertiaExpert.getWeight(currentFluent)

    val initWeightSum = if (awakeInit.nonEmpty) awakeInit.map(x => markedMap(x).w_pos).sum else 0.0
    val termWeightSum = if (awakeTerm.nonEmpty) awakeTerm.map(x => markedMap(x).w_pos).sum else 0.0
    val prediction = inertiaExpertPrediction + initWeightSum - termWeightSum
    //val prediction = initWeightSum - termWeightSum
    (prediction, inertiaExpertPrediction, initWeightSum, termWeightSum)


    /*
    val initWeightSum = if (awakeInit.nonEmpty) awakeInit.map(x => markedMap(x).w_pos).sum else 0.0
    val termWeightSum = if (awakeTerm.nonEmpty) awakeTerm.map(x => markedMap(x).w_pos).sum else 0.0
    // This is used when we have two sub-experts per rule, one predicting 'true' and one 'no'
    /*
    val prediction =
      if (termWeightSum > 0) { // then the termination part predicts 'yes' (so, termination)
        inertiaExpertPrediction + initWeightSum - termWeightSum
      } else {// then the termination part predicts 'no' (so, no termination)
        inertiaExpertPrediction + initWeightSum // just initiation should be taken into account here
      }
    */
    //val prediction = initWeightSum - termWeightSum
    (prediction, inertiaExpertPrediction, initWeightSum, termWeightSum)
    */
  }

  def predictHedge(a: AtomTobePredicted, stateHanlder: StateHandler, markedMap: Map[String, Clause], withInertia: Boolean = true) = {

    // Here we assume that initiation rules predict '1' and termination rules predict '0'.
    // The prediction is a number in [0,1] resulting from the weighted avegage of the experts predictions:
    // prediction = (Sum_{weight of awake init rules} + inertia_weight) / (Sum_{weight of awake term rules} + Sum_{weight of awake init rules} + inertia_weight)
    // if prediction > threshold (e.g. 0.5) then we predict holds, else false

    val (_, _, awakeInit, awakeTerm, currentFluent) = (a.atom, a.time, a.initiatedBy, a.terminatedBy, a.fluent)

    val inertiaExpertPrediction = stateHanlder.inertiaExpert.getWeight(currentFluent)

    //val initWeightSum = if (awakeInit.nonEmpty) awakeInit.map(x => markedMap(x).w_pos).sum else 0.0
    //val termWeightSum = if (awakeTerm.nonEmpty) awakeTerm.map(x => markedMap(x).w_pos).sum else 0.0

    val initWeightSum = if (awakeInit.nonEmpty) awakeInit.map(x => markedMap(x)).filter(x => x.body.nonEmpty).map(x => x.w_pos).sum else 0.0
    val termWeightSum = if (awakeTerm.nonEmpty) awakeTerm.map(x => markedMap(x)).filter(x => x.body.nonEmpty).map(x => x.w_pos).sum else 0.0

    val _prediction =
      if (withInertia) {
        (inertiaExpertPrediction + initWeightSum) / (inertiaExpertPrediction + initWeightSum + termWeightSum)
      } else {
        initWeightSum / (initWeightSum + termWeightSum)
      }

    //val _prediction = initWeightSum / (initWeightSum + termWeightSum)

    val prediction = if (_prediction.isNaN) 0.0 else _prediction


    (prediction, inertiaExpertPrediction, initWeightSum, termWeightSum)

  }




  def predict_NEW(a: AtomTobePredicted, stateHanlder: StateHandler, markedMap: Map[String, Clause]) = {

    val (awakeInit, awakeTerm, currentFluent) = (a.initiatedBy, a.terminatedBy, a.fluent)
    val inertiaExpertPrediction = stateHanlder.inertiaExpert.getWeight(currentFluent)

    if (awakeInit.isEmpty && awakeTerm.isEmpty && inertiaExpertPrediction == 0.0) {
      (0.0, 0.0, "None")
    } else {
      val bestInit = awakeInit.map(x => markedMap(x)).map(x => (x.##.toString, x.w_pos)).sortBy(x => -x._2)
      val bestTerm = awakeTerm.map(x => markedMap(x)).map(x => (x.##.toString, x.w_pos)).sortBy(x => -x._2)

      val nonEmprtyBodied = (awakeInit ++ awakeTerm).map(x => markedMap(x)).filter(_.body.nonEmpty)
      val awakeRuleExpertsWithWeights = nonEmprtyBodied.map(x => (x.##.toString, x.w_pos)).toMap
      val awakeExpertsWithWeights =
        if (inertiaExpertPrediction > 0) awakeRuleExpertsWithWeights + ("inertia" -> inertiaExpertPrediction)
        else awakeRuleExpertsWithWeights

      val totalWeight = awakeExpertsWithWeights.values.sum

      var v = Vector.empty[(String, Double)]
      if (bestInit.nonEmpty) v = v :+ bestInit.head
      if (bestTerm.nonEmpty) v = v :+ bestTerm.head

      val _sorted =
        if (inertiaExpertPrediction > 0) v :+ ("inertia" -> inertiaExpertPrediction)
        else v

      val pick = _sorted.sortBy(x => -x._2).head

      val selected = pick._1

      if (selected == "inertia") {
        // return
        stateHanlder.predictedWithInertia += 1
        (inertiaExpertPrediction, totalWeight, selected)
      } else {

        if (!markedMap.keySet.contains(selected)) {
          throw new RuntimeException(s"atom: ${a.atom}, selected: $selected")
        }

        val expert = markedMap(selected)
        // return
        if (expert.head.functor.contains("initiated")) {
          stateHanlder.predictedWithInitRule += 1
          (expert.w_pos, totalWeight, selected)
        } else {
          stateHanlder.predictedWithTermRule += 1
          (-expert.w_pos, totalWeight, selected)
        }
      }

    }
  }



  def predictRandomized(a: AtomTobePredicted, stateHanlder: StateHandler, markedMap: Map[String, Clause]) = {

    val (awakeInit, awakeTerm, currentFluent) = (a.initiatedBy, a.terminatedBy, a.fluent)
    val inertiaExpertPrediction = stateHanlder.inertiaExpert.getWeight(currentFluent)

    if (awakeInit.isEmpty && awakeTerm.isEmpty && inertiaExpertPrediction == 0.0) {
      (0.0, 0.0, "None")
    } else {


      val nonEmprtyBodied = (awakeInit ++ awakeTerm).map(x => markedMap(x)).filter(_.body.nonEmpty)
      val awakeRuleExpertsWithWeights = nonEmprtyBodied.map(x => (x.##.toString, x.w_pos)).toMap

      val awakeExpertsWithWeights =
        if (inertiaExpertPrediction > 0) awakeRuleExpertsWithWeights + ("inertia" -> inertiaExpertPrediction)
        else awakeRuleExpertsWithWeights


      val totalWeight = awakeExpertsWithWeights.values.sum

      // We need to pick an element according to the probability of w_i/totalAwakeWeight

      // ORDERING DOESN'T MATTER. IS THIS TRUE?
      val sorted = awakeExpertsWithWeights.toVector.map(x => (x._1, x._2/totalWeight.toDouble)).sortBy(x => x._2)
      //val sorted = awakeExpertsWithWeights.toVector.map(x => (x._1, x._2/totalWeight.toDouble))

      // Pick an element according to its probability:
      // 1) Generate a uniformly distributed random number.
      // 2) Iterate through the list until the cumulative probability of the visited elements is greater than the random number.
      val p = Math.random()
      var cummulativeProbability = 0.0

      var selected = ""
      breakable {
        for (i <- sorted) {
          cummulativeProbability += i._2
          if (p <= cummulativeProbability) {
            selected = i._1
            break
          }
        }
      }
      if (selected == "inertia") {
        // return
        stateHanlder.predictedWithInertia += 1
        (inertiaExpertPrediction, totalWeight, selected)
      } else {

        if (!markedMap.keySet.contains(selected)) {
          throw new RuntimeException(s"atom: ${a.atom}, selected: $selected")
        }

        val expert = markedMap(selected)
        // return
        if (expert.head.functor.contains("initiated")) {
          stateHanlder.predictedWithInitRule += 1
          (expert.w_pos, totalWeight, selected)
        } else {
          stateHanlder.predictedWithTermRule += 1
          (-expert.w_pos, totalWeight, selected)
        }
      }
    }
  }





}

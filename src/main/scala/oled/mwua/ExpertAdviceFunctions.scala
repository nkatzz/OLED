package oled.mwua

import java.io.{File, PrintWriter}

import scala.util.{Failure, Random, Success}
import app.runutils.RunningOptions
import com.typesafe.scalalogging.{LazyLogging, Logger}
import logic.{Clause, Constant, Literal, Theory}
import logic.Examples.Example
import lomrf.logic.{AtomSignature, EvidenceAtom, TRUE}
import lomrf.mln.model.Evidence
import oled.functions.SingleCoreOLEDFunctions
import oled.mwua.AuxFuncs._
import oled.mwua.HelperClasses.AtomTobePredicted
import utils.Utils

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
              splice: Option[Map[String, Double] => (Set[EvidenceAtom], Evidence)] = None,
              mapper: Option[Set[EvidenceAtom] => Vector[String]] = None,
              incompleteTrueAtoms: Option[Set[String]] = None,
              inputTheory: Option[List[Clause]] = None) = {

    var batchError = 0
    var batchFPs = 0
    var batchFNs = 0
    var batchAtoms = 0
    var finishedBatch = false

    if (batchCounter == 43) {
      val stop = "stop"
    }

    var spliceInput = Map.empty[String, Double]
    stateHandler.batchCounter = batchCounter

    val withInputTheory = inputTheory.isDefined

    var alreadyProcessedAtoms = Set.empty[String]
    val predictAndUpdateTimed = Utils.time {
      while(!finishedBatch) {
        val (markedProgram, markedMap, groundingsMap) = groundEnsemble(batch, inps, stateHandler, withInputTheory)
        val sortedAtomsToBePredicted = sortGroundingsByTime(groundingsMap)
        breakable {
          sortedAtomsToBePredicted foreach { atom =>
            val currentAtom = atom.atom
            if (!alreadyProcessedAtoms.contains(currentAtom)) {

              //logger.info(s"predicting with:\n${stateHandler.ensemble.merged(inps).tostring}")

              // this should be place at the end of the iteration. In this way, if
              // a break actuall occurs due to structure update we'll retrain on the
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
              if (!randomizedPrediction) {
                val (_prediction, _inertiaExpertPrediction, _initWeightSum, _termWeightSum) = predict(atom, stateHandler, markedMap)
                prediction = _prediction
                inertiaExpertPrediction = _inertiaExpertPrediction
                initWeightSum = _initWeightSum
                termWeightSum = _termWeightSum
              } else {
                val (_prediction_, _totalWeight, _selected) = predictRandomized(atom, stateHandler, markedMap)
                //val (_prediction_, _totalWeight, _selected) = predict_NEW(atom, stateHandler, markedMap)
                prediction = _prediction_
                totalWeight = _totalWeight
                selected = _selected
              }

              if(splice.isDefined) {
                val time = atom.atomParsed.terms.tail.head
                val eventAtom = atom.atomParsed.terms.head.asInstanceOf[Literal]
                val eventPred = eventAtom.functor.capitalize
                val eventArgs = eventAtom.terms.map(x => x.tostring.capitalize).mkString(",")
                val out = s"$eventPred($eventArgs,$time)"
                // Rescale the prediction to [0,1]. If I is the interval (range) of the prediction, then
                // rescaledPrediction = (prediction - min(I))/(max(I) - min(I)),
                // where min(I) = -termWeightSum
                // max(I) = initWeightSum + inertiaExpertPrediction
                val rescaledPrediction = (prediction - (-termWeightSum))/((initWeightSum + inertiaExpertPrediction) - (-termWeightSum))
                spliceInput += (out -> rescaledPrediction)
              }

              val feedback =
                if (incompleteTrueAtoms.isDefined) {
                  getFeedback(atom, spliceInput, splice, mapper, incompleteTrueAtoms.get)
                } else {
                  getFeedback(atom, spliceInput, splice, mapper, trueAtoms)
                }

              val predictedLabel = if (prediction > 0) "true" else "false"

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
                      logger.info(s"Inertia FP prediction for fluent ${atom.fluent}. Inertia weight: ${stateHandler.inertiaExpert.getWeight(atom.fluent)}")
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
                if (receiveFeedbackBias == 1.0) {
                  true
                } else {
                  val p = Math.random()
                  if (p <= receiveFeedbackBias) true else false
                }
              }

              if (update) {
                stateHandler.receivedFeedback += 1
                if (!randomizedPrediction) {
                  // Updates weights only after a mistake (in the TP case it just updates the inertia expert)
                  updateWeights(atom, prediction, inertiaExpertPrediction, initWeightSum,
                    termWeightSum, predictedLabel, markedMap, feedback, stateHandler, learningRate)
                  stateHandler.normalizeWeights( (atom.initiatedBy ++ atom.terminatedBy).map(x => markedMap(x)), atom.fluent )
                } else {
                  updateWeightsRandomized(atom, prediction, inertiaExpertPrediction,
                    predictedLabel, feedback, stateHandler, epsilon, markedMap, totalWeight)
                }

                if (predictedLabel != feedback) {
                  if (!withInputTheory) { // we only update weights when an input theory is given
                    // Shouldn't we update the weights on newly generated rules here? Of course it's just 1 example, no big deal, but still...

                    /*
                     updateStructure_STRONGLY_INIT(atom, markedMap, predictedLabel, feedback, batch,
                      currentAtom, inps, Logger(this.getClass).underlying, stateHandler,
                      percentOfMistakesBeforeSpecialize, randomizedPrediction, selected, specializeAllAwakeOnMistake)
                     */

                    val structureUpdate_? = updateStructure_NEW(atom, markedMap, predictedLabel, feedback, batch,
                      currentAtom, inps, Logger(this.getClass).underlying, stateHandler, percentOfMistakesBeforeSpecialize,
                      randomizedPrediction, selected, specializeAllAwakeOnMistake, conservativeRuleGeneration)

                    if (structureUpdate_?) break
                  }
                }
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

          // Try to specialize all rules currently in the ensemble
          // Just to be on the safe side filter out rules with no refinements
          // (i.e. rules that have "reached" their bottom rule).
          /*
          val expandedInit =
            SingleCoreOLEDFunctions.expandRules(Theory(stateHandler.ensemble.initiationRules.filter(x => x.refinements.nonEmpty)),
              inps, Logger(this.getClass).underlying)

          val expandedTerm =
            SingleCoreOLEDFunctions.expandRules(Theory(stateHandler.ensemble.terminationRules.filter(x => x.refinements.nonEmpty)),
              inps, Logger(this.getClass).underlying)

          stateHandler.ensemble.initiationRules = expandedInit._1.clauses
          stateHandler.ensemble.terminationRules = expandedTerm._1.clauses
          */
        }
      }
    }
    //logger.info(s"Batch processing time: ${predictAndUpdateTimed._2}")
    if (batchError > 0) {
      logger.info(s"*** Batch #$batchCounter Total mistakes: ${batchFPs+batchFNs} (FPs: $batchFPs | FNs: $batchFNs). Total batch atoms: $batchAtoms ***")
    }
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
        incorrectExperts.map(i => i.w/totalWeight.toDouble).sum
      } else {
        incorrectExperts.map(i => i.w/totalWeight.toDouble).sum + inertiaExpertPrediction/totalWeight.toDouble
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
        rule.w = weightNoInfinity(rule.w, rule.w * Math.pow(1+epsilon, correctExponent))
      }

      incorrectExperts foreach { x =>
        val rule = markedMap(x)
        rule.w = weightNoInfinity(rule.w, rule.w * Math.pow(1+epsilon, inCorrectExponent))
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

      //if (stateHandler.inertiaExpert.knowsAbout(atom.fluent)) stateHandler.inertiaExpert.forget(atom.fluent)

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
                    initWeightSum: Double, termWeightSum: Double, predictedLabel: String, markedMap: Map[String, Clause],
                    feedback: String, stateHandler: StateHandler, learningRate:Double) = {

    val currentFluent = atom.fluent

    val nonFiringInitRules =
      markedMap.filter(x =>
        x._2.head.functor.contains("initiated") && !atom.initiatedBy.toSet.contains(x._1))

    val nonFiringTermRules =
      markedMap.filter(x =>
        x._2.head.functor.contains("terminated") && !atom.terminatedBy.toSet.contains(x._1))

    if (is_TP(predictedLabel, feedback)) {
      val holdsWeight = inertiaExpertPrediction + initWeightSum
      // Either adds or updates the fluent
      stateHandler.inertiaExpert.updateWeight(currentFluent, holdsWeight)
      ///*
      updateRulesScore("TP", atom.initiatedBy.map(x => markedMap(x)), nonFiringInitRules.values.toVector,
          atom.terminatedBy.map(x => markedMap(x)), nonFiringTermRules.values.toVector)
      //*/

    }

    if (is_FP_mistake(predictedLabel, feedback)) {
      reduceWeights(atom.initiatedBy, markedMap, learningRate)
      increaseWeights(atom.terminatedBy, markedMap, learningRate)
      if (inertiaExpertPrediction > 0.0) {
        val newWeight = inertiaExpertPrediction * Math.pow(Math.E, (-1.0) * learningRate)
        stateHandler.inertiaExpert.updateWeight(currentFluent, newWeight)
      } else {
        // Since we predicted it as holding, we should add it to the inertia map
        // (???? does this make sense? Only for some sort of soft inertia for example, where
        // the actual label (FP) could be wrong, so we remember hoping to let the weights sort things out)
        //stateHandler.inertiaExpert.updateWeight(currentFluent, prediction)
      }
      ///*
      updateRulesScore("FP", atom.initiatedBy.map(x => markedMap(x)), nonFiringInitRules.values.toVector,
        atom.terminatedBy.map(x => markedMap(x)), nonFiringTermRules.values.toVector)
      //*/
    }

    if (is_FN_mistake(predictedLabel, feedback)) {
      increaseWeights(atom.initiatedBy, markedMap, learningRate)
      reduceWeights(atom.terminatedBy, markedMap, learningRate)
      if (inertiaExpertPrediction > 0.0) {
        val newWeight = inertiaExpertPrediction * Math.pow(Math.E, 1.0 * learningRate)
        //newWeight = if (newWeight.isPosInfinity) stateHandler.inertiaExpert.getWeight(currentFluent) else newWeight
        stateHandler.inertiaExpert.updateWeight(currentFluent, newWeight)
      } else {
        // This is because in response to the FN mistake we have generated an new initiation rule
        // (if the fluent does not hold by inertia -- see the updateStructure method). Therefore,
        // the new initiation rule initiates the fluent, so we add it to the inertia map.
        // Not doing so won't work in case the initiation conditions are not repeated exactly
        // in the next time point (after this FN), so as for the fluent to be re-initiated.
        /* THIS SHOULD BE HANDLED BY RETRAINING ON THE CURRENT EXAMPLE, SO AFTER A RULE GENERATION (INITIATION) THE fn TURNS INTO A TP */
        /*
        if (atom.initiatedBy.nonEmpty) {
          stateHandler.inertiaExpert.updateWeight(currentFluent, 1.0)
        }
        */

      }
      ///*
      updateRulesScore("FN", atom.initiatedBy.map(x => markedMap(x)), nonFiringInitRules.values.toVector,
        atom.terminatedBy.map(x => markedMap(x)), nonFiringTermRules.values.toVector)
      //*/
    }

    if (is_TN(predictedLabel, feedback)) {
      stateHandler.inertiaExpert.forget(currentFluent)
      ///*
      updateRulesScore("TN", atom.initiatedBy.map(x => markedMap(x)), nonFiringInitRules.values.toVector,
        atom.terminatedBy.map(x => markedMap(x)), nonFiringTermRules.values.toVector)
      //*/
    }

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
        ///*
        val (topLevelAwakeRules, topLevelAsleepRules) = splitAwakeAsleep(stateHandler.ensemble.initiationRules, atom.initiatedBy.toSet)
        //val expandedInit = SingleCoreOLEDFunctions.
        // expandRules(Theory((topLevelAwakeRules ++ topLevelAsleepRules).toList.filter(x => x.refinements.nonEmpty)), inps, logger)
        val expandedInit = SingleCoreOLEDFunctions.
          expandRules(Theory(topLevelAwakeRules.toList.filter(x => x.refinements.nonEmpty)), inps, logger)
        if (expandedInit._2) {
          stateHandler.ensemble.initiationRules = expandedInit._1.clauses ++ topLevelAsleepRules
          updatedStructure = true
        }
        //*/
        // This is for expanding to a sleeping refinement immediately after an FP mistake. No much sense in doing that.
        /*
        val break_? = specialize(atom, stateHandler, markedMap, "initiated", inps,
          logger, percentOfMistakesBeforeSpecialize, "FP", randomizedPrediction, selected, specializeAllAwakeOnMistake)
        if (break_?) break
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
        ///*
        val (topLevelAwakeRules, topLevelAsleepRules) = splitAwakeAsleep(stateHandler.ensemble.terminationRules, atom.terminatedBy.toSet)
        //val expandedInit = SingleCoreOLEDFunctions.
        // expandRules(Theory((topLevelAwakeRules ++ topLevelAsleepRules).toList.filter(x => x.refinements.nonEmpty)), inps, logger)
        val expandedInit = SingleCoreOLEDFunctions.
          expandRules(Theory(topLevelAwakeRules.toList.filter(x => x.refinements.nonEmpty)), inps, logger)
        if (expandedInit._2) {
          stateHandler.ensemble.terminationRules = expandedInit._1.clauses ++ topLevelAsleepRules
          updatedStructure = true
        }
        //*/
        // This is for expanding to a sleeping refinement immediately after an FP mistake. No much sense in doing that.
        /*
        val break_? = specialize(atom, stateHandler, markedMap, "terminated", inps,
            logger, percentOfMistakesBeforeSpecialize, "FN", randomizedPrediction, selected, specializeAllAwakeOnMistake)
        if (break_?) break
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

  def generateNewRule(batch: Example, currentAtom: String, inps: RunningOptions, mistakeType: String,
                      logger: org.slf4j.Logger, stateHandler: StateHandler,
                      what: String, totalWeight: Double, removePastExperts: Boolean = false) = {

    var generatedRule = false
    val newRule = generateNewExpert(batch, currentAtom, inps.globals, what, totalWeight)
    if (!newRule.equals(Clause.empty)) {
      logger.info(s"Generated new $what rule in response to $mistakeType atom: $currentAtom")
      stateHandler.addRule(newRule)
      generatedRule = true
    } else {
      logger.info(s"At batch ${stateHandler.batchCounter}: Failed to generate bottom rule from $mistakeType mistake with atom: $currentAtom")
    }
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
        sortBy { x => (- x.w, - x.score, x.body.length+1) }
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

    logger.info(s"\nSpecialization in response to $mistakeType atom $currentAtom:\nRule (id: ${parent.##} | score: ${parent.score} | tps: ${parent.tps} fps: ${parent.fps} " +
      s"fns: ${parent.fns} | ExpertWeight: ${parent.w} " +
      s"AvgExpertWeight: ${parent.avgWeight})\n${parent.tostring}\nwas refined to" +
      s"(id: ${child.##} | score: ${child.score} | tps: ${child.tps} fps: ${child.fps} fns: ${child.fns} | " +
      s"ExpertWeight: ${child.w} AvgExpertWeight: ${child.avgWeight})\n${child.tostring}")

  }


  /* Generate groundings of the rules currently in the ensemble. */

  def groundEnsemble(batch: Example,
                     inps: RunningOptions,
                     stateHandler: StateHandler,
                     withInputTheory: Boolean = false) = {

    val ensemble = stateHandler.ensemble
    val merged = ensemble.merged(inps, withInputTheory)

    //println(s"Predicting with:\n${merged.tostring}")

    val _marked = marked(merged.clauses.toVector, inps.globals)
    val markedProgram = _marked._1
    val markedMap = _marked._2

    //val e = (batch.annotationASP ++ batch.narrativeASP).mkString("\n")
    //val trueAtoms = batch.annotation.toSet

    //val trueAtoms = Set.empty[String]

    val e = batch.narrativeASP.mkString("\n")

    val groundingsMapTimed = Utils.time { computeRuleGroundings(inps, markedProgram, markedMap, e) }
    val groundingsMap = groundingsMapTimed._1
    val groundingsTime = groundingsMapTimed._2
    stateHandler.updateGrndsTimes(groundingsTime)

    (markedProgram, markedMap, groundingsMap)
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
    val initWeightSum = if (awakeInit.nonEmpty) awakeInit.map(x => markedMap(x).w).sum else 0.0
    val termWeightSum = if (awakeTerm.nonEmpty) awakeTerm.map(x => markedMap(x).w).sum else 0.0
    val prediction = inertiaExpertPrediction + initWeightSum - termWeightSum
    //val prediction = initWeightSum - termWeightSum
    (prediction, inertiaExpertPrediction, initWeightSum, termWeightSum)
  }




  def predict_NEW(a: AtomTobePredicted, stateHanlder: StateHandler, markedMap: Map[String, Clause]) = {

    val (awakeInit, awakeTerm, currentFluent) = (a.initiatedBy, a.terminatedBy, a.fluent)
    val inertiaExpertPrediction = stateHanlder.inertiaExpert.getWeight(currentFluent)

    if (awakeInit.isEmpty && awakeTerm.isEmpty && inertiaExpertPrediction == 0.0) {
      (0.0, 0.0, "None")
    } else {
      val bestInit = awakeInit.map(x => markedMap(x)).map(x => (x.##.toString, x.w)).sortBy(x => -x._2)
      val bestTerm = awakeTerm.map(x => markedMap(x)).map(x => (x.##.toString, x.w)).sortBy(x => -x._2)

      val nonEmprtyBodied = (awakeInit ++ awakeTerm).map(x => markedMap(x)).filter(_.body.nonEmpty)
      val awakeRuleExpertsWithWeights = nonEmprtyBodied.map(x => (x.##.toString, x.w)).toMap
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
          (expert.w, totalWeight, selected)
        } else {
          stateHanlder.predictedWithTermRule += 1
          (-expert.w, totalWeight, selected)
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
      val awakeRuleExpertsWithWeights = nonEmprtyBodied.map(x => (x.##.toString, x.w)).toMap

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
          (expert.w, totalWeight, selected)
        } else {
          stateHanlder.predictedWithTermRule += 1
          (-expert.w, totalWeight, selected)
        }
      }
    }
  }





}

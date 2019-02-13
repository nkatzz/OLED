package oled.mwua

import app.runutils.RunningOptions
import com.typesafe.scalalogging.{LazyLogging, Logger}
import logic.{Clause, Literal}
import logic.Examples.Example
import oled.mwua.AuxFuncs._
import oled.mwua.HelperClasses.AtomTobePredicted
import utils.Utils

import scala.util.control.Breaks._

object ExpertAdviceFunctions extends LazyLogging {



  // If withSplice is true then trueLabels is empty and we use splice to get a label.
  // Otherwise (if we're using this in a stand-alone fashion) trueLabels is the true holdsAt
  // atoms (and we're using CWA for the false ones).
  def process(withSplice: Boolean,
              batch: Example,
              inps: RunningOptions,
              stateHandler: StateHandler,
              trueLabels: Set[String],
              learningRate: Double,
              epsilon: Double, // used for the randomized version
              randomizedPrediction: Boolean,
              batchCounter: Int,
              percentOfMistakesBeforeSpecialize: Int,
              testHandCrafted: Boolean = false) = {

    if (batchCounter == 255) {
      val stop = "stop"
    }

    var batchError = 0
    var batchFPs = 0
    var batchFNs = 0
    var batchAtoms = 0

    var finishedBatch = false

    var alreadyProcessedAtoms = Set.empty[String]
    val predictAndUpdateTimed = Utils.time {
      while(!finishedBatch) {
        val (markedProgram, markedMap, groundingsMap) = groundEnsemble(batch, inps, stateHandler, testHandCrafted)
        val sortedAtomsToBePredicted = sortGroundingsByTime(groundingsMap)
        breakable {
          sortedAtomsToBePredicted foreach { atom =>
            val currentAtom = atom.atom
            if (!alreadyProcessedAtoms.contains(currentAtom)) {

              //println( (atom.initiatedBy++atom.terminatedBy).map(x => markedMap(x)).map(x => x.w) )

              alreadyProcessedAtoms = alreadyProcessedAtoms + currentAtom
              batchAtoms += 1
              //-------------------
              // MAKE A PREDICTION
              //-------------------
              var prediction = 0.0
              var inertiaExpertPrediction = 0.0
              var initWeightSum = 0.0
              var termWeightSum = 0.0
              var totalWeight = 0.0
              if (!randomizedPrediction) {
                val (_prediction, _inertiaExpertPrediction, _initWeightSum, _termWeightSum) = predict(atom, stateHandler, markedMap)
                prediction = _prediction
                inertiaExpertPrediction = _inertiaExpertPrediction
                initWeightSum = _initWeightSum
                termWeightSum = _termWeightSum
              } else {
                val (_prediction_, _totalWeight) = predictRandomized(atom, stateHandler, markedMap)
                prediction = _prediction_
                totalWeight = _totalWeight
              }

              val feedback = getFeedback(atom, withSplice, trueLabels)
              val predictedLabel = if (prediction > 0) "true" else "false"

              if (predictedLabel != feedback) {
                batchError += 1
                if (is_FP_mistake(predictedLabel, feedback)) {
                  batchFPs += 1
                  logger.info(s"FP mistake for atom $currentAtom")
                }
                if (is_FN_mistake(predictedLabel, feedback)) {
                  batchFNs += 1
                  logger.info(s"FN mistake for atom $currentAtom")
                }
              }

              if (!randomizedPrediction) {
                // Updates weights only after a mistake (in the TP case it just updates the inertia expert)
                updateWeights(atom, prediction, inertiaExpertPrediction, initWeightSum,
                  termWeightSum, predictedLabel, markedMap, feedback, stateHandler, learningRate)
                stateHandler.normalizeWeights( (atom.initiatedBy ++ atom.terminatedBy).map(x => markedMap(x)), atom.fluent )
              } else {
                updateWeightsRandomized(atom, prediction, inertiaExpertPrediction, predictedLabel,
                  feedback, stateHandler, epsilon, markedMap, totalWeight)
              }

              if (predictedLabel != feedback) {
                // Shouldn't we update the weights on newly generated rules here? Of course it's just 1 example, no big deal, but still...
                updateStructure(atom, markedMap, predictedLabel, feedback, batch, currentAtom,
                  inps, Logger(this.getClass).underlying, stateHandler, batchCounter, percentOfMistakesBeforeSpecialize)
              }
            }
          }
          finishedBatch = true
          stateHandler.perBatchError = stateHandler.perBatchError :+ batchError
        }
      }
    }
    //*/
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

    def getMistakeProbability(incorrectExperts: Vector[Clause]) = {
      // The algorithm's probability of making a mistake is the sum, for all awake
      // experts, of the each expert's h_i selection probability (h_i/totalAwakeWeight)
      // times 1 (if the expert is incorrect) or 0 (if the expert is correct). Since
      // correct experts do not contribute to the sum we only take into account the incorrect ones.
      incorrectExperts.map(i => i.w/totalWeight).sum
    }

    def updateRulesWeights(correctExperts: Vector[String], incorrectExperts: Vector[String], isInertiaCorrect: Boolean) = {

      val inc = incorrectExperts.map(x => markedMap(x))
      val mistakeProbability = getMistakeProbability(inc)
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
          stateHandler.inertiaExpert.updateWeight(atom.fluent, weightNoInfinity(inertiaExpertPrediction, inertiaExpertPrediction * Math.pow(1+epsilon, correctExponent)) )
        } else {
          stateHandler.inertiaExpert.updateWeight(atom.fluent, weightNoInfinity(inertiaExpertPrediction, inertiaExpertPrediction * Math.pow(1+epsilon, inCorrectExponent)) )
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

      // No weight updates here
      stateHandler.totalTPs = stateHandler.totalTPs + 1

      val holdsWeight = inertiaExpertPrediction + initWeightSum
      stateHandler.inertiaExpert.updateWeight(currentFluent, holdsWeight)

      ///*
      updateRulesScore("TP", atom.initiatedBy.map(x => markedMap(x)), nonFiringInitRules.values.toVector,
          atom.terminatedBy.map(x => markedMap(x)), nonFiringTermRules.values.toVector)
      //*/

    }

    if (is_FP_mistake(predictedLabel, feedback)) {

      stateHandler.totalFPs = stateHandler.totalFPs + 1

      reduceWeights(atom.initiatedBy, markedMap, learningRate)

      increaseWeights(atom.terminatedBy, markedMap, learningRate)

      if (inertiaExpertPrediction > 0.0) {
        val newWeight = inertiaExpertPrediction * Math.pow(Math.E, (-1.0) * learningRate)
        stateHandler.inertiaExpert.updateWeight(currentFluent, newWeight)
      }

      ///*
      updateRulesScore("FP", atom.initiatedBy.map(x => markedMap(x)), nonFiringInitRules.values.toVector,
        atom.terminatedBy.map(x => markedMap(x)), nonFiringTermRules.values.toVector)
      //*/
    }

    if (is_FN_mistake(predictedLabel, feedback)) {

      stateHandler.totalFNs = stateHandler.totalFNs + 1

      increaseWeights(atom.initiatedBy, markedMap, learningRate)

      reduceWeights(atom.terminatedBy, markedMap, learningRate)

      if (inertiaExpertPrediction > 0.0) {
        var newWeight = inertiaExpertPrediction * Math.pow(Math.E, 1.0 * learningRate)
        //newWeight = if (newWeight.isPosInfinity) stateHandler.inertiaExpert.getWeight(currentFluent) else newWeight
        stateHandler.inertiaExpert.updateWeight(currentFluent, newWeight)
      }

      ///*
      updateRulesScore("FN", atom.initiatedBy.map(x => markedMap(x)), nonFiringInitRules.values.toVector,
        atom.terminatedBy.map(x => markedMap(x)), nonFiringTermRules.values.toVector)
      //*/
    }

    if (is_TN(predictedLabel, feedback)) {

      stateHandler.totalTNs = stateHandler.totalTNs + 1

      stateHandler.inertiaExpert.forget(currentFluent)

      ///*
      updateRulesScore("TN", atom.initiatedBy.map(x => markedMap(x)), nonFiringInitRules.values.toVector,
        atom.terminatedBy.map(x => markedMap(x)), nonFiringTermRules.values.toVector)
      //*/
    }

  }


  def updateStructure(atom: AtomTobePredicted, markedMap: Map[String, Clause],
                      predictedLabel: String, feedback: String, batch: Example,
                      currentAtom: String, inps: RunningOptions,
                      logger: org.slf4j.Logger, stateHandler: StateHandler,
                      batchCounter: Int, percentOfMistakesBeforeSpecialize: Int) = {

    if (is_FP_mistake(predictedLabel, feedback)) {
      if (atom.terminatedBy.isEmpty) {
        // We don't have firing termination rules. Generate one.
        generateNewRule(batch, currentAtom, inps, "FP", logger, stateHandler, batchCounter, "terminatedAt", 1.0)

      } else {
        // We do have firing termination rules.
        // Specialize initiation rules.
        if (atom.initiatedBy.nonEmpty) {
          val break_? = specialize(atom, stateHandler, markedMap, "initiated", inps, logger, percentOfMistakesBeforeSpecialize, "FP")
          if (break_?) break
        }
      }
    }
    if (is_FN_mistake(predictedLabel, feedback)) {
      if (atom.initiatedBy.isEmpty) {
        // We don't have firing initiation rules. Generate one.
        generateNewRule(batch, currentAtom, inps, "FN", logger, stateHandler, batchCounter, "initiatedAt", 1.0)

      } else {
        // We do have firing termination rules.
        // Specialize initiation rules.
        if (atom.terminatedBy.nonEmpty) {
          val break_? = specialize(atom, stateHandler, markedMap, "terminated", inps, logger, percentOfMistakesBeforeSpecialize, "FN")
          if (break_?) break
        }
      }
    }
  }

  def generateNewRule(batch: Example, currentAtom: String, inps: RunningOptions, mistakeType: String,
                      logger: org.slf4j.Logger, stateHandler: StateHandler, batchCounter: Int,
                      what: String, totalWeight: Double, removePastExperts: Boolean = false) = {

    val newRule = generateNewExpert(batch, currentAtom, inps.globals, what, totalWeight)
    if (!newRule.equals(Clause.empty)) {
      logger.info(s"Generated new $what rule in response to $mistakeType atom: $currentAtom")
      stateHandler.addRule(newRule)
      break
    } else {
      logger.info(s"At batch $batchCounter: Failed to generate bottom rule from $mistakeType mistake with atom: $currentAtom")
    }
  }

  def specialize(atom: AtomTobePredicted, stateHandler: StateHandler,
                 markedMap: Map[String, Clause], what: String, inps: RunningOptions,
                 logger: org.slf4j.Logger, percentOfMistakesBeforeSpecialize: Int, mistakeType: String) = {

    val topLevelRules = if (what == "initiated") stateHandler.ensemble.initiationRules else stateHandler.ensemble.terminationRules
    val allRules = topLevelRules.flatMap(x => x.refinements)
    val awakeExperts = if (what == "initiated") atom.initiatedBy else atom.terminatedBy // This contains the refinements

    // This contains the refinements
    val nonFiringRules =
      markedMap.filter(x =>
        x._2.head.functor.contains(what) && !awakeExperts.toSet.contains(x._1))

    // Select the top-level rules which are awake
    val rulesToSpecialize = topLevelRules.filter{ x =>
      val totalFPs = stateHandler.totalFPs
      val specialize = x.fps >= totalFPs * (percentOfMistakesBeforeSpecialize.toDouble/100)
      awakeExperts.toSet.contains(x.##.toString) && specialize
    }

    var performedSpecialization = false

    rulesToSpecialize foreach { ruleToSpecialize =>
      val suitableRefs =
        ruleToSpecialize.refinements.
          filter(r => nonFiringRules.keySet.contains(r.##.toString)).
          //filter(s => s.score > ruleToSpecialize.score).
          //filter(r => !allRules.exists(r1 => r1.thetaSubsumes(r) && r.thetaSubsumes(r1))).
          sortBy { x => (- x.w, - x.score, x.body.length+1) }

      if (suitableRefs.nonEmpty) {
        performedSpecialization = true
        val bestRefinement = suitableRefs.head
        if (bestRefinement.refinements.isEmpty) bestRefinement.generateCandidateRefs(inps.globals)
        showInfo(ruleToSpecialize, bestRefinement, atom.atom, mistakeType)
        stateHandler.removeRule(ruleToSpecialize)
        stateHandler.addRule(bestRefinement)
      }
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

  def getFeedback(a: AtomTobePredicted, withSplice: Boolean, trueLabels: Set[String] = Set[String]()) = {
    // The prediction is sent to Splice here to receive the feedback.
    // If the atom is labelled Slice will return its true label, otherwise it will send
    // a Splice-predicted label for this atom.
    // To work in a stand-alone fashion (without Splice) and also test it we also use
    // the true labels here.
    if (withSplice) {
      // ...
      ""
    } else {
      if (trueLabels.contains(a.atom)) "true" else "false"
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
                     testHandCrafted: Boolean = false) = {

    val ensemble = stateHandler.ensemble
    val merged = ensemble.merged(inps, testHandCrafted)

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
      val obj = new AtomTobePredicted(atom, fluent, time, initBy, termBy)

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

    /*
    if (inertiaExpertPrediction + initWeightSum > 0.0) {
      if (termWeightSum > 0) {
        (prediction, inertiaExpertPrediction, initWeightSum, termWeightSum)
      } else {
        (inertiaExpertPrediction + initWeightSum, inertiaExpertPrediction, initWeightSum, termWeightSum)
      }
    } else {
      (0.0, inertiaExpertPrediction, initWeightSum, termWeightSum)
    }
    */

    (prediction, inertiaExpertPrediction, initWeightSum, termWeightSum)
  }

  def predictRandomized(a: AtomTobePredicted, stateHanlder: StateHandler, markedMap: Map[String, Clause]) = {


    if(a.atom == "holdsAt(meeting(id1,id0),3280)") {
      val stop = "stop"
    }

    val (awakeInit, awakeTerm, currentFluent) = (a.initiatedBy, a.terminatedBy, a.fluent)
    val inertiaExpertPrediction = stateHanlder.inertiaExpert.getWeight(currentFluent)

    if (awakeInit.isEmpty && awakeTerm.isEmpty && inertiaExpertPrediction == 0.0) {
      (0.0, 0.0)
    } else {

      val awakeRuleExpertsWithWeights = (awakeInit ++ awakeTerm).map(x => (x, markedMap(x).w)).toMap

      val awakeExpertsWithWeights =
        if (inertiaExpertPrediction > 0) awakeRuleExpertsWithWeights + ("inertia" -> inertiaExpertPrediction) else awakeRuleExpertsWithWeights

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
        (inertiaExpertPrediction, totalWeight)
      } else {
        val expert = markedMap(selected)
        // return
        if (expert.head.functor.contains("initiated")) (expert.w, totalWeight) else (-expert.w, totalWeight)
      }
    }
  }





}

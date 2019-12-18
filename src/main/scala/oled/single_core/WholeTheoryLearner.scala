/*
 * Copyright (C) 2016  Nikos Katzouris
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

package oled.single_core

/**
  * Created by nkatz on 19/6/2017.
  */

class WholeTheoryLearner {}

/*

class WholeTheoryLearner(override val DB: Database,
                         override val delta: Double,
                         override val breakTiesThreshold: Double,
                         override val pruningThreshold: Double,
                         override val minSeenExmpls: Double,
                         val trainingSetSize: Int,
                         override val repeatFor: Int,
                         override val chunkSize: Int,
                         targetClass: String,
                         val withInertia: Boolean,
                         val withPostPruning: Boolean,
                         val onlinePruning: Boolean,
                         val trainingData: TrainingSet,
                         override val HLE: String,
                         override val learningInitWithInertia: Boolean = false,
                         //handCraftedTheoryFile: String = "",
                         kernelSet: Theory = Theory(),
                         globals: Globals)
  extends TheoryLearner(DB, delta, breakTiesThreshold, pruningThreshold,
    minSeenExmpls, trainingSetSize, repeatFor, chunkSize, targetClass,
    withInertia, withPostPruning, onlinePruning, trainingData,
    HLE, learningInitWithInertia, kernelSet, globals) {


  //override val bottomClauses = super.kernelSet

  override def receive = {
    case "go" => sender ! this.run
  }

  override def run: (Theory, Double) = {
    def runOnce(inTheory: Theory): Theory = {
      val data = getTrainingData
      data.foldLeft(inTheory){ (topTheory, newExample) =>
        //println(newExample.time)
        val res = Utils.time {
          this.processExample(topTheory, newExample)
        }
        res._1
      }
    }
    logger.info(s"Starting learning....")
    val _finalTheory = Utils.time{ (1 to repeatFor).foldLeft(Theory())( (t,_) =>  runOnce(t)) }
    val (finalTheory,time) = (_finalTheory._1,_finalTheory._2)
    logger.info(s"\nTraining time: $time")
    val output = withPostPruning match {
      case true =>
        //logger.info(s"Starting pruning...")
        //logger.info(s"Rescoring...")
        //reScore(DB,finalTheory,chunkSize,this.jep,trainingSetSize,targetClass, withInertia)
        //val pruned = finalTheory.clauses.filter(x => x.score > pruningThreshold)
        //Theory(pruned)
        finalTheory
      case _ => finalTheory
    }
    this.jep.close()
    (output,time)
  }


  def processExample(topTheory: Theory, e: Exmpl): Theory = {
    var newTopTheory = topTheory
    if (this.bottomClauses.clauses.isEmpty){
    val startNew = newTopTheory.growNewRuleTestWholeTheories(e, this.jep, globals)
      if (startNew) {
        val newRules = generateNewRules(topTheory, e)
        newTopTheory = topTheory.clauses ++ newRules
      }
    }
    if (newTopTheory.clauses.nonEmpty) {
      //val t0 = System.nanoTime()
      newTopTheory.scoreRules2(e.exmplWithInertia, this.jep, globals)
      //val t1 = System.nanoTime()
      //println(s"scoreRules time: ${(t1-t0)/1000000000.0}")

      try {
        val expanded = expandRules(newTopTheory)
        if (onlinePruning) {
          pruneRules(expanded)
        } else {
          expanded
        }
      } catch {
        case z: IndexOutOfBoundsException =>
          println(s"top theory:\n ${topTheory.tostring}")
          println(e.id)
          Theory()
      }
    } else {
      if (this.bottomClauses.clauses.isEmpty) {
        newTopTheory
      } else {
        // generate a top theory from the already constructed bottom clauses
        val top = this.bottomClauses.clauses map { x =>
          val c = Clause(head=x.head, body = List())
          logger.debug(s"Started growing new rule: \n ${c.tostring} from bottom clause: \n ${x.tostring}")
          c.addToSupport(x)
          c
        }
        Theory(top)
      }
    }
  }


  def generateNewRules(topTheory: Theory, e: Exmpl) = {
    val (_, varKernel) = Utils.generateKernel(e.exmplWithInertia.toMapASP, jep = this.jep, learningTerminatedOnly=false)
    val bottomTheory = topTheory.clauses flatMap(x => x.supportSet.clauses)
    val goodKernelRules =
      varKernel.filter(newBottomRule => !bottomTheory.exists(supportRule => newBottomRule.thetaSubsumes(supportRule)))
    goodKernelRules map { x =>
      val c = Clause(head=x.head, body = List())
      logger.debug(s"Started growing new rule: \n ${c.tostring} from bottom clause: \n ${x.tostring}")
      //println(x.tostring)
      c.addToSupport(x)
      c
    }
  }

  def getAverages(parentRule: Clause) = {
    val (observedDiff,best,secondBest) = parentRule.meanDiff2
    // Note that the seen examples count for each "top" clause and
    // each of its refinements is updated at the score method of the Theory class
    val epsilon = Utils.hoeffding(delta, parentRule.seenExmplsNum)
    val passesTest = if (epsilon < observedDiff) true else false
    val tie =
      if (observedDiff < epsilon  && epsilon < breakTiesThreshold && parentRule.seenExmplsNum >= minSeenExmpls) true
      else false
    val couldExpand = passesTest || tie
    (couldExpand,epsilon,observedDiff,best,secondBest)
  }

  override def expandRules(topTheory: Theory): Theory = {
    //val t0 = System.nanoTime()
    val out = topTheory.clauses flatMap { parentRule =>
      val (couldExpand, epsilon, observedDiff, best, secondBest) = getAverages(parentRule)
      couldExpand match {
        case true =>
          (best.score > parentRule.score) && (best.score - parentRule.score > epsilon) match { //&& (1.0/best.body.size+1 > 1.0/parentRule.body.size+1) match {
            case true =>
              val refinedRule = best
              logger.info(showInfo(parentRule, best, secondBest, epsilon, observedDiff, parentRule.seenExmplsNum))
              refinedRule.seenExmplsNum = 0 // zero the counter
              refinedRule.supportSet = parentRule.supportSet // only one clause here
              List(refinedRule)
            case _ => List(parentRule)
          }
        case _ => List(parentRule)
      }
    }
    //val t1 = System.nanoTime()
    //println(s"expandRules time: ${(t1-t0)/1000000000.0}")
    Theory(out)
  }

/*
  override def reScore(DB: Database, theory: Theory, chunkSize: Int, jep: Jep, trainingSetSize: Int, what: String, withInertia: Boolean) = {
    val dataChunks = getTrainingData
    theory.clauses foreach (p => p.clearStatistics) // zero all counters before re-scoring
    for (x <- dataChunks) {
      //println(x.id)
      theory.scoreRules(x.exmplWithInertia,jep,globals, postPruningMode = true)
    }
    logger.debug( theory.clauses map { p => s"score: ${p.score}, tps: ${p.tps}, fps: ${p.fps}, fns: ${p.fns}\n${p.tostring}" } mkString("\n") )
  }
*/



}
*/ 

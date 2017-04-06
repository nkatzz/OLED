package oled_distributed



import app.Globals
import com.typesafe.scalalogging.LazyLogging
import jep.Jep
import logic.{LogicUtils, Clause, Theory}
import oled_distributed.Structures.ClauseStats
import utils.DataUtils.{TrainingSet, DataAsExamples, DataAsIntervals}
import utils._
import utils.Implicits._


/**
  * Created by nkatz on 17/2/2017.
  */

object Functions extends LazyLogging {

  def score(clause: Clause) = clause.distScore

  def rightWay(parentRule: Clause, delta: Double, breakTiesThreshold: Double, minSeenExmpls: Int) = {
    val (observedDiff,best,secondBest) = parentRule.distributedMeanDiff
    val epsilon = utils.Utils.hoeffding(delta, parentRule.getTotalSeenExmpls)
    val passesTest = if (epsilon < observedDiff) true else false
    val tie = if (observedDiff < epsilon  && epsilon < breakTiesThreshold && parentRule.getTotalSeenExmpls >= minSeenExmpls) true else false
    val couldExpand = passesTest || tie
    (couldExpand,epsilon,observedDiff,best,secondBest)
  }

  def expandRule(parentRule: Clause, delta: Double, breakTiesThreshold: Double, minSeenExmpls: Int, nodeName: String) = {
    val (couldExpand,epsilon,observedDiff,best,secondBest) = rightWay(parentRule, delta, breakTiesThreshold, minSeenExmpls)
    if (couldExpand) {
      // This is the extra test that I added at Feedzai
      val extraTest =
        if(secondBest != parentRule) (score(best) > score(parentRule)) && (score(best) - score(parentRule) > epsilon)
        else score(best) > score(parentRule)
      if (extraTest) {
        val refinedRule = best


        logger.info(showInfo(parentRule, best, secondBest, epsilon, observedDiff, parentRule.seenExmplsNum, nodeName))

        refinedRule.seenExmplsNum = 0 // zero the counter
        refinedRule.totalSeenExmpls = 0 // zero the counter
        refinedRule.supportSet = parentRule.supportSet // only one clause here
        // In the distributed setting, refinements must be generated right after the construction of a clause,
        // in order to copy them in the clause copies that will be sent to other nodes (ensure same uuid's etc.)
        refinedRule.generateCandidateRefs
        (true, refinedRule)
      } else {
        (false, parentRule)
      }
    } else {
      (false, parentRule)
    }
  }

  def shouldExpand(parentRule: Clause, delta: Double, breakTiesThreshold: Double, minSeenExmpls: Int) = {
    val (couldExpand,epsilon,observedDiff,best,secondBest) = rightWay(parentRule, delta, breakTiesThreshold, minSeenExmpls)
    if (couldExpand) {
      val extraTest =
        if(secondBest != parentRule) (score(best) > score(parentRule)) && (score(best) - score(parentRule) > epsilon)
        else score(best) > score(parentRule)
      if (extraTest) {
        true
      } else {
        false
      }
    } else {
      false
    }
  }


  def getTrainingData(data: TrainingSet, DB: Database, chunkSize: Int,
                      targetClass: String, withInertia: Boolean, trainingSetSize: Int, HLE: String): Iterator[Exmpl] = {

    def getData = utils.CaviarUtils.getDataAsChunks(DB, chunkSize, targetClass, withInertia).take(trainingSetSize)

    data match {
      case x: DataAsIntervals =>
        if (data.isEmpty) getData
        else CaviarUtils.getDataFromIntervals(DB, HLE, data.asInstanceOf[DataAsIntervals].trainingSet, chunkSize)
      case x: DataAsExamples => data.asInstanceOf[DataAsExamples].trainingSet.toIterator
      case _ => throw new RuntimeException(s"${data.getClass}: Don't know what to do with this data container!")
    }
  }

  def reScore(data: TrainingSet, DB: Database, theory: Theory, chunkSize: Int, jep: Jep,
              trainingSetSize: Int, targetClass: String, withInertia: Boolean, HLE: String, globals: Globals) = {
    val dataChunks = getTrainingData(data, DB, chunkSize, targetClass, withInertia, trainingSetSize, HLE)
    theory.clauses foreach (p => p.clearStatistics) // zero all counters before re-scoring
    for (x <- dataChunks) {
      //println(x.id)
      theory.scoreRules(x.exmplWithInertia, jep, globals, postPruningMode = true)
    }
    logger.debug( theory.clauses map { p => s"score: ${score(p)}, tps: ${p.tps}, fps: ${p.fps}, fns: ${p.fns}\n${p.tostring}" } mkString("\n") )
  }

  def showInfo(c: Clause, c1: Clause, c2: Clause, hoeffding: Double, observedDiff: Double, n: Int, onNode: String) = {

    s"\n===========================================================\n" +
      s"\nClause (score: ${score(c)} | ${c.showCountsPerNode(onNode)}\n\n${c.tostring}\n\nwas refined to" +
      s" (new score: ${score(c1)} | ${c1.showCountsPerNode(onNode)}\n\n${c1.tostring}\n\nε: $hoeffding, ΔG: $observedDiff, examples used: $n" +
      //s"\nall refs: \n\n ${c.refinements.sortBy(z => -z.score).map(x => x.tostring+" "+" | score "+x.score+" | similarity "+similarity(x)).mkString("\n")}" +
      s"\nall refs (total tp/fp/fn counts):\n\n${c.refinements.sortBy(z => (-score(z), z.body.length+1)).map(x => x.tostring+" | " +
        "score "+score(x)+x.showCountsPerNode(onNode)).mkString("\n")}" +
      s"\n===========================================================\n"

  }



  def generateNewRules(topTheory: Theory, e: Exmpl, jep: Jep, initorterm: String,
                       globals: Globals, learningInitWithInertia: Boolean = false, otherNodeNames: List[String]) = {
    val terminatedOnly = if(initorterm == "terminatedAt") true else false
    val specialBKfile = if(initorterm=="initiatedAt") globals.BK_INITIATED_ONLY else globals.BK_TERMINATED_ONLY
    val (_, varKernel) =
      LogicUtils.generateKernel(e.exmplWithInertia.toMapASP, jep=jep, learningTerminatedOnly = terminatedOnly,
        oledLearningInitWithInertia = learningInitWithInertia, bkFile = specialBKfile, globals=globals)
    val bottomTheory = topTheory.clauses flatMap(x => x.supportSet.clauses)
    val goodKernelRules = varKernel.filter(newBottomRule => !bottomTheory.exists(supportRule => newBottomRule.thetaSubsumes(supportRule)))
    goodKernelRules map { x =>
      val c = Clause(head=x.head, body = List())
      logger.debug(s"Started growing new rule: \n ${c.tostring} from bottom clause: \n ${x.tostring}")
      c.addToSupport(x)
      otherNodeNames.foreach{ node =>
        c.countsPerNode(node) = new ClauseStats(0, 0, 0, 0)
      }
      // In the distributed setting, refinements must be generated right after the construction of a clause,
      // in order to copy them in the clause copies that will be sent to other nodes (ensure same uuid's etc.)
      c.generateCandidateRefs
      c
    }
  }

  def pruneRules(topTheory: Theory, delta: Double, pruningThreshold: Double, minSeenExmpls: Int) = {
    val pruned = topTheory.clauses.foldLeft(List[Clause]()){ (keep, clause) =>
      val epsilon = utils.Utils.hoeffding(delta, clause.seenExmplsNum)
      val meanPruningScore = clause.meanScorePruning(pruningThreshold)
      //if (this.pruningThreshold - meanScore > epsilon && clause.seenExmplsNum > minSeenExmpls) {
      if (meanPruningScore > epsilon && clause.seenExmplsNum > minSeenExmpls*10) {
        logger.info(s"\nPruned clause:\n${clause.tostring}\nMean score" +
          s" so far: ${clause.meanScorePruning(pruningThreshold)} | tps: ${clause.tps} fps: ${clause.fps}, fns: ${clause.fns}")
        keep
      } else {
        keep :+ clause
      }
    }
    Theory(pruned)
  }




}

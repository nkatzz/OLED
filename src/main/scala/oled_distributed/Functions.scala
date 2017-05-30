package oled_distributed



import app.{Globals, InputParameters}
import com.mongodb.casbah.commons.MongoDBObject
import com.typesafe.scalalogging.LazyLogging
import jep.Jep
import logic.{Clause, Literal, LogicUtils, Theory}
import oled_distributed.Structures.ClauseStats
import utils.DataUtils.{DataAsExamples, DataAsIntervals, TrainingSet}
import utils._
import utils.Implicits._


/**
  * Created by nkatz on 17/2/2017.
  */

object Functions extends LazyLogging {

  def score(clause: Clause) = clause.distScore

  def rightWay(parentRule: Clause, delta: Double, breakTiesThreshold: Double, minSeenExmpls: Int, minTpsRequired: Int = 0) = {
    val (observedDiff,best,secondBest) = parentRule.distributedMeanDiff
    val epsilon = utils.Utils.hoeffding(delta, parentRule.getTotalSeenExmpls)
    val passesTest = if (epsilon < observedDiff) true else false
    val tie = if (observedDiff < epsilon  && epsilon < breakTiesThreshold && parentRule.getTotalSeenExmpls >= minSeenExmpls) true else false
    val couldExpand = if (minTpsRequired != 0) (passesTest || tie) && best.getTotalTPs > minTpsRequired else passesTest || tie
    (couldExpand,epsilon,observedDiff,best,secondBest)
  }

  def expandRule(parentRule: Clause, delta: Double, breakTiesThreshold: Double, minSeenExmpls: Int, nodeName: String, params: InputParameters) = {
    val minTpsRequired = params.minTpsRequired
    val (couldExpand,epsilon,observedDiff,best,secondBest) = rightWay(parentRule, delta, breakTiesThreshold, minSeenExmpls, minTpsRequired)
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
        logger.info(s"Hoeffding test failed (clause ${parentRule.uuid}) not expanded")
        (false, parentRule)
      }
    } else {
      logger.info(s"Hoeffding test failed (clause ${parentRule.uuid}) not expanded")
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

  /*
  def showInfo(c: Clause, c1: Clause, c2: Clause, hoeffding: Double, observedDiff: Double, n: Int, onNode: String) = {

    s"\n===========================================================\n" +
      s"\nClause (score: ${score(c)} | ${c.showCountsPerNode(onNode)}\n\n${c.tostring}\n\nwas refined to" +
      s" (new score: ${score(c1)} | ${c1.showCountsPerNode(onNode)}\n\n${c1.tostring}\n\nε: $hoeffding, ΔG: $observedDiff, examples used: $n" +
      s"\nall refs (total tp/fp/fn counts):\n\n${c.refinements.sortBy(z => (-score(z), z.body.length+1)).map(x => x.tostring+" | " +
        "score "+score(x)+x.showCountsPerNode(onNode)).mkString("\n")}" +
      s"\n===========================================================\n"

  }
  */

  def showInfo(c: Clause, c1: Clause, c2: Clause, hoeffding: Double, observedDiff: Double, n: Int, onNode: String) = {

    s"\n===========================================================\n" +
      s"\nClause (score: ${score(c)} | ${c.showCountsPerNode(onNode)}\n\n${c.tostring}\n\nwas refined to" +
      s" (new score: ${score(c1)} | ${c1.showCountsPerNode(onNode)}\n\n${c1.tostring}\n\nε: $hoeffding, ΔG: $observedDiff, examples used: $n" +
      //s"\nall refs (total tp/fp/fn counts):\n\n${c.refinements.sortBy(z => (-score(z), z.body.length+1)).map(x => x.tostring+" | " +
      //  "score "+score(x)+x.showCountsPerNode(onNode)).mkString("\n")}" +
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



  def crossVal(t: Theory, DB: Database, jep: Jep, testingSetSize: Int, data: TrainingSet,
               handCraftedTheoryFile: String = "", globals: Globals, HLE: String, chunkSize: Int) = {
    val WHOLE_DATA_SET_VALE = 1000000000
    data match {
      case x: DataAsExamples =>
        val dataIterator =
          if (testingSetSize < WHOLE_DATA_SET_VALE) DB.collection.find().sort(MongoDBObject("exampleId" -> 1)).drop(testingSetSize)
          else DB.collection.find().sort(MongoDBObject("exampleId" -> 1))
        while (dataIterator.hasNext) {
          val e = dataIterator.next()
          val ee = new Exmpl(e)
          println(ee.id)
          evaluateTheory(t, ee, withInertia = true, jep, handCraftedTheoryFile, globals)
        }
      case x: DataAsIntervals =>
        val _data = getTestingData(data, DB, HLE, chunkSize)
        while (_data.hasNext) {
          val e = _data.next()
          println(e.time)
          evaluateTheory(t, e, withInertia = true, jep, handCraftedTheoryFile, globals)
        }
      case _ => throw new RuntimeException("This logic is not implemented yet")
    }
    val stats = t.stats
    (stats._1, stats._2, stats._3, stats._4, stats._5, stats._6)
  }

  def getTestingData(data: TrainingSet, DB: Database, HLE: String, chunkSize: Int): Iterator[Exmpl] = {
    data match {
      case x: DataAsExamples =>
        data.asInstanceOf[DataAsExamples].testingSet.toIterator
      case x: DataAsIntervals =>
        CaviarUtils.getDataFromIntervals(DB,HLE,data.asInstanceOf[DataAsIntervals].testingSet,chunkSize, withChunking = false)
      case _ => throw new RuntimeException("This logic is not implemented yet")
    }
  }

  def evaluateTheory(theory: Theory, e: Exmpl, withInertia: Boolean = true, jep: Jep, handCraftedTheoryFile: String = "", globals: Globals): Unit = {
    val varbedExmplPatterns = globals.EXAMPLE_PATTERNS_AS_STRINGS
    val coverageConstr = s"${globals.TPS_RULES}\n${globals.FPS_RULES}\n${globals.FNS_RULES}"
    val t =
      if(theory != Theory()) {
        theory.clauses.map(x => x.withTypePreds(globals).tostring).mkString("\n")
      } else {
        globals.INCLUDE_BK(handCraftedTheoryFile)
      }

    val show = globals.SHOW_TPS_ARITY_1 + globals.SHOW_FPS_ARITY_1 + globals.SHOW_FNS_ARITY_1
    val ex = if(withInertia) e.exmplWithInertia.tostring else e.exmplNoInertia.tostring
    val program = ex + globals.INCLUDE_BK(globals.BK_CROSSVAL) + t + coverageConstr + show
    val f = utils.Utils.getTempFile("isConsistent",".lp",deleteOnExit = true)
    utils.Utils.writeLine(program, f, "overwrite")
    val answerSet = ASP.solve(task = Globals.INFERENCE, aspInputFile = f, jep=jep)
    if (answerSet.nonEmpty) {
      val atoms = answerSet.head.atoms
      atoms.foreach { a=>
        val lit = Literal.toLiteral(a)
        val inner = lit.terms.head
        lit.functor match {
          case "tps" => theory.tps += inner.tostring
          case "fps" => theory.fps += inner.tostring
          case "fns" => theory.fns += inner.tostring
        }
      }
    }
  }





}

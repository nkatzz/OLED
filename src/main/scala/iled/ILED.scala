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

package iled

import java.io.File

import akka.actor.{Actor, ActorSystem, Props}
import com.mongodb.casbah.commons.MongoDBObject
import com.typesafe.scalalogging.LazyLogging
import logic.Examples.{Example, ExampleBatch, ExamplePair}
import logic.Rules.InconstistentRule
import logic._
import utils.{ASP, Database, MongoUtils}
import utils.Utils.{lined, time}
import xhail.Xhail
import akka.pattern.ask
import akka.util.Timeout
import app.runutils.Globals
import utils.parsers.{ASPResultsParser, ClausalLogicParser}

import scala.concurrent.Await
import scala.concurrent.duration._


/**
  * Created by nkatz on 6/20/17.
  */

object ILED extends ClausalLogicParser with LazyLogging with MongoUtils{


  def main(args: Array[String]) {
    val db = new Database("caviar_meeting_clean", "examples")
    val entryPath = "/home/nkatz/dev/iled/datasets/Caviar/meeting"
    val globals = new Globals(entryPath)
    //db.inspectDB(seeWhat="example")
    //collectKernels(db, batchSize=100, trainingSetSize=100000000,startTime = 0,jep=jep)

    val withBacktr = true
    val withWeaks = false

    //val weaksSelectionStrategy = Left("all-weaks")
    val weaksSelectionStrategy = Right(50)

    Globals.glvalues("withWeaks") = withWeaks.toString
    Globals.glvalues("withBacktr") = withBacktr.toString

    run(db, batchSize = 100, trainingSetSize = 100000000 , //100000 //15000
      withBacktr = withBacktr, withWeaks = withWeaks,
      weaksSelectionStrategy = weaksSelectionStrategy, startTime = 0, globals = globals, bkFile = globals.BK_WHOLE_EC)

  }



  def run(DB: Database, batchSize: Int = 1, trainingSetSize: Int = 0,
          withWeaks: Boolean = false, withBacktr: Boolean = true,
          weaksSelectionStrategy: Either[String, Int] = Left("all-weaks"),
          startTime: Int = 0, globals: Globals, bkFile: String) = {

    logger.info(s"${lined("Running configuration:")} \n" + (for ((k, v) <- Globals.glvalues) yield s"$k: $v").mkString("\n"))

    var finalTheory = new PriorTheory()
    var seenExs = List[Example]()
    var totalTime = 0.0
    var endTimeVar = 0
    var timesPerBatch = List[Double]()
    batchSize match {
      case 1 =>
        DB.collection.find().sort(MongoDBObject("time" -> 1)).foldLeft(List[Example](), new PriorTheory(), 0.0) {
          (x, y) => iledTop(x._1, Example(y), x._2, globals=globals, bkFile=bkFile)
        }
      case _ =>
        DB.nonEmpty match {
          case true =>
            val mainBlock = time {
              val upto = if (trainingSetSize > DB.size) DB.size / batchSize else trainingSetSize / batchSize
              val repeat = List.range(0, upto)
              // startTime,seenExamples,finalHyp,
              val run = repeat.foldLeft(startTime, List[Example](), new PriorTheory()) {
                (x, _) =>
                  val start = x._1
                  val pastExs = x._2
                  val theory = x._3
                  val _batch = DB.getBatch(start, batchSize, withWeaks)
                  val (batch, endTime) = (_batch._1, _batch._2)
                  val (_seenExs, _theory, _time) = withWeaks match {
                    case true =>
                      iledTop(pastExs, Example.merge(batch.examples),
                        theory, weakBatch = batch, withBacktr = withBacktr,
                        weaksSelectionStrategy = weaksSelectionStrategy, globals=globals, bkFile=bkFile)
                    case _ => iledTop(pastExs, batch.asSingleExmpl, theory, withBacktr = withBacktr, globals=globals, bkFile=bkFile)
                  }
                  endTimeVar = endTime
                  (endTime, _seenExs, _theory)
              }
              seenExs = run._2
              finalTheory = run._3
              if (!withBacktr) finalTheory = goBack(finalTheory, seenExs, globals = globals) // we go back at the end, after one pass over all the examples
              //if (withWeaks) pruneBadWeaks(seenExs,finalTheory.merge)
            }
            totalTime = mainBlock._2

          case _ => logger.info(s"Empty db ${DB.name}"); System.exit(-1)
        }
    }
    println(endTimeVar)

    if (!withWeaks) {
      wrapUp(DB, finalTheory, timesPerBatch, totalTime, globals = globals)
    } else {
      pruneBadWeaks(seenExs, finalTheory.merge, DB, totalTime, globals = globals)
    }
  }

  /*

  Use this for two-pass ILED as we discussed in our last meeting

   def twoPassKernelFirst(DB: Database,batchSize: Int = 1, trainingSetSize: Int = 0, startTime: Int = 0) = {

   }
   */

  def collectKernels(DB: Database, batchSize: Int = 1,
                     trainingSetSize: Int = 0, startTime: Int = 0,
                     globals: Globals, bkFile: String) = {
    val upto = if (trainingSetSize > DB.size) DB.size / batchSize else trainingSetSize / batchSize
    val repeat = List.range(0, upto)
    val run = repeat.foldLeft(startTime, new Theory()) {
      (x, _) =>
        val start = x._1
        val accumKernel = x._2
        val _batch = DB.getBatch(start, batchSize, usingWeakExmpls = false)
        val (batch, endTime) = (_batch._1, _batch._2)
        val toExmp = batch.asSingleExmpl
        //println(toExmp.time)

        val (kernel, _) = LogicUtils.generateKernel(toExmp.toMapASP, bkFile = bkFile, globals = globals)
        println(Theory(kernel).tostring)
        (endTime, accumKernel.extend(Theory(kernel)))
    }
    val wholeKernel = run._2
    val compressed = Theory(LogicUtils.compressTheory(wholeKernel.clauses))
    println(compressed.tostring)
    wrapUp(DB, new PriorTheory(retainedRules = compressed), batchTimes = List[Double](), totalTime = 0.0, globals = globals)
  }


  def iledTop(seenExs: List[Example],
              e: Example,
              pt: PriorTheory,
              weakBatch: ExampleBatch = ExampleBatch(),
              withBacktr: Boolean = false,
              weaksSelectionStrategy: Either[String, Int] = Left("all-weaks"),
              globals: Globals, bkFile: String): (List[Example], PriorTheory, Double) = {

    var totalTime = 0.0
    var forwardTime = 0.0
    var weaksTime = 0.0
    var totalBacktrTime = 0.0
    val allBacktrTimes = List[Double]()
    var supportUpdateTime = 0.0

    if (e.time == "1048160") {
      val stop = "stop"
    }

    val withWeaks = !weakBatch.isEmpty // learning from weak examples
    var priorTheory = if (!withWeaks) pt else pt.clearSupports(e, globals = globals)
    if (!isSAT(priorTheory.merge.strongRules, e, ASP.check_SAT_Program, globals = globals)) {
      logger.info(s"Procesing new example: ${e.time}")
      val wholeBlock = time {
        val learnNewBlock = time {
          priorTheory = learnFromNew(e, priorTheory, withWeaks = withWeaks, bkFile = bkFile, globals = globals)
        }
        forwardTime = learnNewBlock._2
        if (withWeaks) {
          val weaksBlock = time {
            priorTheory = learnFromWeaks(priorTheory, weakBatch, weaksSelectionStrategy, bkFile = bkFile, globals = globals)
          }
          weaksTime = weaksBlock._2
        }
        if (withBacktr) {
          if (priorTheory.newRules.clauses.nonEmpty && seenExs.nonEmpty) {
            val bcktrBlock = time {
              priorTheory = goBack(priorTheory, seenExs, withWeaks = withWeaks, globals = globals)
            }
            totalBacktrTime = bcktrBlock._2
          }
        }
      }
      totalTime = wholeBlock._2
    } else {
      //don't time this it reduces the average revision time
      val sUpdate = time {
        val kernel =
        // It takes too long to do this. Of course it's not correct. It's only for playing around with Arindam's data
          if (!Globals.glvalues("iter-deepening").toBoolean) thisGenerateKernel(e.toMapASP, bkFile=bkFile, globals=globals)
          else List[Clause]()
        if (kernel.nonEmpty) {
          logger.info(s"Example ${e.time}: correct/updating support")
          updateSupport(priorTheory.merge, Theory(kernel), withWeaks && e.isWeak)
        } else {
          logger.info(s"Example ${e.time}: correct")
        }
      }
      supportUpdateTime = sUpdate._2

      /*
      val updateSupports =
        priorTheory.merge.clauses filter {
          p => p.supportNeedsUpdate(e)
        }
      if (updateSupports.nonEmpty) {
        val kernel = IledStreaming.generateKernel(e.toMapASP)
        if (kernel.nonEmpty) {
          logger.info(s"Example: ${e.time} correct/updating support")
          updateSupport(Theory(updateSupports), Theory(kernel))
        }
      } else {
        logger.info(s"Example: ${e.time} correct")
      }
      */
    }
    val newSeen = seenExs :+ e
    (newSeen, priorTheory, totalTime)
  }


  def thisGenerateKernel(examples: Map[String,List[String]], fromWeakExmpl: Boolean = false, bkFile: String, globals: Globals) = {
    val infile = utils.Utils.getTempFile("example", ".lp", deleteOnExit = true)
    val f = (x: String) => if(x.endsWith(".")) x.split("\\.")(0) else x
    //val g = (x: String) => if(x.contains("example(")) x else s"example($x)"
    val interpretation = examples("annotation").map(x => s"${f(x)}.") ++ examples("narrative").map(x => s"${f(x)}.")
    utils.Utils.writeToFile(infile, "overwrite") { p => interpretation.foreach(p.println) }

    var (_, varKernel) = Xhail.runXhail(fromFile=infile.getAbsolutePath, kernelSetOnly=true, fromWeakExmpl=fromWeakExmpl, bkFile=bkFile, globals=globals)
    if (fromWeakExmpl) {
      varKernel = varKernel.map (x => Clause.updateField(x, fromWeakExample = true))
    }
    varKernel
  }

  def learnFromNew(e: Example, priorTheory: PriorTheory, withWeaks: Boolean, bkFile: String, globals: Globals): PriorTheory = {
    //val kernel = IledStreaming.generateKernel(e.toMapASP, jep=jep)
    val kernel =
      if (Globals.glvalues("specializeOnly").toBoolean && !priorTheory.isEmpty) List[Clause]()
      else thisGenerateKernel(e.toMapASP, bkFile = bkFile, globals=globals)

    var theory = new PriorTheory(retainedRules = priorTheory.merge.compress)
    if (!withWeaks) {
      theory = revise(kernelSet = Theory(kernel), priorTheory = theory, example = e, globals = globals)
    } else {
      theory = reviseWithWeaks(Theory(kernel), theory, e, globals = globals)
    }
    show(theory.retainedRules, theory.newRules, theory.refinedRules, e, "forward")
    //Utils.checkIfCompressed(theory)
    theory
  }


  def learnFromWeaks(pt: PriorTheory, weakBatch: ExampleBatch,
                     weaksSelectionStrategy: Either[String, Int], globals: Globals, bkFile: String): PriorTheory = {

    def showWeaks(WeakRules: Theory) = {
      if (WeakRules != Theory()) logger.info(s"\nNew weak rule: \n ${WeakRules.tostring}")
    }

    def weaksSelection = (how: Either[String, Int], weakBatch: ExampleBatch) => {
      how match {
        case Left(x) => x match {
          case "all-weaks" => weakBatch.weakExmpls
          case "hausdorff" => List[Example]() // Not implemented yet
        }
        case Right(x) =>
          val howMany = (x.toFloat / 100.0 * weakBatch.weakExmpls.length).toInt
          utils.Utils.sampleN(howMany, weakBatch.weakExmpls)
      }
    }
    var theory = pt.clearSupports(Example.merge(weakBatch.examples), globals = globals)
    val weaksToLearnFrom = weaksSelection(weaksSelectionStrategy, weakBatch)
    for (weak <- weaksToLearnFrom) {
      val newWeakBatch = Example.merge(weakBatch.weakExmpls.drop(weakBatch.weakExmpls.indexOf(weak)), markedAsWeak = true)
      logger.info(s"learning from weak example ${newWeakBatch.time}")
      val kernel = thisGenerateKernel(newWeakBatch.toMapASP, fromWeakExmpl = true, globals = globals, bkFile = bkFile)
      // IT IS NOT CORRECT TO TRY TO LEARN A NEW RULE FROM EACH NEW POINT. Instead, we simply pack weak support rules in existing rules

      val extraRules =
        if (!isSAT(theory.merge, newWeakBatch, ASP.check_SAT_Program, globals = globals)) {
          val weakRules = revise(kernelSet = Theory(kernel), priorTheory = new PriorTheory(),
            example = newWeakBatch, fromWeakExmpl = true, globals = globals).newRules /** Check if they are marked as weak (during debugging) */
          val extras = weakRules.clauses.filter(x => !theory.merge.containsRule(x) && x.head.functor != "terminatedAt")
          //val extras = weakRules.clauses.filter(x => !theory.merge.containsRule(x) )
          showWeaks(Theory(extras))
          extras
          //val extraRules = List[Clause]() // this shouldn't exist at all. It's left-over from learning weak rules from a batch
        } else {
          logger.info("No new weak rule, updating supports only")
          List[Clause]()
        }

      if (kernel.nonEmpty) {
        // Packing weak support rules is messy and leads to large and over-general theories
        // via the addition of extra rules during specialization

        //theory.newRules.updateSupports(Theory(kernel), fromWeakExample = true)
        //theory.retainedRules.updateSupports(Theory(kernel), fromWeakExample = true)
        //theory.refinedRules.updateSupports(Theory(kernel), fromWeakExample = true)
        theory =
          new PriorTheory(
            retainedRules = theory.retainedRules,
            newRules = theory.newRules.extendUnique(Theory(extraRules)),
            refinedRules = theory.refinedRules)
      }
    }
    theory
  }


  def goBack(priorTheory: PriorTheory, seenExamples: List[Example], withWeaks: Boolean = false, globals: Globals): PriorTheory = {
    logger.info("Re-checking the historical memory")
    var theory = new PriorTheory(retainedRules = priorTheory.merge.compress)
    for (past <- seenExamples) {

      logger.info(s"Re-seeing Example: ${past.time}")
      if (!isSAT(theory.merge, past, ASP.check_SAT_Program, globals = globals)) {

        if (past.time == "26960") {
          val stop = "stop"
        }

        theory = if (!withWeaks) theory else theory.clearSupports(past, globals = globals)
        if (!withWeaks) {
          theory = revise(priorTheory = theory, example = past, globals = globals)
        } else {
          theory = reviseWithWeaks(priorTheory = theory, e = past, globals = globals)
        }
        show(theory.retainedRules, theory.newRules, theory.refinedRules, past, "backwards")
      }
    }
    //Utils.checkIfCompressed(theory)
    theory
  }


  def reviseWithWeaks(kernel: Theory = Theory(), priorTheory: PriorTheory, e: Example, globals: Globals) = {
    var theory = priorTheory
    val (strongs, weaks) = theory.merge.strongWeakSplit
    // The correct approach is to first account for all the strong rules,
    // so that we have a "base" for the current example, and then try to
    // refine the weak ones. If strong and weak rules are treated together,
    // then it is likely to miss a "strong" revision, resulting later on to a dead-end.

    val strongRevision = revise(kernelSet = kernel, priorTheory = new PriorTheory(retainedRules = Theory(strongs)),
      example = e, withSupport = "strongOnly", globals = globals)

    val filtered = strongs.filter(p => p.supportSet.clauses.exists(q => q.fromWeakExample))
    val weakRevision = filtered match {
      case Nil => new PriorTheory()
      case _ =>
        val toRevise = new PriorTheory(retainedRules = Theory(filtered map (p => Clause(p, p.supportSet.weakRules))))
        val keepIntact = new PriorTheory(retainedRules = strongRevision.retainedRules, refinedRules = strongRevision.refinedRules, newRules = strongRevision.newRules)
        revise(priorTheory = toRevise, keepIntact = keepIntact, example = e, withSupport = "fullSupport", globals = globals)
    }

    //val weakRevision = new PriorTheory()

    theory = new PriorTheory(retainedRules = strongRevision.retainedRules.extendUnique(weakRevision.retainedRules),
      refinedRules = strongRevision.refinedRules.extendUnique(weakRevision.refinedRules),
      newRules = strongRevision.newRules.extendUnique(weakRevision.newRules))


    if (weaks != Nil) {
      //val reviseWeaks = revise(keepIntact=theory,priorTheory = new PriorTheory(theory.retainedRules.extendUnique(Theory(weaks)), theory.newRules, theory.refinedRules), example = e)
      val reviseWeaks = revise(keepIntact = theory, priorTheory = new PriorTheory(retainedRules = Theory(weaks)), example = e, globals = globals)
      val newWeaks = reviseWeaks.newRules
      val refinedWeaks = reviseWeaks.refinedRules
      val retainedWeaks = reviseWeaks.retainedRules
      theory =
        new PriorTheory(
          retainedRules = theory.retainedRules.extendUnique(retainedWeaks),
          newRules = theory.newRules.extendUnique(newWeaks),
          refinedRules = theory.refinedRules.extendUnique(refinedWeaks))
    }

    theory
  }


  def revise(kernelSet: Theory = Theory(),
             priorTheory: PriorTheory,
             keepIntact: PriorTheory = new PriorTheory(),
             example: Example,
             withSupport: String = "fullSupport",
             fromWeakExmpl: Boolean = false,
             noiseTolerant: Boolean = false,
             globals: Globals): PriorTheory = {

    val aspFile: File = utils.Utils.getTempFile("aspInduction", ".lp", "", deleteOnExit = true)
    val (_, use2AtomsMap, defeasiblePrior, use3AtomsMap, _, _) =
      ASP.inductionASPProgram(
        kernelSet = kernelSet,
        priorTheory = priorTheory.merge,
        examples = example.toMapASP,
        aspInputFile = aspFile,
        withSupport = withSupport,
        retained = keepIntact.merge,
        globals = globals)
    val answerSet = ASP.solve("iled", use2AtomsMap ++ use3AtomsMap, aspFile, example.toMapASP, fromWeakExmpl = fromWeakExmpl)
    if (answerSet != Nil) {
      val newRules = Rules.getNewRules(
        answerSet.head.atoms, use2AtomsMap,
        fromWeakExample = example.isWeak)

      updateSupport(newRules, kernelSet, fromWeakExample = example.isWeak)
      /*
      val icRules =
        try {

        } catch {
          case e: java.util.NoSuchElementException =>
        }
      */

      val icRules = Rules.getInconsistentRules(answerSet.head.atoms, priorTheory.merge, use3AtomsMap)
      val retainedRules = Theory(priorTheory.merge.clauses.filter(x => icRules.forall(y => y.rule != x)))
      updateSupport(retainedRules, kernelSet, fromWeakExample = example.isWeak)
      // Check whether to keep the initial refinement from
      // each inconsistent rule, or search further.
      /*
       val refinedRules = icRules map (
       p => getRefinedProgram(p, retainedRules, newRules, example)
          ) map (x =>
          if (x.finalRefinement == Theory())
            Theory(x.initialRefinement)
          else x.finalRefinement)
       */

      val refinedRules =
        if (!noiseTolerant) Rules.getRefined(icRules, retainedRules.extendUnique(keepIntact.merge), newRules, example,
          withSupport = withSupport, globals = globals)
        else thisRefine(icRules)
      new PriorTheory(retainedRules, newRules, Theory.mergeTheories(refinedRules))
    } else {
      priorTheory
    }
  }


  /* This is something forgotten from ILEDNoiseTollerant. It's here so that its reference above compiles.
  * If we go into a noise-tollerant version of ILED we could look at this. */

  def thisRefine(incRules: List[InconstistentRule]): List[Theory] = {
    // For each inconsistent rule R:
    // First, check each one of the stored refinements, to see if it
    // rejects the negative examples that the rule currently
    // covers. If one does, we are done (we simply update the
    // negative counts for R to use in the future and also the counts
    // for each existing refinement and each support clause).
    //-----------------------------------------------------
    // This is done by the function checkExistingRefs().
    // Also, maybe its better for the existing refinements
    // to be stored as fields of the support set rule they
    // were generated from.
    //-----------------------------------------------------
    // If None of the existing refinements rejects the negative
    // examples, we try to generate new refinements. How will
    // we do that? Further refine each one of the existing refs?
    // I guess so.
    // This is done by the function generateNewRefs().
    //----------------------------------------------------------
    // Perhaps its best to proceed as follows:
    // When a new example arrives we first score everything
    // (theory rules, their support rules and their refs). Via
    // scoring, we identify the inconsistent theory rules.
    // Next we check if an existing refinement rejects the negatives
    // (for the inconsistent rules)
    for (incRule <- incRules) {
      // val (_, use2AtomsMap, defeasiblePrior, use3AtomsMap, _, _) =
      //   ASP.inductionASPProgram(kernelSet=kernelSet,priorTheory=priorTheory.merge,examples=example.toMapASP,aspInputFile=aspFile,withSupport=withSupport,retained=keepIntact.merge)
    }
    List[Theory]()
  }



  def updateSupport(theory: Theory, kernelSet: Theory, fromWeakExample: Boolean = false) = {

    // This is used to avoid adding redundant rules in the
    // support set. A rule is redundant if it subsumes
    // by a rule already present in the support set
    val isRedundant = (ss: Clause, c: Clause) =>
      c.supportSet.clauses exists (x => ss.thetaSubsumes(x))

    for (c <- theory.clauses;
         ks <- kernelSet.clauses
         if !isRedundant(ks, c) && c.thetaSubsumes(ks)) {
      val markedKS = Clause.updateField(ks, fromWeakExample = fromWeakExample)
      c.supportSet = Theory(c.supportSet.clauses :+ markedKS)
    }

    // This is used in order to avoid maintaining redundant
    // rules in the support set. In this context, a support set
    // rule is redundant if it subsumes some other rule
    // in the support set. This can happen in cases where e.g.
    // p :- q,r was added to the support set early on and
    // later on p :- q,r,s was added. In this case the first
    // rule is redundant and should be removed. This redundancy
    // checking should be done whenever the support set
    // changes with the addition of a rule.

    theory.clauses foreach (x => x.compressSupport)
  }


  def wrapUp(DB: Database, theory: PriorTheory, batchTimes: List[Double] = List(),
             totalTime: Double, testingSet: List[(Int, Int)] = Nil, globals: Globals) = {

    def crossValidation(theory: Theory) = {
      var done = false
      var (tps, fps, fns) = (0.0, 0.0, 0.0)
      //var start = DB.startTime
      var start = 597240
      while (!done) {
        /** @todo factor out the bacth calls it is repeated throughout the code **/
        //println("crossval")
        val getbatch = DB.getBatch1(start, 5000) // get batches at 5000 to go faster
        val (batch, endTime) = (getbatch._1, getbatch._2)
        if (endTime == start) done = true
        val res = new Crossvalidation(batch.toMapASP, theory.clauses.map(x => x.withTypePreds(globals = globals)), globals = globals)
        tps = tps + res.tps
        fps = fps + res.fps
        fns = fns + res.fns
        start = endTime
      }
      val precision = tps.toFloat / (tps + fps)
      val recall = tps.toFloat / (tps + fns)
      val f_score = 2 * (precision * recall) / (precision + recall)
      (tps, fps, fns, precision, recall, f_score)
    }

    def crossValidation2(theory: Theory, testingSet: List[(Int, Int)], globals: Globals): (Double, Double, Double, Double, Double, Double) = {
      def cv(currentInterval: (Int, Int)) = {
        val batches = DB.getBatches(currentInterval._1, currentInterval._2, step = 40,
          howMany = List.range(currentInterval._1, currentInterval._2+40, 40).length, usingWeakExmpls = false)
        val examples = batches map (x => x.asSingleExmpl)
        examples.foldLeft((0.0, 0.0, 0.0)) {
          (x, y) =>
            val (tps, fps, fns) = (x._1, x._2, x._3)
            val res = new Crossvalidation(y.toMapASP, theory.clauses.map(x => x.withTypePreds(globals = globals)), globals = globals)
            (tps + res.tps, fps + res.fps, fns + res.fns)
        }
      }
      val stats = testingSet.foldLeft((0.0, 0.0, 0.0)) {
        (x, y) =>
          val (_tps, _fps, _fns) = (x._1, x._2, x._3)
          val m = cv(y)
          val (tps, fps, fns) = (m._1, m._2, m._3)
          (_tps + tps, _fps + fps, _fns + fns)
      }
      val (tps, fps, fns) = (stats._1, stats._2, stats._3)
      val precision = tps.toFloat / (tps + fps)
      val recall = tps.toFloat / (tps + fns)
      val f_score = 2 * (precision * recall) / (precision + recall)
      //val out = (tps, fps, fns, precision, recall, f_score)
      (tps, fps, fns, precision, recall, f_score)
    }

    //List.range()

    val hypothesis = Theory(Xhail.compressTheory(theory.merge.clauses))
    val (tps, fps, fns, precision, recall, f_score) =
      if (testingSet.isEmpty) {
        crossValidation(hypothesis)
      } else {
        //val c = new crossValidation2(hypothesis, testingSet)
        //c.out
        crossValidation2(hypothesis, testingSet, globals)
      }

    val hypothesisSize = hypothesis.clauses.foldLeft(0)((count, p) => count + p.toLiteralList.length)
    val showHypothesis = hypothesis.tostring

    logger.info(
      /*
      s"\nFinal Hypothesis:\n $showHypothesis" +
        s"\n\nHypothesis size: $hypothesisSize\n" +
        s"Total time (secs): $totalTime \n" +
        s"${lined("Crossvalidation")}\n" +
        s"tps: $tps\n" +
        s"fps: $fps\n" +
        s"fns: $fns\n" +
        s"precision: $precision\n" +
        s"recall: $recall\n" +
        s"f-score: $f_score"
      */
      s"\n$showHypothesis\ntps: $tps\nfps: $fps\nfns: $fns\nprecision: $precision\nrecall: $recall\nf-score: $f_score\ntraining time:" +
        s"$totalTime\ntheory size: $hypothesisSize"

    )

    //Utils.checkIfCompressed(theory)
    (hypothesisSize.toDouble, tps, fps, fns, precision, recall, f_score, totalTime)
  }

  // pruning threshold is an optional parameter that sets the pruning threshold to
  // some percent of the total positives counts. This is used by the code in
  // RTECCleanCaviarExperiments. It may be either a Right(x: Int), in which case the
  // threshold is set to x tps, or a Left( (hle: String, percent: Int) ), in which case
  // the pruning threshold is percent% of the total positives count for hle, found in
  // the training set.
  def pruneBadWeaks(seen: List[Example], theory: Theory, DB: Database,
                    totalTime: Double, pruningThreshold: Either[(String, Int), Int] = Right(2000),
                    testingSet: List[(Int, Int)] = Nil, globals: Globals) = {

    class RuleEvaluator(seen: List[Example], c: Clause) extends Actor {
      def receive = {
        case "go" =>
          seen.foldLeft(()) {
            (_, e) =>
              val res = new Crossvalidation(e.toMapASP, List(c.withTypePreds(globals = globals)), withInertia = false, globals = globals)
              c.tps = c.tps + res.tps
              c.fps = c.fps + res.fps
              c.fns = c.fns + res.fns
              println(c.tps)
          }
          logger.info("done evaluating single rule")
          sender ! "done"
      }
    }

    class CrossVal(pruned: Theory, globals: Globals) extends Actor {
      def receive = {
        case "go" =>
          sender ! wrapUp(DB, new PriorTheory(retainedRules = pruned), batchTimes = List[Double](), totalTime, testingSet = testingSet, globals = globals)
      }
    }

    class ResultsHandler(seen: List[Example], theory: Theory) extends Actor {
      logger.info("\nPruning weak rules")
      var results = (0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0)
      var done = false
      val pruneThreshold = pruningThreshold match {
        case Right(x) => x // hard-coded threshold
        case Left(x) =>
          val hle = x._1
          val percentage = x._2
          // compute the pruning threshold as a percentage of the total positives count in the training set
          val totalTps = seen.foldLeft(0) {
            (x, y) =>
              val positives = y.annotation.toSeq.count(p => p.contains(hle))
              x + positives
          }
          percentage / 100.0 * totalTps.toDouble
      }
      logger.info(s"\nPruning threshold: >= ${pruneThreshold.toInt}")
      var size = theory.clauses.size
      //for (c <- theory.weakRules.clauses) {
      for (c <- theory.clauses) {
        //context.actorOf(Props(new RuleEvaluator(seen, c, jep = new Jep()))) ! "go" // one jep instance per rule
        val grouped = seen.grouped(20).map(x => Example.merge(x)).toList
        context.actorOf(Props(new RuleEvaluator(grouped, c))) ! "go" // one jep instance per rule
      }

      def receive = {
        case "done" =>
          size -= 1
          if (size == 0) {
            val pruned = Theory(theory.clauses.filter(p => !p.fromWeakExample | (p.fromWeakExample && p.tps > pruneThreshold)))
            pruned.clauses foreach {
              x => logger.info(s"\n ${if (x.fromWeakExample) "weak" else "strong"} rule: \n ${x.tostring} \n tps: ${x.tps} \n fps: ${x.fps} \n fns: ${x.fns}")
            }
            // Pruning is finished, perform cross-validation on the pruned theory
            val crossValActor = context.actorOf(Props(new CrossVal(pruned, globals = globals)), name = "CrossValidationActor")
            implicit val timeout = Timeout(600 seconds) // wait ten minutes, just to be sure (it won't ever happen but I don't konw how to handle early time-outs)
            val future = crossValActor ? "go"
            results = Await.result(future, timeout.duration).asInstanceOf[(Double, Double, Double, Double, Double, Double, Double, Double)]
            done = true
          }

        case "isItReady?" =>
          if (done) {
            sender ! Right(results)
          } else {
            sender ! Left("wait!")
          }
      }
    }

    val system = ActorSystem("WeakRulesPruning")
    val actor = system.actorOf(Props(new ResultsHandler(seen, theory)), name = "WeakRulesEvaluator")
    implicit val timeout = Timeout(600 seconds)
    var done = false
    var out = (0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0)
    while (!done) {
      Thread.sleep(5000)
      val future = actor ? "isItReady?"
      val result = Await.result(future, timeout.duration).asInstanceOf[Either[String, (Double, Double, Double, Double, Double, Double, Double, Double)]]
      result match {
        case Left(x) => logger.info(s"...just asked if pruning is finished. ResultsHandler response: $x") // do nothing, ask again in a while to see of the results are ready
        case Right(x) =>
          done = true
          out = x
          //system.shutdown()
          system.terminate()
      }
    }
    out
  }


  class ResultsBean(val hypothesisSize: Double, val tps: Double, val fps: Double,
                    val fns: Double, val precision: Double, val recall: Double,
                    val f_score: Double, val totalTime: Double)


  def show(retained: Theory, news: Theory, refined: Theory, e: Example, how: String): Unit = {
    def showTheory(flag: String, t: Theory) = t match {
      case Theory.empty => ""
      case _ =>
        val header = s"\n$flag rules:\n"
        val line = "-" * header.length + "\n"
        header + line + t.tostring + "\n"
    }
    def header = how match {
      case "forward" => s" Example: ${e.time} "
      case "backwards" => s" Example: ${e.time} (re-seeing) "
    }

    val theoryEmpty = retained.isEmpty && news.isEmpty && refined.isEmpty

    val headerStr = s"\n==============$header==================\n"
    val footer = "\n" + "=" * headerStr.length + "\n"
    if (!theoryEmpty) {
      logger.info(headerStr + showTheory("Retained", retained) + showTheory("Refined", refined) + showTheory("New", news) + footer)
    }

  }


  def isSAT(theory: Theory, example: Example, F: (Theory, Example, Globals) => String, globals: Globals): Boolean = {
    val f = F(theory, example, globals)
    val out = ASP.solve(Globals.CHECKSAT, Map(), new java.io.File(f), example.toMapASP)
    if (out != Nil && out.head == AnswerSet.UNSAT) false else true
  }

  def iterSearchFindNotCovered(theory: Theory, example: Example, F: (Theory, Example) => String, globals: Globals): List[String] = {
    val f = ASP.iterSearchFindNotCoveredExmpls(theory, example, globals)
    val out = ASP.solve(Globals.CHECKSAT, Map(), new java.io.File(f), example.toMapASP)
    if (out != Nil && out.head == AnswerSet.UNSAT) out.head.atoms else List[String]()
  }



}


class Crossvalidation(val examples: Map[String, List[String]], val theory: List[Clause],
                      val withInertia: Boolean = true, val globals: Globals) extends ASPResultsParser {

  val FNS_PRED = "posNotCovered"
  val FPS_PRED = "negsCovered"
  val TPS_PRED = "posCovered"
  val aspInputFile = utils.Utils.getTempFile(prefix = "crossVal", suffix = ".lp", deleteOnExit = true)
  //val bk = if (withInertia) List(s"\n#include " + "\"" + Core.bkFile + "\".\n") else List(s"\n#include " + "\"" + Core.bkFileNoInertia + "\".\n")

  val bk = if (withInertia) List(s"\n#include " + "\"" + globals.BK_CROSSVAL + "\".\n") else List(s"\n#include " + "\"" + globals.ILED_NO_INERTIA + "\".\n")

  val command = Seq("python", Globals.ASPHandler, s"aspfile=${aspInputFile.getCanonicalPath}")
  val varbedExmplPatterns = for (x <- globals.eps2) yield x.varbed.tostring
  val coverageConstr =
    varbedExmplPatterns flatMap (x => List(s"\nposNotCovered($x) :- example($x), not $x.", s"\nnegsCovered($x):- $x, not example($x).", s"\nposCovered($x):- $x, example($x).\n"))

  utils.Utils.toASPprogram(
    program = bk ++ examples("annotation") ++ examples("narrative") ++
      theory.map(x => x.tostring) ++ coverageConstr,
    show = List("posNotCovered/1","negsCovered/1","posCovered/1"), writeToFile = aspInputFile.getCanonicalPath
  )

  val res = ASP.solveASPNoJep("crossvalidation", aspFile=aspInputFile.getCanonicalPath)
  val model = if (res.isEmpty) List[String]() else res.head.atoms

  def get(what: String) = {
    model match {
      case Nil => 0
      case _ =>
        model count (_.contains(what))
    }
  }

  val fns = get(FNS_PRED)
  val fps = get(FPS_PRED)
  val tps = get(TPS_PRED)
  val precision = tps.toFloat / (tps + fps)
  val recall = tps.toFloat / (tps + fns)
  val fscore = 2 * (precision * recall) / (precision + recall)
  val out = (tps, fps, fns, precision, recall, fscore)
}

package iled.core


import java.io.File
import com.mongodb.casbah.commons.MongoDBObject
import jep.Jep

import scala.collection.mutable.ListBuffer
import scala.math.sqrt
import scala.sys.process.ProcessLogger
import scala.sys.process.stringSeqToProcess
import scala.util.Random

import com.mongodb.BasicDBObject
import com.mongodb.casbah.MongoClient
import com.typesafe.scalalogging.LazyLogging
import com.typesafe.scalalogging.Seq

import iled.parsers.ASPResultsParser
import iled.utils.Utils
import iled.structures._
import iled.structures.Examples._
import iled.structures.Exceptions._


object IledStreaming extends ASPResultsParser with LazyLogging {

   

   def main(args: Array[String]) {
      val resultsFile = Utils.createOrClearFile("streaming-results")
      val examples = getAllPairs(step = 40, debug = false)
      val init = examples.filter { e => e.isStronglyInitiated }
      val term = examples.filter { e => e.isStronglyTerminated }
      val seedExample = term.head
      val learningTerminatedRule = if(seedExample.isStronglyInitiated || seedExample.isWeaklyInitiated) false else true
      val learningFromWeakExamples = true

      val jep = new Jep()

      generateRule(seedExample, delta = 0.05, breakTiesThreshold = 0.05, examples,
            resultsFile, learningTerminatedRule, learningFromWeakExamples, jep=jep)
   }

   def generateKernel(seedExample: ExamplePair, learningTerminatedRule: Boolean, jep: Jep) = {
      val interpretation =
         if (!learningTerminatedRule) seedExample.annotationASP ::: seedExample.narrativeASP
         else seedExample.annotationASP ::: seedExample.narrativeWithPriorSupervisionASP

      val infile = Utils.getTempFile("example", ".lp", deleteOnExit = true)
      Utils.writeToFile(infile, "overwrite") { p => interpretation.foreach(p.println) }
      val (_, varKernel) = Xhail.runXhail(fromFile = infile.getAbsolutePath, 
         kernelSetOnly = true, learningTerminatedAtOnly = learningTerminatedRule, jep=jep)
      varKernel
   }
   
   /**
    * This is a helper used for development. 
    */
   
   def generateKernel(examples: Map[String,List[String]], fromWeakExmpl: Boolean = false, jep: Jep) = {
      val infile = Utils.getTempFile("example", ".lp", deleteOnExit = true)
      val f = (x: String) => if(x.endsWith(".")) x.split("\\.")(0) else x
      //val g = (x: String) => if(x.contains("example(")) x else s"example($x)"
      val interpretation = examples("annotation").map(x => s"${f(x)}.") ++ examples("narrative").map(x => s"${f(x)}.")  
      Utils.writeToFile(infile, "overwrite") { p => interpretation.foreach(p.println) }
      
      var (_, varKernel) = Xhail.runXhail(fromFile = infile.getAbsolutePath, kernelSetOnly = true, fromWeakExmpl = fromWeakExmpl, jep=jep)
      if (fromWeakExmpl) {
         varKernel = varKernel.map (x => Clause.updateField(x, fromWeakExample = true))
      }
      varKernel
   }
   
   def generateRule(seedExample: ExamplePair,delta: Double, breakTiesThreshold: Double,
         examples: List[ExamplePair], resultsFile: String, learningTerminatedRule: Boolean, learningFromWeakExamples: Boolean, jep: Jep) = {
      val varKernel = generateKernel(seedExample,learningTerminatedRule, jep=jep)
      if(varKernel.isEmpty) {logger.error("No Kernel set was generated.");System.exit(-1)}
      val rules = new ListBuffer[Clause]
      /*
      for (c <- varKernel) {
         val parentRule = Clause(c.head, List())
         val (rule,_) = expandRule(parentRule, c, examples, 0, 0.05)
         rules += rule
      }
      val rs = Reasoning.compressTheory(rules.toList)
      Utils.writeToFile(new File(resultsFile), "append")(p => rs.map(x => s"\n${x.tostring}" ).foreach(p.println))
      * 
      */
      val c = varKernel.head
      val parentRule = Clause(c.head, List())
      val (rule, _) =
         expandRule(parentRule,c,examples,seenExamples=0,delta=0.05,breakTiesThreshold=0.05,learningFromWeakExamples=learningFromWeakExamples)
      Utils.writeToFile(new File(resultsFile), "append")(p => List(s"\n${rule.tostring}").foreach(p.println))
   }
   
   
   def expandRule(parentRule: Clause, bottomClause: Clause,
      examples: List[ExamplePair], seenExamples: Int, delta: Double,
      breakTiesThreshold: Double, fullyExpanded: Boolean = false, learningFromWeakExamples:Boolean = true): (Clause, Int) = {
      
      val max = examples.length
      val candidates = getOneStepRefinements(parentRule, bottomClause.body)
      val (refinement, newSeenExamples, expanded) = fullyExpanded match {
         case true => return (parentRule, seenExamples)
         case _ =>
            candidates match {
               case List() => return (parentRule, seenExamples)
               case _ =>
                  seenExamples match {
                     case `max` => return (parentRule, seenExamples)
                     case _ => scoreRefinements(parentRule, candidates, examples,
                           seenExamples, delta, breakTiesThreshold, learningFromWeakExamples = learningFromWeakExamples)
                  }
            }
      }
      expandRule(refinement, bottomClause, examples,
            newSeenExamples, delta, breakTiesThreshold, learningFromWeakExamples = learningFromWeakExamples)
   }

   /**
    * @param parentRule @tparam Clause the parent clause that is to be refined
    * @param candidates @tparam List[Clause] candidate one-step refinements of parentRule
    * @param examples @tparam a list representing the examples stream
    * @param seenExamples @tparam Int The number of examples (index in the stream)
    * from which the previous best refinement was generated. New examples from the
    * stream are accumulated from this index on, until the Hoeffding test succeeds for the
    * difference between the two best-scoring current refinements.
    * @param delta @tparam Double the confidence for the Hoeffding test
    * @param breakTiesThreshold @tparam Double a threshold on the Hoeffding bound value.
    * If the scores of the best-scoring antecedents are very close (for a long time), then a large number
    * of examples may be necessary in order for the Hoeffding test to succeed. This is presumably
    * wasteful, because in this case it makes little difference which attrinute (antecedent in our case) is chosen
    * (quoting "Mining High-Speed Data Streams" paper). If the Hoeffding bound reaches the breakTiesThreshold
    * then we infer that there is a tie and we break it by selecting randomly one of the
    * best-scoring antecedents.
    *
    */

   def scoreRefinements(parentRule: Clause, candidates: List[Clause], examples: List[ExamplePair],
      seenExamples: Int, delta: Double, breakTiesThreshold: Double, learningFromWeakExamples:Boolean = true): (Clause, Int, Boolean) = {

      val aspFile = Utils.getTempFile("scorerules", ".lp", "", deleteOnExit = true)
      var keepOn: Boolean = true
      // globalExmplCounter is an index in the examples stream, indicating the position in the stream
      // where the last antecedent was added to the current rule (i.e. the index
      // where the Hoeffding test last succeeded (or ties were broke randomly)
      var globalExmplCounter = seenExamples
      // currentCounter is the current examples counter. It counts the examples seen until
      // the Hoeffding test succeeds or until we break ties. This is the number based on
      // which the current Hoeffding bound is calculated.
      var currentExmplCounter = 0
      while (globalExmplCounter < examples.length) { // we break out with a return statement
         val epair = examples(globalExmplCounter)
         if(epair.isWeaklyInitiated) {
            val stop = "stop"
         }
         currentExmplCounter += 1
         globalExmplCounter += 1
        // val examplesBuffer =
        //    scala.collection.mutable.Map[String, List[String]]("annotation" -> epair.annotationASP, "narrative" -> epair.narrativeASP)
    
         val examplesBuffer = getInterp(epair,if(parentRule.head.tostring.contains("terminatedAt")) true else false, learningFromWeakExamples)
                

         scoreRule(parentRule, examplesBuffer, aspFile)
         logger.info(s"\n(#example $globalExmplCounter) parent node, ${parentRule.tps}, ${parentRule.fps}, ${parentRule.score} :\n" + s"${parentRule.tostring}\n")

         for (c <- candidates) {
            scoreRule(c, examplesBuffer, aspFile)
            logger.debug(s"\n(#example $globalExmplCounter) child node, ${c.tps}, ${c.fps}, ${c.score} :\n" + s"${c.tostring}\n")
         }

         val best2 = candidates.toList.sortBy { x => -x.score }.take(2) // - sign is to order with decreasing order (default is with increasing)
         val showRefs = (x: List[Clause]) => x.map { x => x.tostring + " " + x.score }.mkString("\n")
         val hoeffding = sqrt(scala.math.log(1.0 / delta) / (2 * currentExmplCounter))
         val observedDiff = best2.head.score - best2(1).score
         val passesTest = if (hoeffding < observedDiff) true else false
         val tie = if (hoeffding <= breakTiesThreshold) true else false
         if (passesTest || tie) {
            showInfo(parentRule, best2.head, best2(1), hoeffding, observedDiff, currentExmplCounter)
            // We expand the rule only if the best refinement is better than the current version of the rule:
            if (best2.head.score > parentRule.score) return (best2.head, globalExmplCounter, false)
            else return (parentRule, globalExmplCounter, false)
         }
      }
      (parentRule, globalExmplCounter, true)
   }

   
   def getInterp(pair: ExamplePair, learningTermRule: Boolean, learningFromWeakExamples: Boolean) = {
      // If we are currently learning a terminatedAt rule, then the annotation
      // for t1 (assuming that the example pair is for time points t1, t2) must
      // be provided. This way a positive example may be covered by intertia,
      // Note that if we do not follow this approach then a terminated rule has 
      // no true positives and it cannot even be scored while searching to expand it.
      // The same approach must be followed in case we we are learning from strongly
      // initiated/terminated examples. The prior supervision must be provided, in
      // order for inertia to be taken into account while scoring a rule on the example
      val (annotation,narrative) = 
         if(learningTermRule || !learningFromWeakExamples) 
            (pair.second.annotationASP, pair.first.narrativeASP:::pair.second.narrativeASP:::pair.first.annotation.map { x => x+"." })
         // not learning a termination rule AND learning from weakly initiated/terminated examples.
         // here prior supervision is omitted (it is given as target -- surrounded in the example/1 predicate)   
         else (pair.first.annotationASP:::pair.second.annotationASP, pair.first.narrativeASP:::pair.second.narrativeASP)
         scala.collection.mutable.Map("annotation" -> annotation, "narrative" -> narrative)
   }
   
   
   def showInfo(c: Clause, c1: Clause, c2: Clause, hoeffding: Double, observedDiff: Double, n: Int) = {
      logger.info("\n========================================" +
         s"\nHoeffding bound : $hoeffding" +
         s"\nobserved mean difference: $observedDiff" +
         s"\nNumber of examples used: $n\n" +
         "------------------------------------------------\n" +
         "Current rule (" + c.score + "): \n" +
         c.tostring + "\n" +
         "------------------------------------------------\n" +
         "Best refinement (" + c1.score + "): \n" +
         c1.tostring + "\n" +
         //"------------------------------------------------\n" +
         //"second best refinement (" + c2.score + "): \n" +
         //c2.tostring + "\n" +
         "============================================")
   }

   def scoreRule(c: Clause, exmplBuffer: scala.collection.mutable.Map[String, List[String]],
      aspFile: java.io.File) = {
      val (tps, fps, fns, _, _, _) = checkTheory(exmplBuffer, List(c.withTypePreds()), aspFile)
      updateStatistics(c, tps, fps, fns)
   }

   def updateStatistics(c: Clause, tps: Int, fps: Int, fns: Int) {
      c.tps += tps
      c.fps += fps
      c.fns += fns
      val (precision, recall) = (c.tps.toFloat / (c.tps + c.fps), c.tps.toFloat / (c.tps + c.fns))
      //c.precision = precision
      //c.recall = recall
      val fscore = 2 * (precision * recall) / (precision + recall)
      //c.score = fscore
      
      // We will use accuracy (which is the same as precision in the context of scoring function -- see e.g. the Aleph manual)
      // or other ILP textbooks and recall as the scoring functions. For initiatedAt rules, the scoring function will be 
      // accuracy (or the gini index based on accuracy) and for terminatedAt rules the scoring function will be recall.
      // Note that in order to use recall for scoring we have to allow for inertia to cover tps, (otherwise no tp will be covered).
      
      //if(c.head.tostring.contains("initiatedAt")) c.score = c.precision else c.score = c.recall

      val p = precision
      //c.score = p*scala.math.log(p) + (1-p)*scala.math.log(1-p) //entropy.This has a problem, the values are negative, not in [0,1]
      // so there's a small problem with the Hoeffding bound (mothing that cannot be fixed). 
      //p log p + (1-p) log (1-p)

      //c.score = 2*p*(1-p) // gini index
      
   }

   def evaluationFunction(name: String,tps:Int, fps:Int, fns: Int) = {
      
   }
   
   def checkTheory(examples: scala.collection.mutable.Map[String, List[String]],
      theory: List[Clause], aspInputFile: java.io.File): (Int, Int, Int, Float, Float, Float) = {

      """
        |This needs fixing (since I updated the solver)
        |
        |See command below
        |
      """.stripMargin


      val command = scala.collection.immutable.Seq("Core.aspSolverPath" + "/./clingo", Core.bkFile, aspInputFile.getCanonicalPath, "0", "--asp09")
      val varbedExmplPatterns = for (x <- Core.examplePatterns) yield x.varbed.tostring
      val coverageConstr: List[String] =
         varbedExmplPatterns.map(x => List(s"\nposNotCovered($x) :- example($x), not $x.",
            s"\nnegsCovered($x):- $x, not example($x).\n",
            s"\nposCovered($x):- $x, example($x).\n")).flatten
     // val extraRuleForTerminated = if (theory(0).head.tostring.contains("terminatedAt"))
     //    varbedExmplPatterns.map(x => List(s"$x :- example($x)."))
     // else List()
      Utils.toASPprogram(
         program = examples("annotation") :::
            examples("narrative") ::: theory.map(x => x.tostring) :::
            coverageConstr,
         show = List("posNotCovered/1") :+ "negsCovered/1" :+ "posCovered/1",
         writeToFile = aspInputFile.getCanonicalPath)
      val model = asp(command)(0).asInstanceOf[List[String]]
      val split = model.groupBy {
         case atom if atom.contains("posNotCovered") => "posNotCovered"
         case atom if atom.contains("negsCovered") => "negsCovered"
         case atom if atom.contains("posCovered") => "posCovered"
         case _ => "uknown"
      }
      //logger.debug("True positives: " + split("posCovered"))
      //logger.debug("False Positives: " + split("negsCovered"))
      //logger.debug("False Negatives: " + split("posNotCovered"))
      val f = (map: Map[String, List[String]], key: String) => if (map.keySet.exists(_ == key)) map(key).length else 0
      val (tps, fps, fns) = (f(split, "posCovered"), f(split, "negsCovered"), f(split, "posNotCovered"))

      val (precision, recall) = (tps.toFloat / (tps + fps), tps.toFloat / (tps + fns))
      val fscore = 2 * (precision * recall) / (precision + recall)
      (tps, fps, fns, precision, recall, fscore)
   }

   def asp(command: Seq[String]) = {
      val buffer = new StringBuffer()
      val allLines = command lines_! ProcessLogger(buffer append _)
      val lines = allLines match {
         case Stream() =>
            throw new ASPInputException(buffer.toString())
         case _ =>
            (for (x <- allLines) yield parseAll(aspResult, x.replaceAll("." + "\\s", "")) match {
               case Success(result, _) => result
               case f => None
            }).filter(x => x != None)
      }
      lines
   }

   /**
    * @param c @tparam Clause
    * @param lits @tparam List[Literal]
    * @returns all one-step refinements of c that may be generated with the addition of one literal from lits
    */

   def getOneStepRefinements(c: Clause, lits: List[Literal]): List[Clause] = {
      for (lit <- lits if !c.body.contains(lit)) yield Clause(c.head, c.body :+ lit)
   }

   /**
    * @param bottomClause @tparam CLause
    * @returns all clauses that may be generated using head(bottomClause)
    * as head and literals from body(bottomClause) as body
    */

   def getAllRefinements(bottomClause: Clause) = {
      // CAN WE FILTER OUT DUPLICATES (θ-subsumption equivalent) while generating the refinements?
      for (
         ref <- bottomClause.body.toSet.subsets;
         c = Clause(bottomClause.head, ref.toList);
         _ = logger.info(c.tostring + " ")
      ) yield c
   }



   /*=============================================================================
   The ones below already exist in iled.utils.MongoUtils. Refactor to reduce code.
   See iled.experiments.experiments.KRExperiments
   =============================================================================*/


   /**
     * If debug = true then it only gets a few data to work with
     */

   def getAllPairs(step: Int = 40, debug: Boolean = false): List[ExamplePair] = {
      val examplePairs = new ListBuffer[ExamplePair]
      val dbs = getDBsWithTimes()
      val alldbs = if (!debug) dbs else List(dbs.head)
      for (db <- alldbs) {
         val mongoClient = MongoClient()
         val collection = mongoClient(db.name)("examples")
         val pairs =
            for (i <- db.times.head to db.times(db.times.length - 2) by step; result <- Utils.getExamplePairs("time", i, i + step, collection)) yield result
         examplePairs ++= pairs
         mongoClient.close()
      }
      examplePairs.toList
   }



   /**
    * Helper container class for getDBsWithTimes()
    */

   case class DB(name: String) {
      var times: List[Int] = List()
   }

   /**
    * Helper method returns all DB names and the time points in every DB.
    */
   def getDBsWithTimes(): List[DB] = {
      val alldbs = new ListBuffer[DB]
      val dbs = Utils.getAllDBs()
      for (db <- dbs) {
         val database = DB(db)
         val mongoClient = MongoClient()
         val collection = mongoClient(db)("examples")
         for (x <- collection.find().sort(MongoDBObject("time" -> 1))) {
            val time = x.asInstanceOf[BasicDBObject].get("time").asInstanceOf[Int]
            database.times = database.times :+ time
         }
         database.times = database.times.sorted
         alldbs += database
         mongoClient.close()
      }
      alldbs.toList
   }

}
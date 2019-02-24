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

package logic

import java.text.DecimalFormat
import java.util.UUID

import app.runutils.Globals
import logic.Examples.Example
import oled.distributed.Structures.ClauseStats
import logic.Exceptions._
import logic.Modes.ModeAtom
import utils.{ASP, Utils}
import oled.distributed.Structures

import scala.collection.mutable.ListBuffer
import scala.util.Random
import utils.ClauseImplicits._
import utils.parsers.{ClausalLogicParser, PB2LogicParser}

/**
 * Companion object
 */

object Clause {

  val empty = Clause()

  def apply(lits: List[Literal]) = {
    new Clause(head = lits.head.asPosLiteral, body = lits.drop(1))
  }

  def apply(c: Clause, ss: Theory) = {
    new Clause(head=c.head,body=c.body,supportSet=ss,fromWeakExample=c.fromWeakExample)
  }

  def updateField(c: Clause, fromWeakExample: Boolean) = {
    new Clause(head = c.head, body = c.body, supportSet = c.supportSet, fromWeakExample = fromWeakExample)
  }

  def apply(head: Literal, body: List[Literal]) = {
    new Clause(head = head.asPosLiteral, body = body)
  }

  /* Parses a string into a Clause. */
  /*
  def parse(cl: String): Clause = {
    val p = new ClausalLogicParser
    p.getParseResult(p.parse(p.clause, cl)).asInstanceOf[Clause]
  }
  */

  /* Use this which is faster that combinators parsing. If any problems occur just fall back to the previous parser
  * (uncomment the method above.) See also the related comment at the parse method of the Literal companion object.*/
  def parse(cl: String) = parseWPB2(cl)

  def parseWPB2(cl: String) = PB2LogicParser.parseClause(cl).asInstanceOf[Clause]

  def toMLNFlat(c: Clause) = {}



}

case class Clause(head: PosLiteral = PosLiteral(),
                  body: List[Literal] = Nil,
                  fromWeakExample: Boolean = false,
                  var supportSet: Theory = Theory(),
                  uuid: String = UUID.randomUUID.toString) extends Expression {


  var parentClause: Clause = Clause.empty

  var isBottomRule = false

  // This field is used by the distributed version of oled.
  // It is a (k,v) map, where each k is the id (name) of one of the other nodes N and
  // v is a Structures.Stats instance carrying the current counts from N.
  var countsPerNode = scala.collection.mutable.Map[String, Structures.ClauseStats]()

  var mlnWeight: Double = 0.0
  var subGradient: Double = 0.0

  // This is a "general-purpose" weight variable, the intention is to use this
  // for all online convex optimization methods that we'll try (e.g. winnow, AdaGrad, Adam etc).
  // Currently, it's only used for the multiplicative weights update framework.
  var w: Double = 1.0

  // This is used in the (sleeping) expert setting, for randomized prediction.
  var selectionProbability = 0.0

  // Counts the updates to the w variable to calculate the running average
  var weightUpdateCount = 0.0
  var avgWeight = 0.0


  def updateRunningWeightAvg(newWeight: Double) = {
    val newAvg = ((avgWeight * weightUpdateCount) + newWeight)/(weightUpdateCount + 1)
    avgWeight = newAvg
    weightUpdateCount += 1
  }

  private val weights = new ListBuffer[Double]

  def updateWeightsBuffer(weight: Double) = weights += weight

  //(previousMeanDiff * previousMeanDiffCount) + newDiff)/(previousMeanDiffCount + 1)

  // These variables store the total current counts from all nodes.
  // These are also used in the distributed setting.
  var totalTPs = 0
  var totalFPs = 0
  var totalFNs = 0
  var totalSeenExmpls = 0

  // This is used in the distributed setting.
  // excludeNodeName is th e name of the node the clause is currently being
  // evaluated. The stats from this node are excluded from the total counts sum,
  // since these stats are taken from this.tps, this.fps etc
  def updateTotalCounts(excludeNodeName: String) = {
    // These are the tp, fp, fn and Nexmpl counts for clause, accumulated for all nodes
    val (totalTps, totalFps, totalFns, totalNexmpls) = this.countsPerNode.filter(x => x._1 != excludeNodeName).foldLeft(0,0,0,0){ (x, y) =>
      (x._1 + y._2.tps, x._2 + y._2.fps, x._3 + y._2.fns, x._4 + y._2.Nexmls)
    }
    this.totalTPs = totalTps*tpw
    this.totalFPs = totalFps*fpw
    this.totalFNs = totalFns*fnw
    this.totalSeenExmpls = totalNexmpls
  }

  // These are used in the distributed setting
  def getTotalTPs = this.tps*tpw + this.totalTPs
  def getTotalFPs = this.fps*fpw + this.totalFPs
  def getTotalFNs = this.fns*fnw + this.totalFNs
  def getTotalSeenExmpls = this.seenExmplsNum + this.totalSeenExmpls

  def showCountsPerNode(excludeNodeName: String) = {
    val transposed = this.countsPerNode.filter(x => x._1 != excludeNodeName).values.toVector.map(obj => Vector(obj.tps, obj.fps, obj.fns, obj.Nexmls)).transpose
    /*
    val tpsMsg = s"tps: ${transposed(0).sum} (${transposed(0).mkString("+")})"
    val fpsMsg = s"fps: ${transposed(1).sum} (${transposed(1).mkString("+")})"
    val fnsMsg = s"fps: ${transposed(2).sum} (${transposed(2).mkString("+")})"
    val exmplsMsg = s"emxpls: ${transposed(3).sum} (${transposed(3).mkString("+")})"
    */

    val tpsMsg = s"tps: $getTotalTPs [${tps*tpw} + (${transposed(0).mkString("+")})]"
    val fpsMsg = s"fps: $getTotalFPs [${fps*fpw} + (${transposed(1).mkString("+")})]"
    val fnsMsg = s"fns: $getTotalFNs [${fns*fnw} + (${transposed(2).mkString("+")})]"
    val exmplsMsg = s"emxpls: $getTotalSeenExmpls [$seenExmplsNum + (${transposed(3).mkString("+")})]"

    s"$tpsMsg $fpsMsg $fnsMsg $exmplsMsg"
  }



  /**
   * True positives, false positive and false negatives
   * covered by this clause (respectively). These are
   * intended to be used as accumulative scores (in streaming mode).
   * Also there are used in the noise-tolerant setting to identify
   * "good" support clauses to use for refinement.
   */

  var tps: Int = 0
  var fps: Int = 0
  var fns: Int = 0
  var tns: Int = 0

  private val (tpw, fpw, fnw) = (Globals.glvalues("tp-weight").toInt, Globals.glvalues("fp-weight").toInt, Globals.glvalues("fn-weight").toInt)

  def mestimate(mparam: Double, totalPositives: Int, totalNegatives: Int): Double = {
    // The Laplace estimate is:
    // l = (positivesCovered + m * relFreq)/totalExamplesCovered + m
    // where
    // positivesCovered = the number of positives covered by the clause
    // relFreq = the relative frequency of positives in the training set (pos/pos+negs)
    // totalExamplesCovered = the total number of examples covered by the clause
    // m = a parameter set according to the expected amount of noise (larger m for more noise)
    // We'll use the laplace m-estimate where m = 2 and p(+|clause) = 0.5

    //val positivesRelativeFrequency = totalPositives.toFloat / (totalPositives.toFloat + totalNegatives.toFloat)
    //val totalExmplsCovered = tps+fps
    //(tps.toFloat + (mparam * positivesRelativeFrequency)) / (totalExmplsCovered.toFloat + mparam)
    (tps.toFloat*tpw + 1) / (tps.toFloat*tpw  + fps.toFloat*fpw + 2.0)
  }

  def precision: Double = {
    val pr = tps.toFloat*tpw / (tps*tpw + fps*fpw)
    if (pr.isNaN) 0.0 else pr
  }

  def recall: Double = {
    val rec = tps.toFloat*tpw / ( tps*tpw + fns*fnw)
    if (rec.isNaN) 0.0 else rec
  }

  def fscore: Double =
    if (this.precision+this.recall == 0) 0.0
    else (2*this.precision*this.recall)/(this.precision+this.recall)

  def compressionInit = (tps*tpw - fps*fpw - (this.body.length + 1)).toDouble

  def compressionTerm = (tps*tpw - (fns*fns) - (this.body.length + 1)).toDouble

  def tpsRelativeFrequency =
    if (this.tps == 0 || this.parentClause.tps == 0) 0.0
    else this.tps.toDouble*tpw/this.parentClause.tps*tpw




  def foilGain(funct: String) = {

    val thisCoverage = if (funct == "precision") this.precision else this.recall
    val parentCoverage = if (funct == "precision") parentClause.precision else parentClause.recall

    if (thisCoverage == 0.0 || thisCoverage == 1.0) {
      // If thisCoverage == 0.0 then this rules covers nothing, it's useless, so set it's gain to 0.
      // Note that otherwise we'll have a logarithm evaluated to -infinity (log(0)).
      // If, on the other hand thisCoverage == 1.0 then this rule is perfect (but so is the parent --
      // the parent cannot have smaller coverage), so again, no gain.
      0.0
    } else {
      // here thisCoverage is in (0,1)
      if (parentCoverage == 1.0 || parentCoverage == 0.0) {
        // If parentCoverage == 1.0 then the parent rule is perfect, no way to beat that, so set this rule's gain to 0
        // Note that otherwise we'll have the parent's log evaluated to 0 and the gain formula
        // returning a negative value (parentTPs * log(thisCoverage), which is < 0 since thisCoverage < 1).
        // Eitherway, we only consider positive gain.
        // If, on the other hand, parentCoverage == 0.0 then thisCoverage == 0 (the parent covers nothing, so no way for
        // this rule -- a refinement --  to cover something)
        0.0
      } else {
        // here parentCoverage is in (0,1)
        val _gain = tps * (Math.log(thisCoverage) - Math.log(parentCoverage))

        // We are interested only in positive gain, therefore we consider 0 as the minimum of the gain function:
        val gain = if (_gain <= 0) 0.0 else _gain

        // This is the maximum gain for a given rule:
        val max = parentClause.tps.toDouble * (- Math.log(parentCoverage) )
        val normalizedGain =  gain/max

        if (normalizedGain.isNaN) {
          if (this.body.nonEmpty) {
            val stop = "stop"
          }

        }

        normalizedGain
      }
    }

  }



  /*
  def foilGainInit = {
    val nonzero = 0.0000006
    val adjust = (x: Double) => if (x.isNaN || x == 0.0) nonzero else x
    // How can this be normalized so we get a range in [0,1]???
    // Remember also that if you use this, the parent rule should not be included
    // in the calculation of the best-scoring rule, since it will always win

    //tpsRelativeFrequency * (Math.log(adjust(precision)) - Math.log(adjust(parentClause.precision)))

    val _gain = tps * (Math.log(adjust(precision)) - Math.log(adjust(parentClause.precision)))

    // We are interested only in positive gain, therefore we consider 0 as the minimum of the gain function:
    val gain = if (_gain <= 0) 0.0 else _gain

    // This is the maximum for a given rule:
    val max = parentClause.tps.toDouble * (- Math.log(adjust(parentClause.precision)) )
    val normalizedGain = if (max == 0) 0.0 else gain/max
    normalizedGain
  }

  def foilGainTerm = {
    val nonzero = 0.0000006
    val adjust = (x: Double) => if (x.isNaN || x == 0.0) nonzero else x
    // How can this be normalized so we get a range in [0,1]???
    // Remember also that if you use this, the parent rule should not be included
    // in the calculation of the best-scoring rule, since it will always win
    //tps * (Math.log(adjust(recall)) - Math.log(adjust(parentClause.recall)))
    //tpsRelativeFrequency * (Math.log(adjust(recall)) - Math.log(adjust(parentClause.recall)))

    val _gain = tps * (Math.log(adjust(recall)) - Math.log(adjust(parentClause.recall)))

    // We are interested only in positive gain, therefore we consider the minimum of the
    // gain function as 0:
    val gain = if (_gain <= 0) 0.0 else _gain

    // This is the maximum for a given rule:
    val max = parentClause.tps.toDouble * (- Math.log(adjust(parentClause.recall)) )
    val normalizedGain = if (max == 0) 0.0 else gain/max
    normalizedGain

  }
  */


  var refinements = List[Clause]()
  //var refinements = List[Refinement]()

  // The number of examples until the Hoeffding test succeeds
  var seenExmplsNum = 0

  // The number over which the mean of best scores' difference
  // has been computed. When a new mean score difference newDiff arrives,
  // to find the new mean we simply need to calculate
  //
  // newMean = (oldMean*previousCount + newDiff)/(previousCount+1)
  //
  // see also the calculation of meanDiff below.
  var previousMeanDiffCount = 0
  var previousMeanScoreCount = 0

  // Stores the previous mean difference observed between
  // the best and second-best specializations. It is used
  // for the calculation of the current mean in meanDiff method.
  var previousMeanDiff = 0.0

  // This stores the previous mean score (used for pruning)
  var previousScore = 0.0

  def meanDiff(scoringFunction: String) = {

    /*
    if (Globals.glvalues("distributed").toBoolean) {
      throw new RuntimeException("This is just to debug the distributed version, where the execution flow should not pass from here!")
    }
    */

    // The - sign is to sort with decreasing order (default is with increasing)
    // Also sort clauses by length, so that sorter clauses be preferred over longer ones with the same score
    val allSorted =
      if (scoringFunction == "foilgain")
        // The parent rule should not be included here (otherwise it will always win, see the foil gain formula)
        this.refinements.sortBy { x => (- x.score, - x.mlnWeight, x.body.length+1) }
      else
        (List(this) ++ this.refinements).sortBy { x => (- x.score, - x.mlnWeight, x.body.length+1) }

    val bestTwo = allSorted.take(2)

    //val (best,secondBest) = (bestTwo.head,bestTwo.tail.head)
    // The correct way to do it is as the commented one above. But in some cases
    // the refinements lists is empty (this has only occurred when I use basic and auxiliary predicates in fraud).
    // This should be handled generically, a clause with no candidate refs should not be considered for specialization
    val (best,secondBest) =
      if (bestTwo.length > 1)
        (bestTwo.head,bestTwo.tail.head)
      else
        (bestTwo.head,bestTwo.head)
    val newDiff = best.score - secondBest.score
    val newMeanDiff = ( (previousMeanDiff * previousMeanDiffCount) + newDiff)/(previousMeanDiffCount + 1)

    if (newMeanDiff.isNaN) {
      val stop = "stop"
    }

    previousMeanDiffCount += 1 // increase the count
    previousMeanDiff = newMeanDiff

    (newMeanDiff,best,secondBest)
  }


  /* This is used in the distributed setting */
  def distributedMeanDiff = {
    // The - sign is to sort with decreasing order (default is with increasing)
    // Also sort clauses by length also, so that sorter clauses be preferred over longer ones with the same score
    val allSorted = (List(this) ++ this.refinements).sortBy { x => (- x.distScore, x.body.length+1) }
    val bestTwo = allSorted.take(2)

    //val (best,secondBest) = (bestTwo.head,bestTwo.tail.head)
    // The correct way to do it is as the commented one above. But in some cases
    // the refinements lists is empty (this has only occurred when I use basic and auxiliary predicates in fraud).
    // This should be handled generically, a clause with no candidate refs should not be considered for specialization
    val (best,secondBest) =
      if (bestTwo.length > 1)
        (bestTwo.head,bestTwo.tail.head)
      else
        (bestTwo.head,bestTwo.head)
    val newDiff = best.distScore - secondBest.distScore
    val newMeanDiff = ( (previousMeanDiff * previousMeanDiffCount) + newDiff)/(previousMeanDiffCount + 1)
    previousMeanDiffCount += 1 // increase the count
    previousMeanDiff = newMeanDiff

    (newMeanDiff, best, secondBest)
  }


  def meanScorePruning(pruningThreshold: Double) = {
    val newScore = pruningThreshold - this.score
    //val newScore = this.score
    val newMeanScore = ( (previousScore*previousMeanScoreCount) + newScore)/(previousMeanScoreCount+1)
    previousMeanScoreCount += 1
    previousScore = newScore
    newMeanScore
  }

  def clearStatistics = {
    tps = 0
    fps = 0
    fns = 0
    seenExmplsNum = 0
    refinements = Nil
    previousMeanDiffCount = 0
    previousMeanScoreCount = 0
    previousMeanDiff = 0
  }

  def length = this.body.length + 1

  def presision_length = {
    val p = if (!precision.isNaN) precision else 0.0
    val l = length.toFloat/Globals.MAX_CLAUSE_LENGTH.toFloat
    val pl = if (p - l > 0.0) p-l else p
    pl
  }

  def recall_length = {
    val r = if (!recall.isNaN) recall else 0.0
    val l = length.toFloat/Globals.MAX_CLAUSE_LENGTH.toFloat
    val rl = if (r - l > 0.0) r-l else r
    rl
  }

  def weighted_precision = {
    if (!precision.isNaN) tps * precision else 0.0
  }

  def weighted_recall = {
    if (!recall.isNaN) tps * recall else 0.0
  }




  def score: Double = {

    /*
    if (this.foilGainInit.isInfinite || this.foilGainTerm.isInfinite) {
      val debug = "stop"
    }
    */

    /*
    if (Globals.glvalues("distributed").toBoolean) {
      throw new RuntimeException("This is just to debug the distributed version, where the execution flow should not pass from here!")
    }
    */

    if (this.head.functor == "initiatedAt") {
      Globals.scoringFunction match {
        case "default" => if (!precision.isNaN) precision else 0.0 // That's the standard

        //case "default" => weighted_precision

        //case "default" => if (!precision.isNaN) (tps.toFloat- (fps.toFloat - this.length.toFloat))/(tps.toFloat+fps.toFloat) else 0.0

        //case "default" => if (!precision.isNaN)  (1.0 - 1.0/(1.0+tps.toDouble)) * precision else 0.0

        case "foilgain" => foilGain("precision")
        case "fscore" => fscore
        case _ => throw new RuntimeException("Error: No scoring function given.")
      }

      //presision_length
      //compressionInit
      //foilGainInit
      //gainInt
    } else if (this.head.functor == "terminatedAt") {
      Globals.scoringFunction match {
        case "default" => if (!recall.isNaN) recall else 0.0

        //case "default" => weighted_recall

        //case "default" => (tps.toFloat- (fns.toFloat - this.length.toFloat))/(tps.toFloat+fns.toFloat)

        //case "default" => if (!recall.isNaN) (1.0 - 1.0/(1.0+tps.toDouble)) * recall else 0.0

        case "foilgain" => foilGain("recall")
        case "fscore" => fscore
        case _ => throw new RuntimeException("Error: No scoring function given.")
      }

      //recall_length
      //compressionTerm
      //foilGainTerm
      //gainTerm
    } else {
      // this.fscore
      /* Until now this has only been used for fraud.
       * We don't use f-score for evaluating individual
       * rules, because a rule's fns are irrelevant.
       * So we'll use precision.
       */

      //foilGainInit // No improvement!

      //gainInt      // No improvement!

      if (!precision.isNaN) precision else 0.0 // This is what I use but does not work well

      //  if (!precision.isNaN) (tps.toFloat + 10) / (tps.toFloat+10 + fps) else 0.0 // weight it just to check

      //rateDiff // No! (takes negative values)

      //tpsRelativeFrequency

      //fscore
    }
  }


  // This is the scoring function used in the distributed setting
  def distScore: Double = {
    val precision_ = getTotalTPs.toFloat / (getTotalTPs + getTotalFPs)
    /*
    val recall_ =
      if (List("initiatedAt","terminatedAt").contains(this.head.functor)) getTotalTPs.toFloat / ( getTotalTPs + (getTotalFNs * 10))
      else  getTotalTPs.toFloat / ( getTotalTPs + (getTotalFNs * 10))
    */
    val recall_ = getTotalTPs.toFloat / ( getTotalTPs + getTotalFNs)

    if (this.head.functor=="initiatedAt"){
      if (!precision_.isNaN) precision_ else 0.0

    } else if (this.head.functor=="terminatedAt") {
      if (!recall_.isNaN) recall_ else 0.0

    } else {
      if (!precision_.isNaN) precision_ else 0.0
    }
  }


  /* Maybe this must be moved to the Theory class and try to score
   * all clauses in a theory simultaneousely. */
  def score(currentTheory: Theory, currentTheoryScore: Double): Double = {
    0.0
  }

  def format(x: Double) = {
    val defaultNumFormat = new DecimalFormat("0.############")
    defaultNumFormat.format(x)
  }

  def showWithStats = {
    val scoreFunction = if (! Globals.glvalues("distributed").toBoolean) this.score else this.distScore
    val (tps_, fps_, fns_) =
      if(! Globals.glvalues("distributed").toBoolean) (tps*tpw, fps*fpw, fns*fnw)
      else (this.getTotalTPs, this.getTotalFPs, this.getTotalFNs)
    s"score:" + s" $scoreFunction, tps: $tps_, fps: $fps_, fns: $fns_ | " +
      s"MLN-weight: ${format(this.mlnWeight)} | Expert Weight (total/avg): ${this.w}/${this.avgWeight} " +
      s"Evaluated on: ${this.getTotalSeenExmpls} examples\n$tostring"
  }

  def showWithStats_NoEC = {
    //s"score (precision): $score, tps: $tps, fps: $fps, fns: $fns\n$tostring"
    s"score (precision): $score, tps: ${tps*tpw}, fps: ${fps*fpw}, fns: ${fns*fnw}\n$tostring"
  }




  /* Returns the maximum Hausdorff distance of this clause from a list of clauses */
  def similarity(x: List[Clause]) = {
    val maxDist = x.foldLeft(List[Double]()){ (accum,newClause) =>
      val sim = Hausdorff.litlistFromlitlist(this.literals,newClause.literals)
      accum :+ sim
    }.max
    maxDist
  }


  def updateStatistics(tps: Int, fps: Int, fns: Int, meanScoreDiff: Double, meanScoreDiff2: Double, exmplsCount: Int) = {
    this.tps = tps
    this.fps = fps
    this.fns = fns
    this.seenExmplsNum = exmplsCount
  }


  def condition = true

  /* generates candidate refinements for the Hoeffding test.
     The otherNodesNames is used only in the distributed setting. */
  def generateCandidateRefs(gl: Globals): Unit = {

    /*
    * Checks if a specialization is redundant. Currently a specialization is
    * redundant if it consists only of comparison predicates of the same type.
    * For instance, this is redundant:
    *
    * blah :- close(X,Y,30,12), close(X,Y,40,12), close(X,Y,50,12)
    *
    * where close(X, Y, Z, T) means that the Euclidean distance of X and Y at time T is less than Z.
    *
    * */
    def redundant(newLits: Set[Literal]) = {
      val all = this.body ++ newLits

      val test: Set[ModeAtom] = all.map(x => x.modeAtom).toSet

      // if the test variable is a singleton then all predicates are comparison predicates of the same type
      if (all.size == 1) {
        false
      } else {
        test.size == 1 && gl.comparisonPredicates.contains(test.head)
      }
    }

    val specializationDepth = Globals.glvalues("specializationDepth").toInt
    val candidateList = this.supportSet.clauses.flatMap(_.body).distinct.filter(!this.body.contains(_))
    val refinementsSets =
      (for (x <- 1 to specializationDepth) yield x).foldLeft(List[List[Clause]]()) { (accum, depth) =>
        val z = for ( lits <- candidateList.toSet.subsets(depth).toVector if !redundant(lits) ) yield Clause(this.head, this.body ++ lits)
        val z_ = Theory.compressTheory(z)
        accum :+ z_ 
    }
    val flattend = refinementsSets.flatten
    flattend.foreach{ refinement =>
      refinement.parentClause = this
      //------------------------------------
      refinement.mlnWeight = this.mlnWeight
      //------------------------------------
      refinement.supportSet = this.supportSet
      //------------------------------------
      val newMap = scala.collection.mutable.Map[String, ClauseStats]()
      if (Globals.glvalues("distributed").toBoolean) {
        // Just to be on the safe side in the distributed case
        if (this.countsPerNode.isEmpty) throw new RuntimeException(s"The countsPerNode map of clause ${this.uuid} is empty," +
          s" when it should have been populated at clause generation")
        this.countsPerNode.foreach { entry => newMap(entry._1) = new ClauseStats(0, 0, 0, 0)}
        refinement.countsPerNode = newMap
      }
    }
    this.refinements = flattend
  }

  def marked(globals: Globals) = {
    Clause(head=Literal(functor = "marked", terms=List(this.##.toString, this.head)), body=this.withTypePreds(globals).body)
  }

  //val isEmpty = this == Clause.empty

  def addToSupport(c: Clause) = {
    this.supportSet = Theory(this.supportSet.clauses :+ c)
  }

  def addToSupport(c: List[Clause]) = {
    this.supportSet = Theory(this.supportSet.clauses ++ c)
  }

  def removeFromSupport(c: Clause) = {
    this.supportSet = Theory(this.supportSet.clauses.filter(x => x!=c))
  }

  // This is used in order to avoid maintaining redundant
  // rules in the support set. In this context, a support set
  // rule is redundant if it subsumes some other rule
  // in the support set. This can happen in cases where e.g.
  // p :- q,r was added to the support set early on and
  // later on p :- q,r,s was added. In this case the first
  // rule is redundant and should be removed. This redundancy
  // checking should be done every time the support set
  // changes with the addition of a rule.

  def compressSupport = {
    val redundants = this.supportSet.clauses filter {
      p => this.supportSet.clauses exists {
        q => !p.equals(q) && (p thetaSubsumes q)
      }
    }
    this.supportSet = Theory(this.supportSet.clauses filter (p => !redundants.contains(p)))
  }

  // When a rule is consistent w.r.t.
  // a set of examples w, but there is no rule in its
  // support that entails precisely the examples that
  // this rule entails in w. Then its support needs
  // to be updated by 'encoding' the uncovered examples
  // with a set of new lifted bottom clauses.
  def supportNeedsUpdate(e: Example, globals: Globals): Boolean = {
    val coversThis = ASP.inference(this,e, globals=globals).atoms.toSet // what this covers
    val noUpdate = this.supportSet.clauses.exists(
        p => ASP.inference(p, e, globals=globals).atoms.toSet == coversThis // then no update is needed
      )
    ! noUpdate
  }

  def isConsistent(e: Example, globals: Globals): Boolean = {
    LogicUtils.isSAT(Theory(List(this)), e, globals, ASP.isConsistent_program)
  }

  override def tostringQuote = this.tostring

  override lazy val tostring = this.toStrList match {
    case List() => throw new LogicException("Cannot generate a Clause object for the empty clause")
    case h :: ts =>
      ts.length match {
        case 0 => h + "."
        case 1 => h + " :- \n" + "      " + ts.head + "."
        case _ => h + " :- \n" + (for (x <- ts) yield
          if (ts.indexOf(x) == ts.length - 1)
            s"      $x."
          else
            s"      $x,").mkString("\n")
      }
  }

  /* No new line after each literal */
  def tostring_debug = this.toStrList match {
    case List() => throw new LogicException("Cannot generate a Clause object for the empty clause")
    case h :: ts =>
      ts.length match {
        case 0 => h + "."
        case 1 => h + " :- " + ts.head + "."
        case _ =>
          h + " :- " + (for (x <- ts) yield
            if (ts.indexOf(x) == ts.length - 1) s"$x."
            else s"$x,").mkString("")
      }
  }

  /* No new line after each literal, no final ".", "^" instead of "," for conjunctions. */
  def tostring_MLN(id: Int) = {

    def format(x: Double) = {
      val defaultNumFormat = new DecimalFormat("0.############")
      defaultNumFormat.format(x)
    }
    val markedHead = PosLiteral(this.head.functor, terms = this.head.terms :+ Constant(s"ruleId_$id"))
    (List(markedHead.asLiteral) ++ this.body).map(x => x.toMLN).map(x => x.tostring) match {
      case List() => throw new LogicException("Cannot generate a Clause object for the empty clause")
      case h :: ts =>
        ts.length match {
          case 0 => format(this.mlnWeight) + " " + h
          case 1 => (format(this.mlnWeight) + " " + h + " :- " + ts.head).replaceAll("not ", "!")
          case _ =>
            format(this.mlnWeight) + " " + h + " :- " + (for (x <- ts) yield
              if (ts.indexOf(x) == ts.length - 1) s"$x" else s"$x ^ ").mkString("").replaceAll("not ", "!")
        }
    }
  }

  /* The id term is already given. */
  def tostring_MLN_1() = {

    def format(x: Double) = {
      val defaultNumFormat = new DecimalFormat("0.############")
      defaultNumFormat.format(x)
    }
    //val markedHead = PosLiteral(this.head.functor, terms = this.head.terms)

    this.toLiteralList.map(x => x.toMLN).map(x => x.tostring) match {
      case List() => throw new LogicException("Cannot generate a Clause object for the empty clause")
      case h :: ts =>
        ts.length match {
          case 0 => format(this.mlnWeight) + " " + h
          case 1 => (format(this.mlnWeight) + " " + h + " :- " + ts.head).replaceAll("not ", "!")
          case _ =>
            format(this.mlnWeight) + " " + h + " :- " + (for (x <- ts) yield
              if (ts.indexOf(x) == ts.length - 1) s"$x" else s"$x ^ ").mkString("").replaceAll("not ", "!")
        }
    }
  }

  def to_MLNClause() = {
    val litsToMLN = this.toLiteralList.map(x => Literal.toMLNClauseLiteral(x).tostring)
    litsToMLN match {
      case List() => throw new LogicException("Cannot generate a Clause object for the empty clause")
      case h :: ts =>
        ts.length match {
          case 0 => format(this.mlnWeight) + " " + h
          case 1 => (format(this.mlnWeight) + " " + h + " :- " + ts.head).replaceAll("not ", "!")
          case _ =>
            format(this.mlnWeight) + " " + h + " :- " + (for (x <- ts) yield
              if (ts.indexOf(x) == ts.length - 1) s"$x" else s"$x ^ ").mkString("").replaceAll("not ", "!")
        }
    }
  }

  def varbed: Clause = {
    var accum = ListBuffer[Literal]()
    var map = scala.collection.mutable.Map[Expression, Expression]()
    var counter = 0
    for (x <- this.toLiteralList) {
      val (a, _, c, d) = x.variabilize(List(Literal(functor = x.functor, isNAF = x.isNAF)),
        x.terms zip x.modeAtom.args, map, List(), counter)
      val aa = Literal(a.head.functor, a.head.terms, a.head.isNAF, x.modeAtom, a.head.typePreds)
      accum ++= List(aa)
      map ++ c
      counter = d
    }
    val l = accum.toList
    val out = Clause(head = l.head, body = l.tail)
    out
  }

  /**
   * this theta-subsumes that
    *
    * @param that @tparam Clause the (potentially) sumbumed clause
   * @return true if this subsumes that else false
   */

  def thetaSubsumes(that: Clause): Boolean = {
    /*
    this.toLiteralList.forall{ l =>
      that.toLiteralList.exists { l2 =>
        f(l,l2)
      }
    }
    */
    val isVar = (x: String) => try {
      Variable(x); true
    } catch {
      case _: IllegalArgumentException => false
    }
    val (skolemised, skmap) = that.skolemise
    var skolems = (for (y <- skmap.keySet.filter(x => isVar(x))) yield skmap(y)).toList
    val thisVars = this.getVars
    while (thisVars.length > skolems.length) {
      skolems = skolems ::: skolems
    }
    for (x <- skolems.permutations) {
      val trySubstitution = (thisVars zip x).map { x => (x._1, Constant(x._2)) }.toMap
      val repl = this.toLiteralList.map { x => x.replaceAll(trySubstitution) }.map { x => x.tostring }
      if (Utils.isSubset(repl.toSet, skolemised.toStrList.toSet)) return true
    }
    false
  }

  def thetaSubsumes(t: Theory): Boolean = {
    t.clauses.forall(x => this.thetaSubsumes(x))
  }

  /**
   * Same as above, but returns a List[String].
   */

  def toStrList: List[String] = List(head.tostring) ++ (for (x <- body) yield x.tostring)

  def toStrList_no_NAF: List[String] = List(head.tostring) ++ (for (x <- body) yield x.tostring_no_NAF)

  def literals: List[Literal] = List(this.head.asLiteral) ++ this.body

  /**
   * Get the variables from this clause
   */

  def getVars = {
    val vars = this.head.asLiteral.getVars
    for (x <- this.body) vars ++= x.getVars.filter { x => !vars.contains(x) }
    vars.toList
  }

  /**
   * Replaces all variables with a new constant symbol 'skolem0', 'skolem1' etc. Same variables correspond to the
   * same constant symbol. Constants remain intact, i.e. they are used as skolem constants themselves. Example:
   *
   * a(X,Y,Z) :-
   * p(x,q(Y,const1,2),Z),
   * not r(A,B,C).
   *
   * is turned into:
   *
   * a(skolem0,skolem1,skolem2) :-
   * p(skolem0,q(skolem1,const1,2),skolem2),
   * not r(skolem3,skolem4,skolem5).
   *
   * Returns the skolemised clause and the 'vars -> skolems' map
   *
   */

  def skolemise: (Clause, Map[String, String]) = {
    val l = this.toLiteralList
    val skmap = this.getSkolemConsts
    var temp = new ListBuffer[Literal]
    for (x <- l) {
      val m = x.skolemize(skmap).toList
      val toLit = Literal(functor = x.functor, terms = m, isNAF = x.isNAF)
      temp += toLit
    }
    val fl = temp.toList
    val sk = Clause(head = fl.head,
      body = for (x <- fl; if fl.indexOf(x) != 0 ) yield x)
    (sk, skmap)
  }

  /**
   * Generates skolem constants from the variables and the constants of the clause. It returns a map of the form
   * Map('X -> skolem0', 'Y -> skolem1', 'const -> const', .... ) (we use the constants as skolem constants)
   */

  private def getSkolemConsts: Map[String, String] = {
    val l = this.toLiteralList
    //print(l)
    var skolems = new ListBuffer[(String, String)]
    var counter = 0
    for (x <- l) {
      val m = x.getSkolemConsts(skolems, counter);
      skolems = m._1; counter = m._2
    }
    skolems.toMap
  }

  def use_2_split(i: Int, globals: Globals): (Theory, Map[String, Literal]) = {
    val temp = for (
      (lit, j) <- this.toLiteralList zip List.range(0, this.toLiteralList.length);
      vars = lit.variables(globals);
      _try = Literal(functor = "try",
        terms = List(Constant(s"$i"), Constant(s"$j"),
        Literal(functor = "vars",
                terms = for (x <- vars) yield Variable(x.name, _type = x._type))),
                typePreds = for (x <- vars) yield s"${x._type}(${x.name})");
      tryLit = j match {
        case 0 => Literal(functor = "use2", terms = List(Constant(s"$i"), Constant("0")))
        case _ => _try
      };
      useMap = j match {
        case 0 => s"use2($i,0)" -> this.head.asLiteral
        case _ => s"use2($i,$j)" -> Literal(functor = lit.functor, terms = lit.terms,
          isNAF = lit.isNAF, typePreds = _try.typePreds, modeAtom = lit.modeAtom)
      };

      tryClause1 = if (j > 0)
        Clause(head = _try,
          body = List(Literal(functor = "use2",
            terms = List(Constant(s"$i"), Constant(s"$j")) ), lit))
      else None;
      tryClause2 = if (j > 0)
        Clause(head = _try, body = List(Literal(functor = "use2",
          terms = List(Constant(s"$i"), Constant(s"$j")), isNAF = true)) :::
          (for (x <- _try.typePreds) yield Literal(x) ))
      else None

    ) yield (tryLit, tryClause1, tryClause2, useMap)
    val ls = temp.map { x => List(x._1, x._2, x._3, x._4) }.transpose
    val defeasible = Theory(
      (List(Clause(head = this.head, body = ls.head.map(x => x.asInstanceOf[Literal]))) :::
        ls(1).filter { x => x != None }.map(x => x.asInstanceOf[Clause]) :::
        ls(2).filter { x => x != None }.map(x => x.asInstanceOf[Clause])).map { x => x.withTypePreds(globals) })
    //val useMap = ls(3).asInstanceOf[List[(String,Literal)]].groupBy(_._1).map { case (k,v) => (k,v.map(_._2))}
    val useMap = ls(3).asInstanceOf[List[(String, Literal)]].groupBy(_._1).map { case (k, v) => v.head }
    (defeasible, useMap)
  }

  /**
   * Generates a defeasible theory from a single support set rule. This rule
   * may either be chosen at random from the support set, or it can be set
   * to a particular one (given with the input). This is used in order to identify
   * inconsistent rules from the prior hypothesis w.r.t. a new example window.
   *
   */

  def use_3_split_one(i: Int, ssClause: Clause = Clause.empty, withSupport: String="fullSupport", globals: Globals) = {
    if (this != Clause.empty) {
      val SSRule = ssClause match {
        case Clause.empty =>
          if (withSupport == "fullSupport")
            Random.shuffle(this.supportSet.clauses).head // select an arbitrary rule
          else
            Random.shuffle(this.supportSet.strongRules.clauses).head // select a strong rule
        case _ => ssClause
      }
      val j = this.supportSet.clauses.indexOf(SSRule) + 1
      val (defeasible, useMap) = this.f(SSRule, i, j, globals)
      (Theory(defeasible), useMap, s"{use3($i,$j,1..${SSRule.body.length})}.")
    } else {
      (Theory(), Map[String, Literal](), "")
    }
  }


  /**
    * generates a defeasible theory from this clause, using every rule in its
    * support set. Each defeasible theory resulting from each support set rule
    * is merged with the others.
    */

  def use_3_split(i: Int, withSupport: String = "fullSupport", globals: Globals) = {
    //val support = if(withSupport=="fullSupport") this.supportSet else this.supportSet.strongRules
    val e = if(withSupport=="fullSupport") { // analyze the whole support set
      for ((x, j) <- this.supportSet.clauses zip List.range(1, this.supportSet.clauses.length + 1)) yield f(x, i, j, globals)
    } else { // analyze only the strong support rules, by skipping the weak ones. Indexing remains consistent with the actual ordering in the support
      for ((x, j) <- this.supportSet.clauses zip List.range(1, this.supportSet.clauses.length + 1) if !this.supportSet.clauses(j-1).fromWeakExample) yield f(x, i, j, globals)
    }
    val g = e.foldLeft(Tuple2(List[Clause](), Map[String, Literal]()))((x, y) => (x._1 ++ y._1, x._2 ++ y._2))
    val generates = (this.supportSet.clauses zip List.range(1, this.supportSet.clauses.length + 1)).map(p => s"{use3($i,${p._2},1..${p._1.body.length})}.").mkString("\n")
    (Theory(g._1), g._2,generates)
  }


  def f(c2: Clause, i: Int, j: Int, globals: Globals)  = {
    val usedPart = c2.body filter (x => !this.body.contains(x))
    val (iconst, jconst) = (Constant(s"$i"), Constant(s"$j"))
    val vars = this.head.asLiteral.variables(globals)
    val varlit = Literal(functor = "vars", terms = vars map (x => Variable(x.name, _type = x._type)))
    val exceptionLit = Literal(functor = "exception",
      terms = List(iconst, jconst, varlit), isNAF = true)
    val res = (
      for (
        (x, k) <- usedPart zip List.range(1, usedPart.length + 1);
        use3lit = Literal(functor = "use3", terms = List(Constant(s"$i"), Constant(s"$j"), Constant(s"$k")))
      ) yield (Clause(head = exceptionLit.nonNegated, body = List(use3lit, x.negateThis)),
        use3lit.tostring -> x)).map { z => List(z._1, z._2) }.transpose
    val defeasible =
      (List(Clause(this.toLiteralList :+ exceptionLit)) ::: res.head.
        map { x => x.asInstanceOf[Clause] }).
        map { x => x.withTypePreds(globals, extraTypePreds = this.head.asLiteral.getTypePredicates(globals)) }
    val useMap = res(1).asInstanceOf[List[(String, Literal)]].groupBy(_._1).map { case (k, v) => v.head }
    (defeasible, useMap)
  }



  /**
   * Helper method that converts a clause to a List[Literal] with the head of the clause as the first element.
   */

  def toLiteralList = List(head.asLiteral) ++ (for (x <- body) yield x)

  /**
   * Returns this clause with type predicates in the body, for each variable that appears in the
   * clause. The optional input parameter is for adding extra type
   * predicates that cannot be be inferred from the head of the rule.
   * Examples of the latter are transformation rules where the heads consist
   * of auxiliary, fresh predicates, not included in the target
   * language and thus not described by the mode declarations.
   *
   */

  def withTypePreds(globals: Globals, extraTypePreds: List[String] = List()): Clause = {
    val types = (for (x <- this.toLiteralList)
      yield x.getTypePredicates(globals)).filter { z => z != Nil }.
      flatten.++(extraTypePreds).distinct.
      map { y => Literal.parse(y) }
    Clause(head = this.head, body = this.body ::: types)
  }






}

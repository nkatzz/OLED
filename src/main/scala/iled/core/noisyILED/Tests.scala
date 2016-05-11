package iled.core.noisyILED

import com.mongodb.casbah.commons.MongoDBObject
import iled.core.Core
import iled.globalValues.GlobalValues
import iled.structures.Examples.Example
import iled.utils.{CaviarUtils, Database}


/**
  * Created by nkatz on 27/2/2016.
  */
object Tests extends App{

  def mean(s: List[Double]) = s.foldLeft(0.0)(_ + _) / s.size

  def deviation(s: List[Double], mean: Double) = {
    val diffs = s map (x => math.abs(x - mean))
    this.mean(diffs)
  }


  //val intervals = iled.utils.CaviarUtils.getPositiveNegativeIntervals("leavingObject")
  /*
  val meetScores = List(0.8727273,0.7017544,0.9298246,0.7234042,0.99244624,0.19999999,0.96414346,0.8099174,0.85422474,0.7586207)
  println(mean(meetScores),deviation(meetScores,mean(meetScores)))
  // meet f-score: (0.780706303,+/- 0.14780918439999996)
  val meetTimes = List(31.251570466,31.363823715,34.08543294,34.132414622,28.145894052,34.379148478,32.98026956,28.515917435,28.76673843,34.674980495,27.761835024)
  println(mean(meetTimes),deviation(meetTimes,mean(meetTimes)))
  // meet times: 31.459820474272725 (+/- 2.3551170406611566)
  val meetSize = List(25,25,23,29,19,30,27,18,39,26) map (_.toDouble)
  println(mean(meetSize),deviation(meetSize,mean(meetSize)))
  // meet size: 26.1 (+/- 4.12)

  val moveScores = List(0.7421875,0.6655348,0.98294574,0.94930196,0.3795309,0.95297027,0.81213534,0.9878049,0.97183096,0.9502573)
  println(mean(moveScores),deviation(moveScores,mean(moveScores)))
  // move f-score: 0.839449967 (+/- 0.1516822656)
  val moveTimes = List(45.199426744,32.256120023,35.848163794,38.951609764,39.723582064,32.448944063,42.364593834,36.569196133,33.577792205,41.146593884)
  println(mean(moveTimes),deviation(moveTimes,mean(moveTimes)))
  // move time: 37.80860225079999 (+/- 3.668559007199999)
  val moveSize = List(50,30,27,35,36,7,28,21,22,44) map (_.toDouble)
  println(mean(moveSize),deviation(moveSize,mean(moveSize)))
  // move size: 30.0 (+/- 9.0)
*/



  //iled.utils.CaviarUtils.generateCleanData("meeting","/home/nkatz/dev/ILED/datasets/Caviar/meetingHandCrafted.lp")


  //val intervals = iled.utils.CaviarUtils.getPositiveNegativeIntervals("CAVIAR_Real_FixedBorders","moving")
  //val positiveInervals = intervals._1
  //val negativeIntervals = intervals._2

  //iled.utils.ParseCAVIAR.countInterpretations("/home/nkatz/dev/CAVIAR-abrupt-corrected-borderlines")

  /*
  def precision(tps: Int, fps: Int) = {
    tps.toDouble/(tps.toDouble+fps.toDouble)
  }
  def recall(tps: Int, fns: Int) = {
    tps.toDouble/(tps.toDouble+fns.toDouble)
  }
  def fscore(tps: Int, fps: Int, fns: Int) = {
    val p = precision(tps,fps)
    val r = recall(tps,fns)
    val f = (2*p*r)/(p+r)
    println(s"tps: $tps")
    println(s"fps: $fps")
    println(s"fns: $fns")
    println(s"precision: $p")
    println(s"recall: $r")
    println(s"f-score $f")
  }
  val tps = 3099
  val fps = 2588
  val fns = 3622 - tps
  fscore(tps,fps,fns)
  */

  //iled.utils.CaviarUtils.generateCleanData("moving","/home/nkatz/dev/ILED/datasets/hand-crafted-rules/caviar/moving-hand-crafted.lp")
  //val intervals = iled.utils.CaviarUtils.getPositiveNegativeIntervals("CAVIAR_moving_CLEAN","moving")


  //iled.utils.CaviarUtils.copyCAVIAR
  //iled.utils.CaviarUtils.mergeCaviarCopies

  val trainingSets = List(MeetingTrainingData.wholeCAVIAR1)
  val getData = CaviarUtils.getDataFromIntervals(new Database(Core.fromDB, "examples"),"meeting",this.trainingSets.head.trainingSet,4)

  val lengths = getData.foldLeft(List[Int]()){ (x,y) =>
    x :+ (y.exmplWithInertia.annotation++y.exmplWithInertia.narrative).length
  }
  val mean = iled.utils.Utils.mean(lengths.map(_.toDouble))
  println(mean)
}

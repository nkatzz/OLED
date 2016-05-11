package oled

import java.io.File
import iled.core.noisyILED.{MeetingTrainingData, MovingTrainingData}
import iled.core.noisyILED.experimentsMLNdata.MLNDataHandler
import iled.structures.Examples.Example
import iled.utils.{Database, CaviarUtils}
import iled.core.ExmplImplicits._
import scala.util.Random

/**
  * Created by nkatz on 10/4/2016.
  */
class WholeCaviarExperimentsData(DB: Database, HLE: String)  {

  //val HLE = "meeting"
  //val DB = "CAVIAR_Real_FixedBorders"

  val chunkSize = 100

  val MLNDataPath =
    if (HLE == "meeting") "/home/nkatz/dev/CAVIAR_MLN/CAVIAR_MLN/meet"
    else "/home/nkatz/dev/CAVIAR_MLN/CAVIAR_MLN/move"

  // First get all the MLN data training sets
  val d = new File(MLNDataPath)
  val innerFolders = d.listFiles.filter(x => x.getName.contains("fold")).sortBy(_.getName.split("_")(1).toInt)
  //val mlnDatasets = innerFolders map (f => MLNDataHandler.getData(f.getCanonicalPath, chunkSize))

  // Next, get the additional negative training data from the rest of CAVIAR
  val moveTestNegs = List(MovingTrainingData.testingNeg1,MovingTrainingData.testingNeg2,
    MovingTrainingData.testingNeg3,MovingTrainingData.testingNeg4,
    MovingTrainingData.testingNeg4,MovingTrainingData.testingNeg5,
    MovingTrainingData.testingNeg6,MovingTrainingData.testingNeg7,
    MovingTrainingData.testingNeg8,MovingTrainingData.testingNeg9,
    MovingTrainingData.testingNeg10)

  val meetTestNegs = List(MeetingTrainingData.testingNeg1,MovingTrainingData.testingNeg2,
    MeetingTrainingData.testingNeg3,MeetingTrainingData.testingNeg4,
    MeetingTrainingData.testingNeg4,MeetingTrainingData.testingNeg5,
    MeetingTrainingData.testingNeg6,MeetingTrainingData.testingNeg7,
    MeetingTrainingData.testingNeg8,MeetingTrainingData.testingNeg9,
    MeetingTrainingData.testingNeg10)

  def trainingNegs(x: List[CaviarUtils.Interval]) = {
    if (HLE == "meeting") MeetingTrainingData.allNegIntervals.filter( !x.contains(_) )
    else MovingTrainingData.allNegIntervals.filter( !x.contains(_) )
  }

  //val testingNegs = {
  //  if (HLE == "meeting") meetTestNegs else moveTestNegs
  //}

  // Couple each MLN dataset with additional training and testing negatives
  /*
  val jointDatasets: List[MLNDataHandler.TrainingSet] = mlnDatasets.toList map { x =>
    val t = if (HLE == "meeting") meetTestNegs else moveTestNegs
    val _testNegs = t(mlnDatasets.indexOf(x))
    val _trainNegs = trainingNegs(_testNegs)
    val trainNegs: List[Example] = CaviarUtils.getDataFromIntervals(new Database(DB), HLE, _trainNegs, chunkSize).toList
    val testNegs: List[Example] = CaviarUtils.getDataFromIntervals(new Database(DB), HLE, _testNegs, chunkSize, withChunking=false).toList

    // keep the head intact to make sure we're starting with positives
    val wholeTrainingSet: List[Example] = List(x.trainingData.head) ++ Random.shuffle(x.trainingData.tail ++ trainNegs)
    val wholeTestingSet: List[Example] = x.testingData ++ testNegs
    new MLNDataHandler.TrainingSet(trainingData=wholeTrainingSet, testingData=wholeTestingSet)
  }
  */
  def getDataForOneFold(x: MLNDataHandler.TrainingSet, index: Int) = {
    val t = if (HLE == "meeting") meetTestNegs else moveTestNegs
    val _testNegs = t(index)
    val _trainNegs = trainingNegs(_testNegs)
    val trainNegs: List[Example] = CaviarUtils.getDataFromIntervals(DB, HLE, _trainNegs, chunkSize).toList
    val testNegs: List[Example] = CaviarUtils.getDataFromIntervals(DB, HLE, _testNegs, chunkSize, withChunking=false).toList

    // keep the head intact to make sure we're starting with positives
    val wholeTrainingSet: List[Example] = List(x.trainingData.head) ++ Random.shuffle(x.trainingData.tail ++ trainNegs)
    val wholeTestingSet: List[Example] = x.testingData ++ testNegs
    new MLNDataHandler.TrainingSet(trainingData=wholeTrainingSet, testingData=wholeTestingSet)
  }


  //jointDatasets.foreach(x => println(x))



  // We now want to add to this MLN dataset additional training and testing negatives
}

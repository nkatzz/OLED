package iled.core.noisyILED

import iled.globalValues.GlobalValues
import iled.utils.CaviarUtils.Interval

/**
  * Created by nkatz on 3/1/16.
  */
object TrainingSetsHardCoded extends App{

/*

For each HLE and for each of its positive intervals, we'll create one dataset, where I is left out for testing
and we'll train on the rest of the intervals. As statistics of the data indicate (see below) approximately 2500
negatives must be kep out for testing. Here we create the datasets by hand.

This is how things look like for moving

moving: (2520,5800), length: 83
moving: (24440,27360), length: 74
moving: (460640,464200), length: 90
moving: (547400,559280), length: 298
moving: (565800,568120), length: 59
moving: (786240,791880), length: 142
moving: (797120,800440), length: 84
moving: (814880,829480), length: 366
moving: (843560,849440), length: 148
moving: (872000,884560), length: 315
moving: (885480,892520), length: 177
nothing: (680,2560), length: 48
nothing: (5760,24480), length: 469
nothing: (27320,460680), length: 10835
nothing: (464160,547440), length: 2083
nothing: (559240,565840), length: 166
nothing: (568080,786280), length: 5456
nothing: (791840,797160), length: 134
nothing: (800400,814920), length: 364
nothing: (829440,843600), length: 355
nothing: (849400,872040), length: 567
nothing: (884520,885520), length: 26
nothing: (892480,1077680), length: 4631
Average positive length: 167.0
Total negative length: 25134.0
Total positive length: 1836.0
90% of negatives (training set size) is 22620.600000000002
So negatives' testing set size is 2514.0
*/

  object TrainingSet {
    def apply() = {
      new TrainingSet(List(),List())
    }
  }
  class TrainingSet(val trainingSet: List[Interval], val testingSet: List[Interval])

  def getMovingTrainingSets() = {
    val intervals = iled.utils.CaviarUtils.getPositiveNegativeIntervals(GlobalValues.fromDB, "moving")
    val positiveInervals = intervals._1
    val negativeIntervals = intervals._2
    // For moving the negative test data will be the intervals with length 2083 + 364 + 26 + 48 = 2521, slightly bigger than needed
    // nothing: (464160,547440), length: 2083
    // nothing: (800400,814920), length: 364
    // nothing: (884520,885520), length: 26
    // nothing: (680,2560), length: 48
    val lengthTest = (is: List[Interval]) => {
      val f = (i: Interval) => i.length==2083 || i.length==364 || i.length==26 || i.length==48
      is.filter(f(_))
    }
    val negativeTestData = lengthTest(negativeIntervals)
    val negativeTrainingData = negativeIntervals filter (!negativeTestData.contains(_))
    val movingTrainingSets = positiveInervals map { p =>
      // create a training set with the rest of the positive intervals plus the training negative intervals
      val posTrainingSet = positiveInervals diff List(p)
      // keep the head out so that the training starts a positive interval and mix the rest with the negative intervals
      val trainingData = List(posTrainingSet.head) ++ scala.util.Random.shuffle(posTrainingSet.tail ++ negativeTrainingData)
      val testingData = List(p) ++ negativeTestData
      new TrainingSet(trainingData, testingData)
    }
    //movingTrainingSets foreach { x =>
    //  println(s"Training set:\n${x.trainingSet}\nTesting Set:\n${x.testingSet}")
    //}
    movingTrainingSets
  }



  def getMeetingTrainingSets() = {
    val intervals = iled.utils.CaviarUtils.getPositiveNegativeIntervals(GlobalValues.fromDB, "meeting")
    val positiveInervals = intervals._1.filter(x => x.startPoint!=1005760 && x.startPoint!=1009000)
    val negativeIntervals = intervals._2
    // For meeting the negative test data will be the intervals with length 1697 + 182 + 364 + 78 + 81 , slightly smaller than needed
    // nothing: (509760,559240), length: 1238
    //nothing: (843600,892480), length: 1223

    val lengthTest = (is: List[Interval]) => {
      val f = (i: Interval) => i.length==81 || i.length==78 || i.length==182 || i.length==364 || i.length==1697
      is.filter(f(_))
    }
    val negativeTestData = lengthTest(negativeIntervals)
    val negativeTrainingData = negativeIntervals filter (!negativeTestData.contains(_))
    val meetingTrainingSets = positiveInervals map { p =>
      // create a training set with the rest of the positive intervals plus the training negative intervals
      val posTrainingSet = positiveInervals diff List(p)
      // keep the head out so that the training starts a positive interval and mix the rest with the negative intervals
      val trainingData = List(posTrainingSet.head) ++ scala.util.Random.shuffle(posTrainingSet.tail ++ negativeTrainingData)
      val testingData = List(p) ++ negativeTestData
      new TrainingSet(trainingData, testingData)
    }
    //movingTrainingSets foreach { x =>
    //  println(s"Training set:\n${x.trainingSet}\nTesting Set:\n${x.testingSet}")
    //}
    meetingTrainingSets foreach println
    meetingTrainingSets
  }




















}

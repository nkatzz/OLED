package iled.core.noisyILED

import iled.utils.CaviarUtils.Interval

import scala.util.Random

/**
  * Created by nkatz on 3/22/16.
  */


object MovingTrainingData {

  /**
    * To find the intervals call:
    *
    *  val intervals = iled.utils.CaviarUtils.getPositiveNegativeIntervals("meeting")
    *  val positiveInervals = intervals._1
    *  val negativeIntervals = intervals._2
    *
    *
    */


/*
Average positive length: 167.0
Total negative length: 25134.0
Total positive length: 1836.0
90% of negatives (training set size) is 22620.600000000002
So negatives' testing set size is 2514.0
*/

  val movePos1 = Interval("moving",2520,5800)
  val movePos2 = Interval("moving",24440,27360)
  val movePos3 = Interval("moving",460640,464200)
  val movePos4 = Interval("moving",547400,559280)
  val movePos5 = Interval("moving",565800,568120)
  val movePos6 = Interval("moving",786240,791880)
  val movePos7 = Interval("moving",797120,800440)
  val movePos8 = Interval("moving",814880,829480)
  val movePos9 = Interval("moving",843560,849440)
  val movePos10 = Interval("moving",872000,884560)
  val movePos11 = Interval("moving",885480,892520)

  // To break large intervals in smaller of 1000 data points use this (40 is the step):
  // List.range(568080,786280,40).grouped(1000).map(x => (x.head,x.tail.reverse.head)) foreach println
  val moveNeg1 = Interval("moving",680,2560)      //  48
  val moveNeg2 = Interval("moving",5760,24480)    //  469
  val moveNeg3 = Interval("moving",27320,67280)   // 1000
  val moveNeg4 = Interval("moving",67320,107280)  //1000
  val moveNeg5 = Interval("moving",107320,147280) //1000
  val moveNeg6 = Interval("moving",147320,187280) //1000
  val moveNeg7 = Interval("moving",187320,227280) //1000
  val moveNeg8 = Interval("moving",227320,267280) //1000
  val moveNeg9 = Interval("moving",267320,307280) //1000
  val moveNeg10 = Interval("moving",307320,347280)//1000
  val moveNeg11 = Interval("moving",347320,387280)//1000
  val moveNeg12 = Interval("moving",387320,427280)//1000
  val moveNeg13 = Interval("moving",427320,460640)//834
  val moveNeg14 = Interval("moving",464160,504120)//1000
  val moveNeg15 = Interval("moving",504160,544120)//1000
  val moveNeg16 = Interval("moving",544160,547400)//82
  val moveNeg17 = Interval("moving",559240,565840)//166
  val moveNeg18 = Interval("moving",568080,608040)//1000
  val moveNeg19 = Interval("moving",608080,648040)//1000
  val moveNeg20 = Interval("moving",648080,688040)//1000
  val moveNeg21 = Interval("moving",688080,728040)//1000
  val moveNeg22 = Interval("moving",728080,768040)//1000
  val moveNeg23 = Interval("moving",768080,786240)//455
  val moveNeg24 = Interval("moving",791840,797160)//134
  val moveNeg25 = Interval("moving",800400,814920)//364
  val moveNeg26 = Interval("moving",829440,843600)//355
  val moveNeg27 = Interval("moving",849400,872040)//567
  val moveNeg28 = Interval("moving",884520,885520)//26

  val moveNeg29 = Interval("moving",892480,932440)//1000
  val moveNeg30 = Interval("moving",932480,972440)//1000
  val moveNeg31 = Interval("moving",972480,1012440)//1000
  val moveNeg32 = Interval("moving",1012480,1052440)//1000
  val moveNeg33 = Interval("moving",1052480,1077640)//1000


  val allNegIntervals = List(moveNeg1,moveNeg2,moveNeg3,moveNeg4,moveNeg5,moveNeg6,moveNeg7,moveNeg8,moveNeg9,moveNeg10,moveNeg11,moveNeg12,moveNeg13,moveNeg14,
    moveNeg15,moveNeg16,moveNeg17,moveNeg18,moveNeg19,moveNeg20,moveNeg21,moveNeg22,moveNeg23,moveNeg24,moveNeg25,moveNeg26,moveNeg27,
    moveNeg28,moveNeg29,moveNeg30,moveNeg31,moveNeg32,moveNeg33)



  val allPosIntervals = List(movePos1,movePos2,movePos3,movePos4,movePos5,movePos6,movePos7,movePos8,movePos9,movePos10,movePos11)

  val testingNeg1 = List(moveNeg1,moveNeg2,moveNeg3,moveNeg31)
  val testingNeg2 = List(moveNeg4,moveNeg5,moveNeg6)
  val testingNeg3 = List(moveNeg7,moveNeg8,moveNeg9)
  val testingNeg4 = List(moveNeg10,moveNeg11,moveNeg12)
  val testingNeg5 = List(moveNeg13,moveNeg14,moveNeg15)
  val testingNeg6 = List(moveNeg16,moveNeg17,moveNeg18)
  val testingNeg7 = List(moveNeg19,moveNeg20,moveNeg21)
  val testingNeg8 = List(moveNeg21,moveNeg23,moveNeg24,moveNeg33)
  val testingNeg9 = List(moveNeg25,moveNeg26,moveNeg27,moveNeg32)
  val testingNeg10 = List(moveNeg28,moveNeg29,moveNeg30)


  val allNegativeTestingSetIntervals =
    List(testingNeg1,testingNeg2,testingNeg3,testingNeg4,testingNeg5,testingNeg6,testingNeg7,testingNeg8,testingNeg9,testingNeg10)

  // Training set 1. All but movePos1
  //----------------------------------
  val moveTrainingSet1 = {
    val training = allPosIntervals.filter(x => x!= movePos1 && x!= movePos11) ++ allNegIntervals.filter(z => !testingNeg1.contains(z))
    val testing = List(movePos1,movePos11) ++ testingNeg1
    new TrainingSet(trainingSet = List(training.head) ++ Random.shuffle(training.tail), testingSet = testing)
  }

  // Training set 2. All but movePos2
  //----------------------------------
  val moveTrainingSet2 = {
    val training = allPosIntervals.filter(x => x!= movePos2) ++ allNegIntervals.filter(z => !testingNeg2.contains(z))
    val testing = List(movePos2) ++ testingNeg2
    new TrainingSet(trainingSet = List(training.head) ++ Random.shuffle(training.tail), testingSet = testing)
  }

  // Training set 3. All but movePos3
  //----------------------------------
  val moveTrainingSet3 = {
    val training = allPosIntervals.filter(x => x!= movePos3) ++ allNegIntervals.filter(z => !testingNeg3.contains(z))
    val testing = List(movePos3) ++ testingNeg3
    new TrainingSet(trainingSet = List(training.head) ++ Random.shuffle(training.tail), testingSet = testing)
  }

  // Training set 4. All but movePos4
  //----------------------------------
  val moveTrainingSet4 = {
    val training = allPosIntervals.filter(x => x!= movePos4) ++ allNegIntervals.filter(z => !testingNeg4.contains(z))
    val testing = List(movePos4) ++ testingNeg4
    new TrainingSet(trainingSet = List(training.head) ++ Random.shuffle(training.tail), testingSet = testing)
  }

  // Training set 5. All but movePos5
  //----------------------------------
  val moveTrainingSet5 = {
    val training = allPosIntervals.filter(x => x!= movePos5) ++ allNegIntervals.filter(z => !testingNeg5.contains(z))
    val testing = List(movePos5) ++ testingNeg5
    new TrainingSet(trainingSet = List(training.head) ++ Random.shuffle(training.tail), testingSet = testing)
  }

  // Training set 6. All but movePos6
  //----------------------------------
  val moveTrainingSet6 = {
    val training = allPosIntervals.filter(x => x!= movePos6) ++ allNegIntervals.filter(z => !testingNeg6.contains(z))
    val testing = List(movePos6) ++ testingNeg6
    new TrainingSet(trainingSet = List(training.head) ++ Random.shuffle(training.tail), testingSet = testing)
  }

  // Training set 7. All but movePos7
  //----------------------------------
  val moveTrainingSet7 = {
    val training = allPosIntervals.filter(x => x!= movePos7) ++ allNegIntervals.filter(z => !testingNeg7.contains(z))
    val testing = List(movePos7) ++ testingNeg7
    new TrainingSet(trainingSet = List(training.head) ++ Random.shuffle(training.tail), testingSet = testing)
  }

  // Training set 8. All but movePos8
  //----------------------------------
  val moveTrainingSet8 = {
    val training = allPosIntervals.filter(x => x!= movePos8) ++ allNegIntervals.filter(z => !testingNeg8.contains(z))
    val testing = List(movePos8) ++ testingNeg8
    new TrainingSet(trainingSet = List(training.head) ++ Random.shuffle(training.tail), testingSet = testing)
  }

  // Training set 9. All but movePos9
  //----------------------------------
  val moveTrainingSet9 = {
    val training = allPosIntervals.filter(x => x!= movePos9) ++ allNegIntervals.filter(z => !testingNeg9.contains(z))
    val testing = List(movePos9) ++ testingNeg9
    new TrainingSet(trainingSet = List(training.head) ++ Random.shuffle(training.tail), testingSet = testing)
  }

  // Training set 10. All but movePos10
  //----------------------------------
  val moveTrainingSet10 = {
    val training = allPosIntervals.filter(x => x!= movePos10) ++ allNegIntervals.filter(z => !testingNeg10.contains(z))
    val testing =  List(movePos10) ++ testingNeg10
    new TrainingSet(trainingSet = List(training.head) ++ Random.shuffle(training.tail), testingSet = testing)
  }

  val allTrainingSets = List(moveTrainingSet1,moveTrainingSet2,moveTrainingSet3,moveTrainingSet4,moveTrainingSet5,moveTrainingSet6,moveTrainingSet7,moveTrainingSet8,
    moveTrainingSet9,moveTrainingSet10)

  val wholeCAVIAR1 = {
    new TrainingSet(trainingSet = List(allPosIntervals.head) ++ Random.shuffle(allPosIntervals++allNegIntervals), testingSet = allPosIntervals++allNegIntervals)
  }

  val wholeCAVIAR = {
    new TrainingSet(trainingSet = List(), testingSet = allPosIntervals++allNegIntervals)
  }













}

package oled.whole_caviar_data

import utils.DataUtils.{TrainingSet, DataAsIntervals, Interval}

import scala.util.Random

/**
  * Created by nkatz on 3/23/16.
  */
object MeetingCleanTrainingData {

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
Average positive length: 234.0
Total negative length: 23249.0
Total positive length: 3739.0
90% of negatives (training set size) is 20924.100000000002
So negatives' testing set size is 2325.0
*/
  val meetPos1 = Interval("meeting",6200,24480)      // length:458
  val meetPos2 = Interval("meeting",27000,63160)     // length:905
  val meetPos3 = Interval("meeting",585200,585960)   // length:20
  val meetPos4 = Interval("meeting",600520,601640)   // length:29
  val meetPos5 = Interval("meeting",622440,624840)   // length: 61
  val meetPos6 = Interval("meeting",638720,641320)   // length: 66
  val meetPos7 = Interval("meeting",714360,720040)   // length: 143
  val meetPos8 = Interval("meeting",746160,747600)   // length: 37
  val meetPos9 = Interval("meeting",765280,766720)   // length: 37
  val meetPos10 = Interval("meeting",785440,791880)  // length: 162
  val meetPos11 = Interval("meeting",812520,835520)  // length: 576
  val meetPos12 = Interval("meeting",842320,850120)  // length: 196
  val meetPos13 = Interval("meeting",892000,896240)  // length: 107
  val meetPos14 = Interval("meeting",1001320,1003200)// length: 48
  val meetPos15 = Interval("meeting",1008640,1010480)// length: 47
  val meetPos16 = Interval("meeting",1045200,1048280)// length: 78
  val meetPos17 = Interval("meeting",1072360,1077680)//length: 134

  val allPosIntervals = List(meetPos1,meetPos2,meetPos3,meetPos4,meetPos5,meetPos6,meetPos7,meetPos8,
    meetPos9,meetPos10,meetPos11,meetPos12,meetPos13,meetPos14,meetPos15,meetPos16,meetPos17)

  val testingPos1 = List(meetPos1)
  val testingPos2 = List(meetPos2)
  val testingPos3 = List(meetPos3,meetPos4,meetPos17)
  val testingPos4 = List(meetPos5,meetPos6)
  val testingPos5 = List(meetPos7,meetPos8)
  val testingPos6 = List(meetPos9,meetPos10)
  val testingPos7 = List(meetPos11,meetPos12)
  val testingPos8 = List(meetPos13,meetPos14)
  val testingPos9 = List(meetPos15)
  val testingPos10 = List(meetPos16)

  val meetNeg1 = Interval("meeting",680,6240) //length: 140
  val meetNeg2 = Interval("meeting",24440,27040)    //length: 66
  val meetNeg3 = Interval("meeting",63120,143080)   //2000
  val meetNeg4 = Interval("meeting",143120,223080)   //2000
  val meetNeg5 = Interval("meeting",223120,303080)   //2000
  val meetNeg6 = Interval("meeting",303120,383080)   //2000
  val meetNeg7 = Interval("meeting",383120,463080)   //2000
  val meetNeg8 = Interval("meeting",463120,543080)   //2000
  val meetNeg9 = Interval("meeting",543120,585200)   //2000
  val meetNeg10 = Interval("meeting",585920,600560)  //367
  val meetNeg11 = Interval("meeting",601600,622480)  //523
  val meetNeg12 = Interval("meeting",624800,638760)  //350
  val meetNeg13 = Interval("meeting",641280,714400)  //1829
  val meetNeg14 = Interval("meeting",720000,746200)  //656
  val meetNeg15 = Interval("meeting",747560,765320)  //445
  val meetNeg16 = Interval("meeting",766680,785480)  //471
  val meetNeg17 = Interval("meeting",791840,812560)  //519
  val meetNeg18 = Interval("meeting",835480,842360)  //173
  val meetNeg19 = Interval("meeting",850080,892040) //1050
  val meetNeg20 = Interval("meeting",896200,1001360)//2630
  val meetNeg21 = Interval("meeting",1003160,1008680)//139
  val meetNeg22 = Interval("meeting",1010440,1045240)//871
  val meetNeg23 = Interval("meeting",1048240,1072400)//605

  val allNegIntervals =
    List(meetNeg1,meetNeg2,meetNeg3,meetNeg4,meetNeg5,meetNeg6,meetNeg7,meetNeg8,meetNeg9,meetNeg10,meetNeg11,meetNeg12,meetNeg13,meetNeg14,meetNeg15,meetNeg16,
      meetNeg17,meetNeg18,meetNeg19,meetNeg20,meetNeg21,meetNeg22,meetNeg23)

  //allNegIntervals.foreach(x => println(x.length))

  val testingNeg1 = List(meetNeg1,meetNeg2,meetNeg8)
  val testingNeg2 = List(meetNeg3,meetNeg4,meetNeg5)
  val testingNeg3 = List(meetNeg6,meetNeg7,meetNeg9)
  val testingNeg4 = List(meetNeg10,meetNeg11,meetNeg10)
  val testingNeg5 = List(meetNeg13,meetNeg14,meetNeg15)
  val testingNeg6 = List(meetNeg16,meetNeg17,meetNeg18)
  val testingNeg7 = List(meetNeg12,meetNeg19)
  val testingNeg8 = List(meetNeg20,meetNeg21)
  val testingNeg9 = List(meetNeg22)
  val testingNeg10 = List(meetNeg23)

  // Training set 1. All but meetPos1
  //----------------------------------
  val meetTrainingSet1 = {
    val training = allPosIntervals.filter(x => !testingPos1.contains(x)) ++ allNegIntervals.filter(z => !testingNeg1.contains(z))
    val testing = testingPos1 ++ testingNeg1
    new DataAsIntervals(trainingSet = List(training.head) ++ Random.shuffle(training.tail), testingSet = testing)
  }

  val meetTrainingSet2 = {
    val training = allPosIntervals.filter(x => !testingPos2.contains(x)) ++ allNegIntervals.filter(z => !testingNeg2.contains(z))
    val testing = testingPos2 ++ testingNeg2
    new DataAsIntervals(trainingSet = List(training.head) ++ Random.shuffle(training.tail), testingSet = testing)
  }

  val meetTrainingSet3 = {
    val training = allPosIntervals.filter(x => !testingPos3.contains(x)) ++ allNegIntervals.filter(z => !testingNeg3.contains(z))
    val testing = testingPos3 ++ testingNeg3
    new DataAsIntervals(trainingSet = List(training.head) ++ Random.shuffle(training.tail), testingSet = testing)
  }

  val meetTrainingSet4 = {
    val training = allPosIntervals.filter(x => !testingPos4.contains(x)) ++ allNegIntervals.filter(z => !testingNeg4.contains(z))
    val testing = testingPos4 ++ testingNeg4
    new DataAsIntervals(trainingSet = List(training.head) ++ Random.shuffle(training.tail), testingSet = testing)
  }

  val meetTrainingSet5 = {
    val training = allPosIntervals.filter(x => !testingPos5.contains(x)) ++ allNegIntervals.filter(z => !testingNeg5.contains(z))
    val testing = testingPos5 ++ testingNeg5
    new DataAsIntervals(trainingSet = List(training.head) ++ Random.shuffle(training.tail), testingSet = testing)
  }

  val meetTrainingSet6 = {
    val training = allPosIntervals.filter(x => !testingPos6.contains(x)) ++ allNegIntervals.filter(z => !testingNeg6.contains(z))
    val testing = testingPos6 ++ testingNeg6
    new DataAsIntervals(trainingSet = List(training.head) ++ Random.shuffle(training.tail), testingSet = testing)
  }

  val meetTrainingSet7 = {
    val training = allPosIntervals.filter(x => !testingPos7.contains(x)) ++ allNegIntervals.filter(z => !testingNeg7.contains(z))
    val testing = testingPos7 ++ testingNeg7
    new DataAsIntervals(trainingSet = List(training.head) ++ Random.shuffle(training.tail), testingSet = testing)
  }

  val meetTrainingSet8 = {
    val training = allPosIntervals.filter(x => !testingPos8.contains(x)) ++ allNegIntervals.filter(z => !testingNeg8.contains(z))
    val testing = testingPos8 ++ testingNeg8
    new DataAsIntervals(trainingSet = List(training.head) ++ Random.shuffle(training.tail), testingSet = testing)
  }

  val meetTrainingSet9 = {
    val training = allPosIntervals.filter(x => !testingPos9.contains(x)) ++ allNegIntervals.filter(z => !testingNeg9.contains(z))
    val testing = testingPos9 ++ testingNeg9
    new DataAsIntervals(trainingSet = List(training.head) ++ Random.shuffle(training.tail), testingSet = testing)
  }

  val meetTrainingSet10 = {
    val training = allPosIntervals.filter(x => !testingPos10.contains(x)) ++ allNegIntervals.filter(z => !testingNeg10.contains(z))
    val testing = testingPos10 ++ testingNeg10
    new DataAsIntervals(trainingSet = List(training.head) ++ Random.shuffle(training.tail), testingSet = testing)
  }


  val allTrainingSets = List(meetTrainingSet1,meetTrainingSet2,meetTrainingSet3,meetTrainingSet4,meetTrainingSet5,meetTrainingSet6,meetTrainingSet7,
    meetTrainingSet8,meetTrainingSet9,meetTrainingSet10)

  val wholeCAVIARForManualRules = {
    new DataAsIntervals(trainingSet = List(), testingSet = allPosIntervals++allNegIntervals)
  }

  val wholeCAVIARForTraining = {
    new DataAsIntervals(trainingSet = allPosIntervals++allNegIntervals, testingSet = allPosIntervals++allNegIntervals)
  }

}

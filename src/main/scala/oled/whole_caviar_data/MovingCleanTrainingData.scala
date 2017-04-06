package oled.whole_caviar_data


import utils.DataUtils.{DataAsIntervals, Interval}

import scala.util.Random

/**
  * Created by nkatz on 4/18/16.
  */

object MovingCleanTrainingData {

  val movePos1 = Interval("moving",2520,6880)     //length: 110
  val movePos2 = Interval("moving",24440,27080)   //length: 67
  val movePos3 = Interval("moving",224040,225880) //length: 47
  val movePos4 = Interval("moving",547400,551680) //length: 108
  val movePos5 = Interval("moving",551640,551960) //length: 9
  val movePos6 = Interval("moving",551920,552600) //length: 18
  val movePos7 = Interval("moving",552920,553040) //length: 4
  val movePos8 = Interval("moving",553080,553880) //length: 21
  val movePos9 = Interval("moving",557240,559120) //length: 48
  val movePos10 = Interval("moving",604240,606720)//length: 63

  val movePos11 = Interval("moving",785080,785480)//length: 11
  val movePos12 = Interval("moving",786360,791880)//length: 139
  val movePos13 = Interval("moving",797120,800440)//length: 84
  val movePos14 = Interval("moving",814840,829480)//length: 367
  val movePos15 = Interval("moving",841120,841240)//length: 4
  val movePos16 = Interval("moving",841240,850120)//length: 223
  val movePos17 = Interval("moving",850200,852520)//length: 59
  val movePos18 = Interval("moving",872000,881640)//length: 242
  val movePos19 = Interval("moving",881640,882680)//length: 27
  val movePos20 = Interval("moving",882840,883360)//length: 14

  val movePos21 = Interval("moving",884160,884280)//length: 4
  val movePos22 = Interval("moving",884240,884560)//length: 9
  val movePos23 = Interval("moving",885480,892360)//length: 173
  val movePos24 = Interval("moving",895480,896240)//length: 20
  val movePos25 = Interval("moving",919920,922560)//length: 67
  val movePos26 = Interval("moving",950600,951680)//length: 28
  val movePos27 = Interval("moving",960840,961160)// length: 9
  val movePos28 = Interval("moving",1010840,1013720)//length: 73
  val movePos29 = Interval("moving",1037560,1038600)//length: 27
  val movePos30 = Interval("moving",1045200,1045320)//length: 4
  val movePos31 = Interval("moving",1071840,1075240)//length: 86

  val allPosIntervals = List(movePos1,movePos2,movePos3,movePos4,movePos5,movePos6,movePos7,movePos8,movePos9,movePos10,movePos11,movePos12,movePos13,movePos14,
    movePos15,movePos16,movePos17,movePos18,movePos19,movePos20,movePos21,movePos22,movePos23,movePos24,movePos25,movePos26,movePos27,movePos28,movePos29,
    movePos30,movePos31)

  val testingPos1 = List(movePos1,movePos2,movePos3)
  val testingPos2 = List(movePos4,movePos5,movePos6)
  val testingPos3 = List(movePos7,movePos8,movePos9)
  val testingPos4 = List(movePos10,movePos11,movePos12)
  val testingPos5 = List(movePos13,movePos14,movePos15)
  val testingPos6 = List(movePos16,movePos17,movePos18)
  val testingPos7 = List(movePos19,movePos20,movePos21)
  val testingPos8 = List(movePos22,movePos23,movePos24)
  val testingPos9 = List(movePos25,movePos26,movePos27)
  val testingPos10 = List(movePos28,movePos29,movePos30,movePos31)


  val moveNeg1 = Interval("moving",680,2560) //length: 48
  val moveNeg2 = Interval("moving",6840,24480) //length: 442
  val moveNeg3 = Interval("moving",27040,224080) //length: 4927
  val moveNeg4 = Interval("moving",225840,547440) //length: 8041
  val moveNeg5 = Interval("moving",551640,551680) //length: 2
  val moveNeg6 = Interval("moving",551920,551960) //length: 2
  val moveNeg7 = Interval("moving",552560,552960) //length: 11
  val moveNeg8 = Interval("moving",553000,553120) //length: 4
  val moveNeg9 = Interval("moving",553840,557280) //length: 87
  val moveNeg10 = Interval("moving",559080,604280) //length: 1131
  val moveNeg11 = Interval("moving",606680,785120) //length: 4462
  val moveNeg12 = Interval("moving",785440,786400) //length: 25
  val moveNeg13 = Interval("moving",791840,797160) //length: 134
  val moveNeg14 = Interval("moving",800400,814880) //length: 363
  val moveNeg15 = Interval("moving",829440,841160) // length: 294
  val moveNeg16 = Interval("moving",841200,841280) // length: 3
  val moveNeg17 = Interval("moving",850080,850240) //length: 5
  val moveNeg18 = Interval("moving",852480,872040) //length: 490
  val moveNeg19 = Interval("moving",881600,881680) //length: 3

  val moveNeg20 = Interval("moving",882640,882880) //length: 7
  val moveNeg21 = Interval("moving",883320,884200) // length: 23
  val moveNeg22 = Interval("moving",884240,884280) //length: 2
  val moveNeg23 = Interval("moving",884520,885520) //length: 26
  val moveNeg24 = Interval("moving",892320,895520) //length: 81
  val moveNeg25 = Interval("moving",896200,919960) //length: 595
  val moveNeg26 = Interval("moving",922520,950640) //length: 704
  val moveNeg27 = Interval("moving",951640,960880) //length: 232
  val moveNeg28 = Interval("moving",961120,1010880) //length: 1245
  val moveNeg29 = Interval("moving",1013680,1037600) //length: 599
  val moveNeg30 = Interval("moving",1038560,1045240) //length: 168
  val moveNeg31 = Interval("moving",1045280,1071880) //length: 666
  val moveNeg32 = Interval("moving",1075200,1077680) //length: 63


  val allNegIntervals = List(moveNeg1,moveNeg2,moveNeg3,moveNeg4,moveNeg5,moveNeg6,moveNeg7,moveNeg8,moveNeg9,moveNeg10,moveNeg11,moveNeg12,moveNeg13,
    moveNeg14,moveNeg15,moveNeg16,moveNeg17,moveNeg18,moveNeg19,moveNeg20,moveNeg21,moveNeg22,moveNeg23,moveNeg24,moveNeg25,moveNeg26,moveNeg27,
    moveNeg28,moveNeg29,moveNeg30,moveNeg31,moveNeg32)

  val testingNeg1 = List(moveNeg3)
  val testingNeg2 = List(moveNeg4)
  val testingNeg3 = List(moveNeg1,moveNeg2,moveNeg5,moveNeg6)
  val testingNeg4 = List(moveNeg7,moveNeg8,moveNeg9,moveNeg18,moveNeg19,moveNeg20)
  val testingNeg5 = List(moveNeg10)
  val testingNeg6 = List(moveNeg11)
  val testingNeg7 = List(moveNeg12,moveNeg13,moveNeg14,moveNeg15,moveNeg16,moveNeg17)
  val testingNeg8 = List(moveNeg21,moveNeg22,moveNeg23,moveNeg24,moveNeg25)
  val testingNeg9 = List(moveNeg26,moveNeg27,moveNeg29,moveNeg30,moveNeg32)
  val testingNeg10 = List(moveNeg28,moveNeg31)

  val moveTrainingSet1 = {
    val training = allPosIntervals.filter(x => !testingPos1.contains(x)) ++ allNegIntervals.filter(z => !testingNeg1.contains(z))
    val testing = testingPos1 ++ testingNeg1
    new DataAsIntervals(trainingSet = List(training.head) ++ Random.shuffle(training.tail), testingSet = testing)
  }

  val moveTrainingSet2 = {
    val training = allPosIntervals.filter(x => !testingPos2.contains(x)) ++ allNegIntervals.filter(z => !testingNeg2.contains(z))
    val testing = testingPos2 ++ testingNeg2
    new DataAsIntervals(trainingSet = List(training.head) ++ Random.shuffle(training.tail), testingSet = testing)
  }

  val moveTrainingSet3 = {
    val training = allPosIntervals.filter(x => !testingPos3.contains(x)) ++ allNegIntervals.filter(z => !testingNeg3.contains(z))
    val testing = testingPos3 ++ testingNeg3
    new DataAsIntervals(trainingSet = List(training.head) ++ Random.shuffle(training.tail), testingSet = testing)
  }

  val moveTrainingSet4 = {
    val training = allPosIntervals.filter(x => !testingPos4.contains(x)) ++ allNegIntervals.filter(z => !testingNeg4.contains(z))
    val testing = testingPos4 ++ testingNeg4
    new DataAsIntervals(trainingSet = List(training.head) ++ Random.shuffle(training.tail), testingSet = testing)
  }

  val moveTrainingSet5 = {
    val training = allPosIntervals.filter(x => !testingPos5.contains(x)) ++ allNegIntervals.filter(z => !testingNeg5.contains(z))
    val testing = testingPos5 ++ testingNeg5
    new DataAsIntervals(trainingSet = List(training.head) ++ Random.shuffle(training.tail), testingSet = testing)
  }

  val moveTrainingSet6 = {
    val training = allPosIntervals.filter(x => !testingPos6.contains(x)) ++ allNegIntervals.filter(z => !testingNeg6.contains(z))
    val testing = testingPos6 ++ testingNeg6
    new DataAsIntervals(trainingSet = List(training.head) ++ Random.shuffle(training.tail), testingSet = testing)
  }

  val moveTrainingSet7 = {
    val training = allPosIntervals.filter(x => !testingPos7.contains(x)) ++ allNegIntervals.filter(z => !testingNeg7.contains(z))
    val testing = testingPos7 ++ testingNeg7
    new DataAsIntervals(trainingSet = List(training.head) ++ Random.shuffle(training.tail), testingSet = testing)
  }

  val moveTrainingSet8 = {
    val training = allPosIntervals.filter(x => !testingPos8.contains(x)) ++ allNegIntervals.filter(z => !testingNeg8.contains(z))
    val testing = testingPos8 ++ testingNeg8
    new DataAsIntervals(trainingSet = List(training.head) ++ Random.shuffle(training.tail), testingSet = testing)
  }

  val moveTrainingSet9 = {
    val training = allPosIntervals.filter(x => !testingPos9.contains(x)) ++ allNegIntervals.filter(z => !testingNeg9.contains(z))
    val testing = testingPos9 ++ testingNeg9
    new DataAsIntervals(trainingSet = List(training.head) ++ Random.shuffle(training.tail), testingSet = testing)
  }

  val moveTrainingSet10 = {
    val training = allPosIntervals.filter(x => !testingPos10.contains(x)) ++ allNegIntervals.filter(z => !testingNeg10.contains(z))
    val testing = testingPos10 ++ testingNeg10
    new DataAsIntervals(trainingSet = List(training.head) ++ Random.shuffle(training.tail), testingSet = testing)
  }

  val allTrainingSets = List(moveTrainingSet1,moveTrainingSet2,moveTrainingSet3,moveTrainingSet4,moveTrainingSet5,moveTrainingSet6,moveTrainingSet7,moveTrainingSet8,
    moveTrainingSet9,moveTrainingSet10)

}

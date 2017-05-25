package oled.whole_caviar_data

import utils.DataUtils.{DataAsIntervals, Interval}

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

  val movePos1 = Interval("moving",2480,5760) //length: 83
  val movePos2 = Interval("moving",24400,27320) // length: 74
  val movePos3 = Interval("moving",460600,464160) // length: 90
  val movePos4 = Interval("moving",547360,559240) // length: 298
  val movePos5 = Interval("moving",565760,568120) // length: 60
  val movePos6 = Interval("moving",786200,791880) // length: 143
  val movePos7 = Interval("moving",797080,800440) // length: 85
  val movePos8 = Interval("moving",814840,829440) // length: 366
  val movePos9 = Interval("moving",841240,844760) // length: 89
  val movePos10 = Interval("moving",868440,874320) // length: 148
  val movePos11 = Interval("moving",896880,909480) // length: 316
  val movePos12 = Interval("moving",910360,917400) // length: 177

  // To break large intervals in smaller of 1000 data points use this (40 is the step):
  // List.range(568080,786280,40).grouped(1000).map(x => (x.head,x.tail.reverse.head)) foreach println
  val moveNeg1 = Interval("moving",680,2520)      //  47
  val moveNeg2 = Interval("moving",5720,24440)    //  469
  val moveNeg3 = Interval("moving",27280,67240)   // 1000
  val moveNeg4 = Interval("moving",67280,107240)  //1000
  val moveNeg5 = Interval("moving",107280,147240) //1000
  val moveNeg6 = Interval("moving",147280,187240) //1000
  val moveNeg7 = Interval("moving",187280,227240) //1000
  val moveNeg8 = Interval("moving",227280,267240) //1000
  val moveNeg9 = Interval("moving",267280,307240) //1000
  val moveNeg10 = Interval("moving",307280,347240)//1000
  val moveNeg11 = Interval("moving",347280,387240)//1000
  val moveNeg12 = Interval("moving",387280,427240)//1000
  val moveNeg13 = Interval("moving",427280,460600)//1000
  val moveNeg14 = Interval("moving",464120,547400)//2083
  val moveNeg15 = Interval("moving",559200,565800)//166
  val moveNeg16 = Interval("moving",568080,608040)//1000
  val moveNeg17 = Interval("moving",608080,648040)//1000
  val moveNeg18 = Interval("moving",648080,688040)//1000
  val moveNeg19 = Interval("moving",688080,728040)//1000
  val moveNeg20 = Interval("moving",728080,768040)//1000
  val moveNeg21 = Interval("moving",768080,786200)//1000
  val moveNeg22 = Interval("moving",791840,797120)//133
  val moveNeg23 = Interval("moving",800400,814880)//363
  val moveNeg24 = Interval("moving",829400,841280)//298
  val moveNeg25 = Interval("moving",844720,868480)//595
  val moveNeg26 = Interval("moving",874280,896920)//567
  val moveNeg27 = Interval("moving",909440,910400)//25
  val moveNeg28 = Interval("moving",917360,957320)//1000
  val moveNeg29 = Interval("moving",957360,997320)//1000
  val moveNeg30 = Interval("moving",997360,1037320)//1000
  val moveNeg31 = Interval("moving",1037360,1077320)//1000
  val moveNeg32 = Interval("moving",1077360,1102560)//1000



  val allNegIntervals = List(moveNeg1,moveNeg2,moveNeg3,moveNeg4,moveNeg5,moveNeg6,moveNeg7,moveNeg8,moveNeg9,moveNeg10,moveNeg11,moveNeg12,moveNeg13,moveNeg14,
    moveNeg15,moveNeg16,moveNeg17,moveNeg18,moveNeg19,moveNeg20,moveNeg21,moveNeg22,moveNeg23,moveNeg24,moveNeg25,moveNeg26,moveNeg27,
    moveNeg28,moveNeg29,moveNeg30,moveNeg31,moveNeg32)



  val allPosIntervals = List(movePos1,movePos2,movePos3,movePos4,movePos5,movePos6,movePos7,movePos8,movePos9,movePos10,movePos11,movePos12)

  val testingNeg1 = List(moveNeg1,moveNeg2,moveNeg3,moveNeg31)
  val testingNeg2 = List(moveNeg4,moveNeg5,moveNeg6)
  val testingNeg3 = List(moveNeg7,moveNeg8,moveNeg9)
  val testingNeg4 = List(moveNeg10,moveNeg11,moveNeg12)
  val testingNeg5 = List(moveNeg13,moveNeg14,moveNeg15)
  val testingNeg6 = List(moveNeg16,moveNeg17,moveNeg18)
  val testingNeg7 = List(moveNeg19,moveNeg20,moveNeg21)
  val testingNeg8 = List(moveNeg21,moveNeg23,moveNeg24)
  val testingNeg9 = List(moveNeg26,moveNeg27,moveNeg32)
  val testingNeg10 = List(moveNeg28,moveNeg29,moveNeg30)

  val allNegativeTestingSetIntervals =
    List(testingNeg1,testingNeg2,testingNeg3,testingNeg4,testingNeg5,testingNeg6,testingNeg7,testingNeg8,testingNeg9,testingNeg10)

  // Training set 1. All but movePos1 & movePos12
  //----------------------------------------------
  val moveTrainingSet1 = {
    //val training = allPosIntervals.filter(x => x!= movePos1 && x!= movePos12) ++ allNegIntervals.filter(z => !testingNeg1.contains(z))
    val training =
      List(Interval("moving",24400,27320),
        Interval("moving",688080,728040),
        Interval("moving",896880,909480),
        Interval("moving",868440,874320),
        Interval("moving",147280,187240),
        Interval("moving",107280,147240),
        Interval("moving",800400,814880),
        Interval("moving",460600,464160),
        Interval("moving",565760,568120),
        Interval("moving",267280,307240),
        Interval("moving",568080,608040),
        Interval("moving",227280,267240),
        Interval("moving",874280,896920),
        Interval("moving",307280,347240),
        Interval("moving",1077360,1102560),
        Interval("moving",559200,565800),
        Interval("moving",547360,559240),
        Interval("moving",67280,107240),
        Interval("moving",957360,997320),
        Interval("moving",841240,844760),
        Interval("moving",829400,841280),
        Interval("moving",917360,957320),
        Interval("moving",387280,427240),
        Interval("moving",791840,797120),
        Interval("moving",608080,648040),
        Interval("moving",648080,688040),
        Interval("moving",768080,786200),
        Interval("moving",909440,910400),
        Interval("moving",347280,387240),
        Interval("moving",728080,768040),
        Interval("moving",427280,460600),
        Interval("moving",844720,868480),
        Interval("moving",997360,1037320),
        Interval("moving",786200,791880),
        Interval("moving",814840,829440),
        Interval("moving",797080,800440),
        Interval("moving",464120,547400),
        Interval("moving",187280,227240))
    val testing = List(movePos1,movePos12) ++ testingNeg1
    //new DataAsIntervals(trainingSet = List(training.head) ++ Random.shuffle(training.tail), testingSet = testing)
    new DataAsIntervals(trainingSet = training, testingSet = testing)
  }

  // Training set 2. All but movePos2
  //----------------------------------
  val moveTrainingSet2 = {
    //val training = allPosIntervals.filter(x => x!= movePos2) ++ allNegIntervals.filter(z => !testingNeg2.contains(z))
    val training =
      List(Interval("moving",2480,5760),
        Interval("moving",791840,797120),
        Interval("moving",648080,688040),
        Interval("moving",844720,868480),
        Interval("moving",547360,559240),
        Interval("moving",565760,568120),
        Interval("moving",841240,844760),
        Interval("moving",768080,786200),
        Interval("moving",917360,957320),
        Interval("moving",896880,909480),
        Interval("moving",568080,608040),
        Interval("moving",1077360,1102560),
        Interval("moving",909440,910400),
        Interval("moving",387280,427240),
        Interval("moving",608080,648040),
        Interval("moving",559200,565800),
        Interval("moving",1037360,1077320),
        Interval("moving",187280,227240),
        Interval("moving",460600,464160),
        Interval("moving",680,2520),
        Interval("moving",688080,728040),
        Interval("moving",307280,347240),
        Interval("moving",728080,768040),
        Interval("moving",347280,387240),
        Interval("moving",874280,896920),
        Interval("moving",227280,267240),
        Interval("moving",267280,307240),
        Interval("moving",868440,874320),
        Interval("moving",797080,800440),
        Interval("moving",464120,547400),
        Interval("moving",997360,1037320),
        Interval("moving",829400,841280),
        Interval("moving",27280,67240),
        Interval("moving",957360,997320),
        Interval("moving",814840,829440),
        Interval("moving",786200,791880),
        Interval("moving",427280,460600),
        Interval("moving",800400,814880),
        Interval("moving",5720,24440),
        Interval("moving",910360,917400))
    val testing = List(movePos2) ++ testingNeg2
    //new DataAsIntervals(trainingSet = List(training.head) ++ Random.shuffle(training.tail), testingSet = testing)
    new DataAsIntervals(trainingSet = training, testingSet = testing)
  }

  // Training set 3. All but movePos3
  //----------------------------------
  val moveTrainingSet3 = {
    //val training = allPosIntervals.filter(x => x!= movePos3) ++ allNegIntervals.filter(z => !testingNeg3.contains(z))
    val training =
      List(Interval("moving",2480,5760),
        Interval("moving",874280,896920),
        Interval("moving",559200,565800),
        Interval("moving",24400,27320),
        Interval("moving",464120,547400),
        Interval("moving",27280,67240),
        Interval("moving",5720,24440),
        Interval("moving",910360,917400),
        Interval("moving",896880,909480),
        Interval("moving",957360,997320),
        Interval("moving",565760,568120),
        Interval("moving",841240,844760),
        Interval("moving",547360,559240),
        Interval("moving",307280,347240),
        Interval("moving",728080,768040),
        Interval("moving",67280,107240),
        Interval("moving",1077360,1102560),
        Interval("moving",347280,387240),
        Interval("moving",868440,874320),
        Interval("moving",768080,786200),
        Interval("moving",791840,797120),
        Interval("moving",829400,841280),
        Interval("moving",1037360,1077320),
        Interval("moving",427280,460600),
        Interval("moving",688080,728040),
        Interval("moving",844720,868480),
        Interval("moving",797080,800440),
        Interval("moving",648080,688040),
        Interval("moving",147280,187240),
        Interval("moving",997360,1037320),
        Interval("moving",786200,791880),
        Interval("moving",387280,427240),
        Interval("moving",608080,648040),
        Interval("moving",909440,910400),
        Interval("moving",800400,814880),
        Interval("moving",680,2520),
        Interval("moving",107280,147240),
        Interval("moving",917360,957320),
        Interval("moving",568080,608040),
        Interval("moving",814840,829440))
    val testing = List(movePos3) ++ testingNeg3
    new DataAsIntervals(trainingSet = List(training.head) ++ Random.shuffle(training.tail), testingSet = testing)
    //new DataAsIntervals(trainingSet = training, testingSet = testing)
  }

  // Training set 4. All but movePos4
  //----------------------------------
  val moveTrainingSet4 = {
    //val training = allPosIntervals.filter(x => x!= movePos4) ++ allNegIntervals.filter(z => !testingNeg4.contains(z))
    val training =
      List(Interval("moving",2480,5760),
        Interval("moving",565760,568120),
        Interval("moving",868440,874320),
        Interval("moving",797080,800440),
        Interval("moving",728080,768040),
        Interval("moving",909440,910400),
        Interval("moving",800400,814880),
        Interval("moving",107280,147240),
        Interval("moving",768080,786200),
        Interval("moving",67280,107240),
        Interval("moving",24400,27320),
        Interval("moving",841240,844760),
        Interval("moving",608080,648040),
        Interval("moving",791840,797120),
        Interval("moving",910360,917400),
        Interval("moving",187280,227240),
        Interval("moving",688080,728040),
        Interval("moving",844720,868480),
        Interval("moving",874280,896920),
        Interval("moving",896880,909480),
        Interval("moving",464120,547400),
        Interval("moving",917360,957320),
        Interval("moving",559200,565800),
        Interval("moving",814840,829440),
        Interval("moving",786200,791880),
        Interval("moving",5720,24440),
        Interval("moving",997360,1037320),
        Interval("moving",680,2520),
        Interval("moving",27280,67240),
        Interval("moving",1077360,1102560),
        Interval("moving",568080,608040),
        Interval("moving",829400,841280),
        Interval("moving",957360,997320),
        Interval("moving",460600,464160),
        Interval("moving",267280,307240),
        Interval("moving",427280,460600),
        Interval("moving",648080,688040),
        Interval("moving",227280,267240),
        Interval("moving",147280,187240),
        Interval("moving",1037360,1077320))
    val testing = List(movePos4) ++ testingNeg4
    //new DataAsIntervals(trainingSet = List(training.head) ++ Random.shuffle(training.tail), testingSet = testing)
    new DataAsIntervals(trainingSet = training, testingSet = testing)
  }

  // Training set 5. All but movePos5
  //----------------------------------
  val moveTrainingSet5 = {
    //val training = allPosIntervals.filter(x => x!= movePos5) ++ allNegIntervals.filter(z => !testingNeg5.contains(z))
    val training =
      List(Interval("moving",2480,5760),
        Interval("moving",909440,910400),
        Interval("moving",107280,147240),
        Interval("moving",347280,387240),
        Interval("moving",844720,868480),
        Interval("moving",680,2520),
        Interval("moving",187280,227240),
        Interval("moving",814840,829440),
        Interval("moving",786200,791880),
        Interval("moving",797080,800440),
        Interval("moving",460600,464160),
        Interval("moving",791840,797120),
        Interval("moving",608080,648040),
        Interval("moving",957360,997320),
        Interval("moving",387280,427240),
        Interval("moving",1037360,1077320),
        Interval("moving",997360,1037320),
        Interval("moving",728080,768040),
        Interval("moving",896880,909480),
        Interval("moving",27280,67240),
        Interval("moving",841240,844760),
        Interval("moving",568080,608040),
        Interval("moving",910360,917400),
        Interval("moving",688080,728040),
        Interval("moving",5720,24440),
        Interval("moving",1077360,1102560),
        Interval("moving",147280,187240),
        Interval("moving",868440,874320),
        Interval("moving",874280,896920),
        Interval("moving",24400,27320),
        Interval("moving",829400,841280),
        Interval("moving",768080,786200),
        Interval("moving",648080,688040),
        Interval("moving",547360,559240),
        Interval("moving",67280,107240),
        Interval("moving",267280,307240),
        Interval("moving",917360,957320),
        Interval("moving",307280,347240),
        Interval("moving",227280,267240),
        Interval("moving",800400,814880))
    val testing = List(movePos5) ++ testingNeg5
    //new DataAsIntervals(trainingSet = List(training.head) ++ Random.shuffle(training.tail), testingSet = testing)
    new DataAsIntervals(trainingSet = training, testingSet = testing)
  }

  // Training set 6. All but movePos6
  //----------------------------------
  val moveTrainingSet6 = {
    //val training = allPosIntervals.filter(x => x!= movePos6) ++ allNegIntervals.filter(z => !testingNeg6.contains(z))
    val training =
      List(Interval("moving",2480,5760),
        Interval("moving",1037360,1077320),
        Interval("moving",814840,829440),
        Interval("moving",547360,559240),
        Interval("moving",147280,187240),
        Interval("moving",460600,464160),
        Interval("moving",307280,347240),
        Interval("moving",917360,957320),
        Interval("moving",797080,800440),
        Interval("moving",868440,874320),
        Interval("moving",841240,844760),
        Interval("moving",67280,107240),
        Interval("moving",910360,917400),
        Interval("moving",688080,728040),
        Interval("moving",957360,997320),
        Interval("moving",1077360,1102560),
        Interval("moving",107280,147240),
        Interval("moving",565760,568120),
        Interval("moving",267280,307240),
        Interval("moving",829400,841280),
        Interval("moving",24400,27320),
        Interval("moving",227280,267240),
        Interval("moving",768080,786200),
        Interval("moving",680,2520),
        Interval("moving",387280,427240),
        Interval("moving",728080,768040),
        Interval("moving",559200,565800),
        Interval("moving",844720,868480),
        Interval("moving",909440,910400),
        Interval("moving",427280,460600),
        Interval("moving",464120,547400),
        Interval("moving",347280,387240),
        Interval("moving",874280,896920),
        Interval("moving",5720,24440),
        Interval("moving",896880,909480),
        Interval("moving",800400,814880),
        Interval("moving",187280,227240),
        Interval("moving",27280,67240),
        Interval("moving",791840,797120),
        Interval("moving",997360,1037320))
    val testing = List(movePos6) ++ testingNeg6
    //new DataAsIntervals(trainingSet = List(training.head) ++ Random.shuffle(training.tail), testingSet = testing)
    new DataAsIntervals(trainingSet = training, testingSet = testing)
  }

  // Training set 7. All but movePos7
  //----------------------------------
  val moveTrainingSet7 = {
    //val training = allPosIntervals.filter(x => x!= movePos7) ++ allNegIntervals.filter(z => !testingNeg7.contains(z))
    val training =
      List(Interval("moving",2480,5760),
        Interval("moving",27280,67240),
        Interval("moving",307280,347240),
        Interval("moving",874280,896920),
        Interval("moving",800400,814880),
        Interval("moving",680,2520),
        Interval("moving",568080,608040),
        Interval("moving",997360,1037320),
        Interval("moving",559200,565800),
        Interval("moving",67280,107240),
        Interval("moving",387280,427240),
        Interval("moving",841240,844760),
        Interval("moving",147280,187240),
        Interval("moving",464120,547400),
        Interval("moving",547360,559240),
        Interval("moving",910360,917400),
        Interval("moving",5720,24440),
        Interval("moving",24400,27320),
        Interval("moving",829400,841280),
        Interval("moving",460600,464160),
        Interval("moving",1037360,1077320),
        Interval("moving",227280,267240),
        Interval("moving",909440,910400),
        Interval("moving",814840,829440),
        Interval("moving",107280,147240),
        Interval("moving",187280,227240),
        Interval("moving",347280,387240),
        Interval("moving",427280,460600),
        Interval("moving",648080,688040),
        Interval("moving",917360,957320),
        Interval("moving",957360,997320),
        Interval("moving",791840,797120),
        Interval("moving",267280,307240),
        Interval("moving",608080,648040),
        Interval("moving",786200,791880),
        Interval("moving",1077360,1102560),
        Interval("moving",896880,909480),
        Interval("moving",868440,874320),
        Interval("moving",844720,868480),
        Interval("moving",565760,568120))
    val testing = List(movePos7) ++ testingNeg7
    //new DataAsIntervals(trainingSet = List(training.head) ++ Random.shuffle(training.tail), testingSet = testing)
    new DataAsIntervals(trainingSet = training, testingSet = testing)
  }

  // Training set 8. All but movePos8
  //----------------------------------
  val moveTrainingSet8 = {
    //val training = allPosIntervals.filter(x => x!= movePos8) ++ allNegIntervals.filter(z => !testingNeg8.contains(z))
    val training =
      List(Interval("moving",2480,5760),
        Interval("moving",910360,917400),
        Interval("moving",917360,957320),
        Interval("moving",728080,768040),
        Interval("moving",107280,147240),
        Interval("moving",559200,565800),
        Interval("moving",187280,227240),
        Interval("moving",460600,464160),
        Interval("moving",427280,460600),
        Interval("moving",841240,844760),
        Interval("moving",791840,797120),
        Interval("moving",797080,800440),
        Interval("moving",67280,107240),
        Interval("moving",909440,910400),
        Interval("moving",147280,187240),
        Interval("moving",464120,547400),
        Interval("moving",568080,608040),
        Interval("moving",27280,67240),
        Interval("moving",874280,896920),
        Interval("moving",680,2520),
        Interval("moving",844720,868480),
        Interval("moving",387280,427240),
        Interval("moving",24400,27320),
        Interval("moving",868440,874320),
        Interval("moving",1037360,1077320),
        Interval("moving",608080,648040),
        Interval("moving",267280,307240),
        Interval("moving",688080,728040),
        Interval("moving",997360,1037320),
        Interval("moving",547360,559240),
        Interval("moving",565760,568120),
        Interval("moving",648080,688040),
        Interval("moving",307280,347240),
        Interval("moving",957360,997320),
        Interval("moving",1077360,1102560),
        Interval("moving",5720,24440),
        Interval("moving",227280,267240),
        Interval("moving",786200,791880),
        Interval("moving",896880,909480),
        Interval("moving",347280,387240))
    val testing = List(movePos8) ++ testingNeg8
    //new DataAsIntervals(trainingSet = List(training.head) ++ Random.shuffle(training.tail), testingSet = testing)
    new DataAsIntervals(trainingSet = training, testingSet = testing)
  }

  // Training set 9. All but movePos9
  //----------------------------------
  val moveTrainingSet9 = {
    //val training = allPosIntervals.filter(x => x!= movePos9) ++ allNegIntervals.filter(z => !testingNeg9.contains(z))
    val training = List(Interval("moving",2480,5760),Interval("moving",267280,307240),Interval("moving",307280,347240),Interval("moving",768080,786200),Interval("moving",547360,559240),Interval("moving",24400,27320),Interval("moving",910360,917400),Interval("moving",917360,957320),Interval("moving",728080,768040),Interval("moving",460600,464160),Interval("moving",786200,791880),Interval("moving",387280,427240),Interval("moving",347280,387240),Interval("moving",27280,67240),Interval("moving",1037360,1077320),Interval("moving",464120,547400),Interval("moving",680,2520),Interval("moving",997360,1037320),Interval("moving",559200,565800),Interval("moving",67280,107240),Interval("moving",227280,267240),Interval("moving",568080,608040),Interval("moving",844720,868480),Interval("moving",107280,147240),Interval("moving",896880,909480),Interval("moving",5720,24440),Interval("moving",427280,460600),Interval("moving",797080,800440),Interval("moving",648080,688040),Interval("moving",688080,728040),Interval("moving",868440,874320),Interval("moving",814840,829440),Interval("moving",957360,997320),Interval("moving",829400,841280),Interval("moving",608080,648040),Interval("moving",565760,568120),Interval("moving",800400,814880),Interval("moving",791840,797120),Interval("moving",147280,187240),Interval("moving",187280,227240))
    val testing =  testingNeg9 //List(movePos9) ++ testingNeg9
    //new DataAsIntervals(trainingSet = List(training.head) ++ Random.shuffle(training.tail), testingSet = testing)
    new DataAsIntervals(trainingSet = training, testingSet = testing)
  }

  // Training set 10. All but movePos10
  //----------------------------------
  val moveTrainingSet10 = {
    //val training = allPosIntervals.filter(x => x!= movePos10) ++ allNegIntervals.filter(z => !testingNeg10.contains(z))
    val training = List(Interval("moving",2480,5760),Interval("moving",829400,841280),Interval("moving",267280,307240),Interval("moving",387280,427240),Interval("moving",5720,24440),Interval("moving",568080,608040),Interval("moving",67280,107240),Interval("moving",107280,147240),Interval("moving",909440,910400),Interval("moving",896880,909480),Interval("moving",559200,565800),Interval("moving",841240,844760),Interval("moving",347280,387240),Interval("moving",874280,896920),Interval("moving",688080,728040),Interval("moving",814840,829440),Interval("moving",147280,187240),Interval("moving",565760,568120),Interval("moving",24400,27320),Interval("moving",427280,460600),Interval("moving",307280,347240),Interval("moving",768080,786200),Interval("moving",27280,67240),Interval("moving",1077360,1102560),Interval("moving",844720,868480),Interval("moving",800400,814880),Interval("moving",648080,688040),Interval("moving",464120,547400),Interval("moving",910360,917400),Interval("moving",680,2520),Interval("moving",797080,800440),Interval("moving",227280,267240),Interval("moving",460600,464160),Interval("moving",791840,797120),Interval("moving",728080,768040),Interval("moving",547360,559240),Interval("moving",1037360,1077320),Interval("moving",608080,648040),Interval("moving",786200,791880),Interval("moving",187280,227240))
    val testing =  List(movePos10) ++ testingNeg10
    //new DataAsIntervals(trainingSet = List(training.head) ++ Random.shuffle(training.tail), testingSet = testing)
    new DataAsIntervals(trainingSet = training, testingSet = testing)
  }

  val allTrainingSets = List(moveTrainingSet1,moveTrainingSet2,moveTrainingSet3,moveTrainingSet4,moveTrainingSet5,moveTrainingSet6,moveTrainingSet7,moveTrainingSet8,
    moveTrainingSet9,moveTrainingSet10)

  val wholeCAVIAR1 = {
    new DataAsIntervals(trainingSet = List(allPosIntervals.head) ++ Random.shuffle(allPosIntervals++allNegIntervals), testingSet = allPosIntervals++allNegIntervals)
  }

  val wholeCAVIAR = {
    new DataAsIntervals(trainingSet = List(), testingSet = allPosIntervals++allNegIntervals)
  }













}

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

package datautils.caviar_intervals

import utils.DataUtils.{DataAsIntervals, Interval}

import scala.util.Random

/**
  * Created by nkatz on 3/22/16.
  */
object MeetingTrainingData {


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
  meeting:
  Average positive length: 170.0
  Total negative length: 25103.0
  Total positive length: 1867.0
  90% of negatives (training set size) is 22592.7
  So negatives' testing set size is 2511.0
*/
  //val meetPos1 = Interval("meeting",5720,24480)
  val meetPos1 = Interval("meeting",5680,24480)
  //val meetPos2 = Interval("meeting",27280,61560)
  val meetPos2 = Interval("meeting",27240,61520)
  //val meetPos3 = Interval("meeting",507120,509800)
  val meetPos3 = Interval("meeting",507080,509760)
  //val meetPos4 = Interval("meeting",559200,564200)
  val meetPos4 = Interval("meeting",559160,564160)
  //val meetPos5 = Interval("meeting",785240,786320)
  val meetPos5 = Interval("meeting",785200,786280)
  //val meetPos6 = Interval("meeting",813080,814960)
  val meetPos6 = Interval("meeting",813040,814920)
  //val meetPos7 = Interval("meeting",829400,835520)
  val meetPos7 = Interval("meeting",829360,833520)
  //val meetPos8 = Interval("meeting",842680,843640)
  val meetPos8 = Interval("meeting",840200,841320)
  //val meetPos9 = Interval("meeting",892440,894920)
  val meetPos9 = Interval("meeting",867560,868520)
  //val meetPos10 = Interval("meeting",1009000,1009880)
  val meetPos10 = Interval("meeting",917320,919800)
  val meetPos11 = Interval("meeting",1033880,1034760)

  // To break large intervals in smaller of 1000 data points use this (40 is the step):
  // List.range(919760,1030680,40).grouped(500).map(x => (x.head,x.tail.reverse.head)) foreach println
  val meetNeg1 = Interval("meeting",680,5720)       // length: 128
  val meetNeg2 = Interval("meeting",24440,27280)    // length: 73
  val meetNeg3 = Interval("meeting",61480,101440)   // length: 1000
  val meetNeg4 = Interval("meeting",101480,141440)  // length: 1000
  val meetNeg5 = Interval("meeting",141480,181440)  // length: 1000
  val meetNeg6 = Interval("meeting",181480,221440)  // length: 1000
  val meetNeg7 = Interval("meeting",221480,261440)  // length: 1000
  val meetNeg8 = Interval("meeting",261480,301440)  // length: 1000
  val meetNeg9 = Interval("meeting",301480,341440)  // length: 1000
  val meetNeg10 = Interval("meeting",341480,381440) // length: 1000
  val meetNeg11 = Interval("meeting",381480,421440) // length: 1000
  val meetNeg12 = Interval("meeting",421480,461440) // length: 1000
  val meetNeg13 = Interval("meeting",461480,501440) // length: 1000
  val meetNeg14 = Interval("meeting",509720,559200) // length: 1238
  val meetNeg15 = Interval("meeting",564120,604080) // length: 1000
  val meetNeg16 = Interval("meeting",604120,644080) // length: 1000
  val meetNeg17 = Interval("meeting",644120,684080) // length: 1000
  val meetNeg18 = Interval("meeting",684120,724080) // length: 1000
  val meetNeg19 = Interval("meeting",724120,764080) // length: 1000
  val meetNeg20 = Interval("meeting",764120,785200) // length: 528
  val meetNeg21 = Interval("meeting",786240,813080) // length: 672
  val meetNeg22 = Interval("meeting",814880,829400) // length: 364
  val meetNeg23 = Interval("meeting",833480,840240) // length: 182
  val meetNeg24 = Interval("meeting",841280,867600) // length: 659
  val meetNeg25 = Interval("meeting",868480,917360)// length: 1223

  val meetNeg26 = Interval("meeting",919760,939720)// length: 500
  val meetNeg27 = Interval("meeting",939760,959720)// length: 500
  val meetNeg28 = Interval("meeting",959760,979720)// length: 500
  val meetNeg29 = Interval("meeting",999760,1019720)// length: 500
  val meetNeg30 = Interval("meeting",1019760,1030640)// length: 500
  val meetNeg31 = Interval("meeting",1030720,1033920)// length: 81
  val meetNeg32 = Interval("meeting",1009840,1077680)//// length: 1697
  val meetNeg33 = Interval("meeting",1034720,1102600) // length: 1000


  val allNegIntervals = List(meetNeg1,meetNeg2,meetNeg3,meetNeg4,meetNeg5,meetNeg6,meetNeg7,meetNeg8,meetNeg9,meetNeg10,meetNeg11,meetNeg12,meetNeg13,meetNeg14,
    meetNeg15,meetNeg16,meetNeg17,meetNeg18,meetNeg19,meetNeg20,meetNeg21,meetNeg22,meetNeg23,meetNeg24,meetNeg25,meetNeg26,meetNeg27,
    meetNeg28,meetNeg29,meetNeg30,meetNeg31,meetNeg32,meetNeg33)

  val allPosIntervals = List(meetPos1,meetPos2,meetPos3,meetPos4,meetPos5,meetPos6,meetPos7,meetPos8,meetPos9,meetPos10,meetPos11)

  // Negative intervals for the testing sets
  val testingNeg1 = List(meetNeg1,meetNeg2,meetNeg3,meetNeg33)
  val testingNeg2 = List(meetNeg4,meetNeg5,meetNeg6)
  val testingNeg3 = List(meetNeg7,meetNeg8,meetNeg9)
  val testingNeg4 = List(meetNeg10,meetNeg11,meetNeg12)
  val testingNeg5 = List(meetNeg13,meetNeg14,meetNeg15)
  val testingNeg6 = List(meetNeg16,meetNeg17,meetNeg18)
  val testingNeg7 = List(meetNeg19,meetNeg20,meetNeg21)
  val testingNeg8 = List(meetNeg22,meetNeg23,meetNeg24)
  val testingNeg9 = List(meetNeg25,meetNeg26,meetNeg27)
  val testingNeg10 = List(meetNeg28,meetNeg29,meetNeg30,meetNeg31,meetNeg32)

  val allNegativeTestingSetIntervals = List(testingNeg1,testingNeg2,testingNeg3,testingNeg4,testingNeg5,testingNeg6,testingNeg7,testingNeg8,testingNeg9,testingNeg10)


  def getMeetingTrainingData(fold: Int, randomOrder: Boolean) = {
    val training = fold match {
      case 1 =>
        randomOrder match {
          // Training set 1. All but meetPos1 & meetPos11
          case true => allPosIntervals.filter(x => x!= meetPos1 && x!= meetPos11) ++ allNegIntervals.filter(z => !testingNeg1.contains(z))
          case _ => List(Interval("meeting",27240,61520),Interval("meeting",301480,341440),Interval("meeting",939760,959720),Interval("meeting",813040,814920),Interval("meeting",684120,724080),Interval("meeting",644120,684080),Interval("meeting",1009840,1077680),Interval("meeting",833480,840240),Interval("meeting",867560,868520),Interval("meeting",559160,564160),Interval("meeting",786240,813080),Interval("meeting",917320,919800),Interval("meeting",840200,841320),Interval("meeting",785200,786280),Interval("meeting",919760,939720),Interval("meeting",181480,221440),Interval("meeting",829360,833520),Interval("meeting",564120,604080),Interval("meeting",421480,461440),Interval("meeting",141480,181440),Interval("meeting",341480,381440),Interval("meeting",959760,979720),Interval("meeting",999760,1019720),Interval("meeting",509720,559200),Interval("meeting",1019760,1030640),Interval("meeting",261480,301440),Interval("meeting",381480,421440),Interval("meeting",724120,764080),Interval("meeting",461480,501440),Interval("meeting",814880,829400),Interval("meeting",221480,261440),Interval("meeting",604120,644080),Interval("meeting",841280,867600),Interval("meeting",507080,509760),Interval("meeting",868480,917360),Interval("meeting",101480,141440),Interval("meeting",1030720,1033920),Interval("meeting",764120,785200))
        }
      case 2 =>
        randomOrder match {
          // Training set 2. All but meetPos2
          case true => allPosIntervals.filter(x => x!= meetPos2) ++ allNegIntervals.filter(z => !testingNeg2.contains(z))
          case _ => List(Interval("meeting",5680,24480),Interval("meeting",680,5720),Interval("meeting",1033880,1034760),Interval("meeting",814880,829400),Interval("meeting",917320,919800),Interval("meeting",381480,421440),Interval("meeting",999760,1019720),Interval("meeting",840200,841320),Interval("meeting",261480,301440),Interval("meeting",461480,501440),Interval("meeting",507080,509760),Interval("meeting",868480,917360),Interval("meeting",684120,724080),Interval("meeting",221480,261440),Interval("meeting",301480,341440),Interval("meeting",959760,979720),Interval("meeting",813040,814920),Interval("meeting",1034720,1102600),Interval("meeting",341480,381440),Interval("meeting",604120,644080),Interval("meeting",867560,868520),Interval("meeting",509720,559200),Interval("meeting",1009840,1077680),Interval("meeting",421480,461440),Interval("meeting",833480,840240),Interval("meeting",841280,867600),Interval("meeting",785200,786280),Interval("meeting",786240,813080),Interval("meeting",564120,604080),Interval("meeting",1019760,1030640),Interval("meeting",61480,101440),Interval("meeting",24440,27280),Interval("meeting",919760,939720),Interval("meeting",829360,833520),Interval("meeting",559160,564160),Interval("meeting",764120,785200),Interval("meeting",1030720,1033920),Interval("meeting",644120,684080),Interval("meeting",724120,764080),Interval("meeting",939760,959720))
        }
      case 3 =>
        randomOrder match {
          // Training set 3. All but meetPos3
          case true => allPosIntervals.filter(x => x!= meetPos3) ++ allNegIntervals.filter(z => !testingNeg3.contains(z))
          case _ => List(Interval("meeting",5680,24480),Interval("meeting",1009840,1077680),Interval("meeting",684120,724080),Interval("meeting",724120,764080),Interval("meeting",680,5720),Interval("meeting",868480,917360),Interval("meeting",841280,867600),Interval("meeting",27240,61520),Interval("meeting",461480,501440),Interval("meeting",785200,786280),Interval("meeting",559160,564160),Interval("meeting",181480,221440),Interval("meeting",1019760,1030640),Interval("meeting",509720,559200),Interval("meeting",341480,381440),Interval("meeting",867560,868520),Interval("meeting",1033880,1034760),Interval("meeting",833480,840240),Interval("meeting",829360,833520),Interval("meeting",999760,1019720),Interval("meeting",564120,604080),Interval("meeting",939760,959720),Interval("meeting",959760,979720),Interval("meeting",1030720,1033920),Interval("meeting",644120,684080),Interval("meeting",61480,101440),Interval("meeting",840200,841320),Interval("meeting",919760,939720),Interval("meeting",813040,814920),Interval("meeting",917320,919800),Interval("meeting",764120,785200),Interval("meeting",24440,27280),Interval("meeting",101480,141440),Interval("meeting",421480,461440),Interval("meeting",814880,829400),Interval("meeting",786240,813080),Interval("meeting",1034720,1102600),Interval("meeting",604120,644080),Interval("meeting",141480,181440),Interval("meeting",381480,421440))
        }
      case 4 =>
        randomOrder match {
          // Training set 4. All but meetPos4
          case true => allPosIntervals.filter(x => x!= meetPos4) ++ allNegIntervals.filter(z => !testingNeg4.contains(z))
          case _ => List(Interval("meeting",5680,24480),Interval("meeting",840200,841320),Interval("meeting",101480,141440),Interval("meeting",829360,833520),Interval("meeting",1019760,1030640),Interval("meeting",461480,501440),Interval("meeting",868480,917360),Interval("meeting",680,5720),Interval("meeting",61480,101440),Interval("meeting",261480,301440),Interval("meeting",724120,764080),Interval("meeting",604120,644080),Interval("meeting",785200,786280),Interval("meeting",999760,1019720),Interval("meeting",301480,341440),Interval("meeting",841280,867600),Interval("meeting",181480,221440),Interval("meeting",141480,181440),Interval("meeting",939760,959720),Interval("meeting",24440,27280),Interval("meeting",644120,684080),Interval("meeting",221480,261440),Interval("meeting",786240,813080),Interval("meeting",919760,939720),Interval("meeting",813040,814920),Interval("meeting",959760,979720),Interval("meeting",684120,724080),Interval("meeting",27240,61520),Interval("meeting",1034720,1102600),Interval("meeting",509720,559200),Interval("meeting",1030720,1033920),Interval("meeting",564120,604080),Interval("meeting",833480,840240),Interval("meeting",867560,868520),Interval("meeting",814880,829400),Interval("meeting",1033880,1034760),Interval("meeting",507080,509760),Interval("meeting",764120,785200),Interval("meeting",1009840,1077680),Interval("meeting",917320,919800))

        }
      case 5 =>
        randomOrder match {
          // Training set 5. All but meetPos5
          case true => allPosIntervals.filter(x => x!= meetPos5) ++ allNegIntervals.filter(z => !testingNeg5.contains(z))
          case _ => List(Interval("meeting",5680,24480),Interval("meeting",1019760,1030640),Interval("meeting",829360,833520),Interval("meeting",421480,461440),Interval("meeting",999760,1019720),Interval("meeting",680,5720),Interval("meeting",814880,829400),Interval("meeting",1033880,1034760),Interval("meeting",559160,564160),Interval("meeting",1009840,1077680),Interval("meeting",917320,919800),Interval("meeting",840200,841320),Interval("meeting",61480,101440),Interval("meeting",1034720,1102600),Interval("meeting",684120,724080),Interval("meeting",764120,785200),Interval("meeting",101480,141440),Interval("meeting",833480,840240),Interval("meeting",381480,421440),Interval("meeting",221480,261440),Interval("meeting",841280,867600),Interval("meeting",724120,764080),Interval("meeting",507080,509760),Interval("meeting",919760,939720),Interval("meeting",1030720,1033920),Interval("meeting",261480,301440),Interval("meeting",813040,814920),Interval("meeting",301480,341440),Interval("meeting",341480,381440),Interval("meeting",939760,959720),Interval("meeting",604120,644080),Interval("meeting",867560,868520),Interval("meeting",959760,979720),Interval("meeting",27240,61520),Interval("meeting",181480,221440),Interval("meeting",141480,181440),Interval("meeting",868480,917360),Interval("meeting",24440,27280),Interval("meeting",786240,813080),Interval("meeting",644120,684080))
        }
      case 6 =>
        randomOrder match {
          // Training set 6. All but meetPos6
          case true => allPosIntervals.filter(x => x!= meetPos6) ++ allNegIntervals.filter(z => !testingNeg6.contains(z))
          case _ => List(Interval("meeting",5680,24480),Interval("meeting",786240,813080),Interval("meeting",507080,509760),Interval("meeting",840200,841320),Interval("meeting",833480,840240),Interval("meeting",829360,833520),Interval("meeting",785200,786280),Interval("meeting",917320,919800),Interval("meeting",261480,301440),Interval("meeting",341480,381440),Interval("meeting",27240,61520),Interval("meeting",939760,959720),Interval("meeting",724120,764080),Interval("meeting",461480,501440),Interval("meeting",1033880,1034760),Interval("meeting",301480,341440),Interval("meeting",221480,261440),Interval("meeting",764120,785200),Interval("meeting",61480,101440),Interval("meeting",1034720,1102600),Interval("meeting",999760,1019720),Interval("meeting",381480,421440),Interval("meeting",841280,867600),Interval("meeting",509720,559200),Interval("meeting",141480,181440),Interval("meeting",564120,604080),Interval("meeting",680,5720),Interval("meeting",101480,141440),Interval("meeting",181480,221440),Interval("meeting",1019760,1030640),Interval("meeting",959760,979720),Interval("meeting",868480,917360),Interval("meeting",867560,868520),Interval("meeting",814880,829400),Interval("meeting",1030720,1033920),Interval("meeting",421480,461440),Interval("meeting",559160,564160),Interval("meeting",24440,27280),Interval("meeting",1009840,1077680),Interval("meeting",919760,939720))
        }
      case 7 =>
        randomOrder match {
          // Training set 7. All but meetPos7
          case true => allPosIntervals.filter(x => x!= meetPos7) ++ allNegIntervals.filter(z => !testingNeg7.contains(z))
          case _ => List(Interval("meeting",5680,24480),Interval("meeting",27240,61520),Interval("meeting",221480,261440),Interval("meeting",814880,829400),Interval("meeting",301480,341440),Interval("meeting",684120,724080),Interval("meeting",840200,841320),Interval("meeting",509720,559200),Interval("meeting",1033880,1034760),Interval("meeting",1034720,1102600),Interval("meeting",61480,101440),Interval("meeting",841280,867600),Interval("meeting",461480,501440),Interval("meeting",999760,1019720),Interval("meeting",507080,509760),Interval("meeting",381480,421440),Interval("meeting",559160,564160),Interval("meeting",785200,786280),Interval("meeting",181480,221440),Interval("meeting",141480,181440),Interval("meeting",261480,301440),Interval("meeting",1009840,1077680),Interval("meeting",813040,814920),Interval("meeting",341480,381440),Interval("meeting",644120,684080),Interval("meeting",101480,141440),Interval("meeting",919760,939720),Interval("meeting",421480,461440),Interval("meeting",1019760,1030640),Interval("meeting",917320,919800),Interval("meeting",24440,27280),Interval("meeting",564120,604080),Interval("meeting",867560,868520),Interval("meeting",604120,644080),Interval("meeting",959760,979720),Interval("meeting",833480,840240),Interval("meeting",1030720,1033920),Interval("meeting",680,5720),Interval("meeting",868480,917360),Interval("meeting",939760,959720))
        }
      case 8 =>
        randomOrder match {
          // Training set 8. All but meetPos8
          case true => allPosIntervals.filter(x => x!= meetPos8) ++ allNegIntervals.filter(z => !testingNeg8.contains(z))
          case _ => allPosIntervals.filter(x => x!= meetPos8) ++ allNegIntervals.filter(z => !testingNeg8.contains(z))
        }
      case 9 =>
        randomOrder match {
          // Training set 9. All but meetPos9
          case true => allPosIntervals.filter(x => x!= meetPos9) ++ allNegIntervals.filter(z => !testingNeg9.contains(z))
          case _ => List(Interval("meeting",5680,24480),Interval("meeting",341480,381440),Interval("meeting",813040,814920),Interval("meeting",786240,813080),Interval("meeting",917320,919800),Interval("meeting",1009840,1077680),Interval("meeting",724120,764080),Interval("meeting",101480,141440),Interval("meeting",840200,841320),Interval("meeting",1030720,1033920),Interval("meeting",604120,644080),Interval("meeting",814880,829400),Interval("meeting",461480,501440),Interval("meeting",27240,61520),Interval("meeting",559160,564160),Interval("meeting",1019760,1030640),Interval("meeting",24440,27280),Interval("meeting",1033880,1034760),Interval("meeting",381480,421440),Interval("meeting",141480,181440),Interval("meeting",841280,867600),Interval("meeting",61480,101440),Interval("meeting",680,5720),Interval("meeting",509720,559200),Interval("meeting",221480,261440),Interval("meeting",959760,979720),Interval("meeting",829360,833520),Interval("meeting",507080,509760),Interval("meeting",564120,604080),Interval("meeting",301480,341440),Interval("meeting",833480,840240),Interval("meeting",764120,785200),Interval("meeting",181480,221440),Interval("meeting",684120,724080),Interval("meeting",785200,786280),Interval("meeting",644120,684080),Interval("meeting",421480,461440),Interval("meeting",261480,301440),Interval("meeting",1034720,1102600),Interval("meeting",999760,1019720))
        }
      case 10 =>
        randomOrder match {
          // Training set 10. All but meetPos10
          case true => allPosIntervals.filter(x => x!= meetPos10) ++ allNegIntervals.filter(z => !testingNeg10.contains(z))
          case _ => List(Interval("meeting",5680,24480),Interval("meeting",684120,724080),Interval("meeting",764120,785200),Interval("meeting",813040,814920),Interval("meeting",919760,939720),Interval("meeting",644120,684080),Interval("meeting",181480,221440),Interval("meeting",1034720,1102600),Interval("meeting",1033880,1034760),Interval("meeting",814880,829400),Interval("meeting",559160,564160),Interval("meeting",867560,868520),Interval("meeting",841280,867600),Interval("meeting",604120,644080),Interval("meeting",680,5720),Interval("meeting",868480,917360),Interval("meeting",786240,813080),Interval("meeting",221480,261440),Interval("meeting",509720,559200),Interval("meeting",564120,604080),Interval("meeting",785200,786280),Interval("meeting",261480,301440),Interval("meeting",939760,959720),Interval("meeting",724120,764080),Interval("meeting",61480,101440),Interval("meeting",840200,841320),Interval("meeting",341480,381440),Interval("meeting",461480,501440),Interval("meeting",829360,833520),Interval("meeting",381480,421440),Interval("meeting",507080,509760),Interval("meeting",141480,181440),Interval("meeting",24440,27280),Interval("meeting",27240,61520),Interval("meeting",421480,461440),Interval("meeting",833480,840240),Interval("meeting",101480,141440),Interval("meeting",301480,341440))
        }
      case _ => throw new RuntimeException("No such training set exists (use 1..10).")
    }

    val testing = fold match {
      case 1 => List(meetPos1) ++ List(meetPos11) ++ testingNeg1
      case 2 => List(meetPos2) ++ testingNeg2
      case 3 => List(meetPos3) ++ testingNeg3
      case 4 => List(meetPos4) ++ testingNeg4
      case 5 => List(meetPos5) ++ testingNeg5
      case 6 => List(meetPos6) ++ testingNeg6
      case 7 => List(meetPos7) ++ testingNeg7
      case 8 => List(meetPos8) ++ testingNeg8
      case 9 => List(meetPos9) ++ testingNeg9
      case 10 => List(meetPos10) ++ testingNeg10
    }

    if (randomOrder) new DataAsIntervals(trainingSet = List(training.head) ++ Random.shuffle(training.tail), testingSet = testing)
    else new DataAsIntervals(trainingSet = training, testingSet = testing)
  }


  val wholeCAVIARForManualRules = {
    new DataAsIntervals(trainingSet = List(), testingSet = allPosIntervals++allNegIntervals)
  }

  val wholeCAVIAR1 = {
    //new TrainingSet(trainingSet = List(allPosIntervals.head) ++ Random.shuffle(allPosIntervals++allNegIntervals), testingSet = allPosIntervals++allNegIntervals)

    // that's the one used normally
    new DataAsIntervals(trainingSet = List(allPosIntervals.head) ++ Random.shuffle(allPosIntervals++allNegIntervals), testingSet = List(Interval("meeting",680,1077680)))
    Interval("meeting",24440,27320)

    // that's for giving intervals explicitly to re-produce results. For instance, the one ordered as below should give a theory no example coverage at all
    /*
    new DataAsIntervals(trainingSet = List(meetPos1, meetNeg18, meetNeg14, meetNeg28, meetNeg17, meetPos7, meetNeg32,
      meetNeg33, meetNeg20, meetNeg4, meetNeg21, meetNeg25, meetNeg23, meetNeg9, meetNeg19, meetNeg22, meetNeg7,
      meetNeg15, meetPos9, meetNeg31, meetPos5, meetNeg2, meetPos8, meetNeg5, meetPos1, meetNeg3, meetPos4,
      meetNeg11, meetNeg12, meetNeg29, meetNeg16, meetNeg6, meetNeg27, meetPos6, meetPos2, meetNeg1, meetNeg13,
      meetNeg8, meetPos3, meetNeg30, meetNeg10, meetNeg24, meetPos10, meetNeg26
    ), testingSet = List(Interval("meeting",680,1077680)))
    */

  }



}

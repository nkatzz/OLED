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

package experiments.datautils.caviar_intervals

import experiments.datautils.caviar_intervals.MeetingTrainingData._
import utils.DataUtils.{DataAsIntervals, Interval}

import scala.util.Random

/**
  * Created by nkatz on 4/11/17.
  */

object MeetingTrainingDistributed {

  def splitN[A](xs: List[A], n: Int) = {
    val (quot, rem) = (xs.size / n, xs.size % n)
    val (smaller, bigger) = xs.splitAt(xs.size - rem * (quot + 1))
    smaller.grouped(quot) ++ bigger.grouped(quot + 1)
  }

  /* Split large positive intervals into smaller ones to evenly distribute the data */
  def splitInterval(x: Interval, byN: Int) = {
    splitN((x.startPoint to x.endPoint by 40).toList, byN).toList.map(z => Interval(x.HLE, z.head, z.tail.reverse.head))
  }

  def getData(testingPos: Interval, testingNegs: List[Interval], coresNum: Int) = {
    /*
    * meetPos1 (size=470) and meetPos1 (size=858) are large positive intervals.
    * Therefore, if they are in the training set, we break them into smaller
    * intervals in order to evenly distribute the data across nodes.
    * */

    ///*
    def isInTrainingSet(x: Interval) = x != testingPos



    /*

    var allowedPos = allPosIntervals.filter(x => x!= testingPos)

    if (isInTrainingSet(meetPos1)) {
      allowedPos = allowedPos.filter(x => x!= meetPos1)
      // split meetPos1
      val subIntervals = splitInterval(meetPos1, 4)
      allowedPos = subIntervals ++ allowedPos
    }

    if (isInTrainingSet(meetPos2)) {
      allowedPos = allowedPos.filter(x => x!= meetPos2)
      // split meetPos1
      val subIntervals = splitInterval(meetPos2, 8)
      allowedPos = subIntervals ++ allowedPos
    }
    */
    //*/

    val allowedPos = allPosIntervals.filter(x => x!= testingPos)
    val allowedNegs = allNegIntervals.filter(z => !testingNegs.contains(z))

    val positives = splitN(allowedPos, coresNum).toList
    val negatives = splitN(allowedNegs, coresNum).toList

    if (positives.length != negatives.length) throw new RuntimeException("SOMETHING'S WRONG!")

    val zipped = positives zip negatives
    val testing = List(testingPos) ++ testingNegs
    val out = zipped.map(x => new DataAsIntervals(trainingSet = List(x._1.head) ++ Random.shuffle(x._1.tail ++ x._2), testingSet = testing) )
    out
  }

  def main(args: Array[String]) = {
    /*
    * Just for testing-debugging
    *
    * */
    ///*
    val eight = EightFoldSplit.meetTrainingSet1
    val four = FourFoldSplit.meetTrainingSet1
    val two = TwoFoldSplit.meetTrainingSet1

    println(eight)
    println("")
    println(four)
    println("")
    println(two)
    //*/
    //println(splitInterval(meetPos1, 4))
    //println(Interval("meeting",5720,24480) == meetPos1)
  }





  object TwoFoldSplit {

    val cores = 2

    val meetTrainingSet1 = getData(meetPos1, testingNeg1, cores)
    val meetTrainingSet2 = getData(meetPos2, testingNeg2, cores)
    val meetTrainingSet3 = getData(meetPos3, testingNeg3, cores)
    val meetTrainingSet4 = getData(meetPos4, testingNeg4, cores)
    val meetTrainingSet5 = getData(meetPos5, testingNeg5, cores)
    val meetTrainingSet6 = getData(meetPos6, testingNeg6, cores)
    val meetTrainingSet7 = getData(meetPos7, testingNeg7, cores)
    val meetTrainingSet8 = getData(meetPos8, testingNeg8, cores)
    val meetTrainingSet9 = getData(meetPos9, testingNeg9, cores)
    val meetTrainingSet10 = getData(meetPos10, testingNeg10, cores)

  }



  object FourFoldSplit {

    val cores = 4

    val meetTrainingSet1 = getData(meetPos1, testingNeg1, cores)
    val meetTrainingSet2 = getData(meetPos2, testingNeg2, cores)
    val meetTrainingSet3 = getData(meetPos3, testingNeg3, cores)
    val meetTrainingSet4 = getData(meetPos4, testingNeg4, cores)
    val meetTrainingSet5 = getData(meetPos5, testingNeg5, cores)
    val meetTrainingSet6 = getData(meetPos6, testingNeg6, cores)
    val meetTrainingSet7 = getData(meetPos7, testingNeg7, cores)
    val meetTrainingSet8 = getData(meetPos8, testingNeg8, cores)
    val meetTrainingSet9 = getData(meetPos9, testingNeg9, cores)
    val meetTrainingSet10 = getData(meetPos10, testingNeg10, cores)
  }

  object EightFoldSplit {

    val cores = 8

    val meetTrainingSet1 = getData(meetPos1, testingNeg1, cores)
    val meetTrainingSet2 = getData(meetPos2, testingNeg2, cores)
    val meetTrainingSet3 = getData(meetPos3, testingNeg3, cores)
    val meetTrainingSet4 = getData(meetPos4, testingNeg4, cores)
    val meetTrainingSet5 = getData(meetPos5, testingNeg5, cores)
    val meetTrainingSet6 = getData(meetPos6, testingNeg6, cores)
    val meetTrainingSet7 = getData(meetPos7, testingNeg7, cores)
    val meetTrainingSet8 = getData(meetPos8, testingNeg8, cores)
    val meetTrainingSet9 = getData(meetPos9, testingNeg9, cores)
    val meetTrainingSet10 = getData(meetPos10, testingNeg10, cores)
  }



}

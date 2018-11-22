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

package experiments.datautils.caviar_data

import com.mongodb.casbah.commons.MongoDBObject
import logic.Examples._
import utils.DataUtils.Interval
import utils.Database

/**
  * Created by nkatz on 3/22/17.
  */

object FindPositiveNegativeIntervals {

  def main(args: Array[String]) = {
    val intervals = getPositiveNegativeIntervals("caviar", "meeting")
    println(intervals)
  }
  /**
    *
    * @param HLE the HLE we're currently learning
    * @return a tuple containing the list of positive and negative intervals for the HLE respectively
    */

  def getPositiveNegativeIntervals(DBName: String, HLE: String):  (List[Interval], List[Interval]) = {
    import scala.collection.mutable.Stack
    val step = 40
    var lastTime = 0
    val intervals = Stack[Interval]()
    val negativeIntervals = Stack[Interval]()
    var holdsPreviousely = false
    val closeInterval = (i: Stack[Interval], e: Example, what: String) => {
      val lastInterval = i.pop()
      // give some "air" around the positive intervals (hence +step)
      lastInterval.endPoint = if (what == "positive") e.time.toInt + step else e.time.toInt // This is wrong we should have "e.time.toInt-step" here
      i.push(lastInterval)
    }
    val startNewInterval = (i: Stack[Interval], e: Example, what: String) => {
      // give some "air" around the positive intervals (hence -step)
      if (what == "positive") i.push( Interval(HLE, e.time.toInt - step) ) else i.push( Interval("nothing", e.time.toInt) )
    }
    val closeLastInterval = (i: Stack[Interval]) => {
      val last = i.pop()
      if (last.endPoint == 0) last.endPoint = lastTime
      i.push(last)
    }

    val DB = new Database(DBName, "examples")
    val dataIterator = DB.collection.find().sort(MongoDBObject("time" -> 1))
    while(dataIterator.hasNext) {
      val x = dataIterator.next()
      val e = Example(x)
      val annotation = e.annotation

      if(annotation.exists(x => x.contains(HLE))) {
        if (! holdsPreviousely) {
          // close the last negative interval
          closeInterval(negativeIntervals, e, "negative")
          // start a new positive interval
          startNewInterval(intervals, e, "positive")
          holdsPreviousely = true
        }
      } else {
        if (holdsPreviousely) {
          // close the last positive interval
          closeInterval(intervals, e, "positive")
          // and start a new negative interval
          startNewInterval(negativeIntervals, e, "negative")
          holdsPreviousely = false
        } else {
          // start a new negative interval if on is not already being populated
          if (negativeIntervals.isEmpty) {
            startNewInterval(negativeIntervals, e, "negative")
          }
        }
      }
      lastTime = e.time.toInt // remember the last time to close the last interval in the stacks
    }
    closeLastInterval(negativeIntervals)
    closeLastInterval(intervals)
    intervals.toList.reverse foreach (x => println(s"${x.HLE}: (${x.startPoint},${x.endPoint}), length: ${x.length}")  )
    negativeIntervals.toList.reverse foreach (x => println(s"${x.HLE}: (${x.startPoint},${x.endPoint}), length: ${x.length}")  )
    // The average length of positive intervals
    val averagePositiveLenth = intervals.foldLeft(0.0)(_ + _.length) / intervals.length
    println(s"Average positive length: ${averagePositiveLenth.ceil}")
    // The total length of negative intervals
    val totalNegativeLength = negativeIntervals.foldLeft(0.0)(_ + _.length)
    println(s"Total negative length: $totalNegativeLength")
    // The total length of positive intervals
    val totalPositiveLength = intervals.foldLeft(0.0)(_ + _.length)
    println(s"Total positive length: $totalPositiveLength")
    // 90% of negatives will be used for training:
    val trainingNegativesNumber = ((90.0/100) * totalNegativeLength).toInt
    println(s"90% of negatives (training set size) is ${(90.0/100) * totalNegativeLength}")
    println(s"So negatives' testing set size is ${totalNegativeLength - trainingNegativesNumber}")
    (intervals.toList.reverse, negativeIntervals.toList.reverse)
  }


}

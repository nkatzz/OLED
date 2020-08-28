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

package utils

import java.io.File

import logic.{Clause, Constant, PriorTheory, Theory}
import utils.DataUtils.{DataAsIntervals, DataSet, Interval}

/**
  * Created by nkatz on 4/2/2016.
  */

/*

 USE WITH CAUTION. IMPORT THE OBJECT WHERE YOU NEED IT
 AND PAY ATTENTION TO THE ORDERING OF THE CONVERSIONS
 TO AVOID CONFLICTS THAT MAY COMPILE BUT RESULT IN RUNTIME ERRORS.
 ACTUALLY, THE BEST WOULD BE TO HAVE A SEPARATE OBJECT WRAPPING
 IMPLICITS FOR EACH PART OF THE CODE THAT NEEDS ONE. WE'LL SEE ABOUT
 THAT.

 */

object Implicits {
  //implicit def asPriorTheory(x: List[Clause]): PriorTheory = new PriorTheory(retainedRules = Theory(x))
  //implicit def asTheory(t: PriorTheory): Theory = t.merge
  implicit def asTheory(x: List[Clause]): Theory = Theory(x)
  implicit def asPriorTheory(t: Theory): PriorTheory = new PriorTheory(retainedRules = t)
  implicit def asTheory(x: Clause): Theory = Theory(List(x))

  implicit def asConstant(x: String): Constant = Constant(x)
  implicit def asConstant(x: Int): Constant = Constant(x.toString)

  implicit def getFilePath(x: java.io.File): String = x.getCanonicalPath
  implicit def getFileFromPath(x: String): java.io.File = new File(x)

  /*
  implicit def asExample(e: Example): Exmpl = new Exmpl(_id = e.time, exampleWithInertia = e)
  implicit def toExample(e: Iterator[Example]): Iterator[Exmpl] = e map asExample _
  */
}

/**
  * To be use from within the Clause class
  */
object ClauseImplicits {
  implicit def asConstant(x: String): Constant = Constant(x)
  implicit def asConstant(x: Int): Constant = Constant(x.toString)
}

object TrainingSetImplicits {

  // Until I fix this, I'll use this implicit conversion to convert a TrainingSet
  // (used by OLED's routines) to a DataSet (used by ILED's routines -- see the code at RunILED.scala)
  // The signatures of the two classes are as follows:

  //class DataSet(val trainingSet: List[(Int, Int)], val testingSet: List[(Int, Int)])
  //class DataAsIntervals(val trainingSet: List[Interval], val testingSet: List[Interval])

  implicit def toDataset(t: DataAsIntervals): DataSet = {
    val f = (x: List[Interval]) => x map (z => (z.startPoint, z.endPoint))
    new DataSet(trainingSet = f(t.trainingSet), testingSet = f(t.testingSet))
  }

  implicit def toDataset(tl: List[DataAsIntervals]): List[DataSet] = {
    tl map (toDataset(_))
  }
}

/*
object ExmplImplicits {

  implicit def toExample(e: List[Exmpl]): List[Example] = {
    e map (x => new Example(annot = x.exmplWithInertia.annotation, nar = x.exmplWithInertia.narrative, _time = x.exmplWithInertia.time))
  }
}
*/

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

package logic

import com.mongodb.casbah.Imports._
import utils.DataUtils.Data

/**
  *
  * Utilities for representing and handling training examples.
  *
  */

object Examples {


  object Example {

    val f_annot = (x:DBObject) =>
      x.asInstanceOf[BasicDBObject].get("annotation").asInstanceOf[BasicDBList].toList.map(x => x.toString)
    val f_narrative = (x:DBObject) =>
      x.asInstanceOf[BasicDBObject].get("narrative").asInstanceOf[BasicDBList].toList.map(x => x.toString)
    val f_time = (x:DBObject) => x.asInstanceOf[BasicDBObject].get("time").toString


    // Merges a batch into one example with the original annotation (does not throw away annotation for weaks)
    def merge(_exs: List[Example], markedAsWeak: Boolean = false) = {
      _exs match {
        case Nil => new Example()
        case _ =>
          val exs  = _exs.filter(x => x!=Example())
          val startTimePredicate = s"starttime(${exs.map(x => x.time).map(x => x.toInt).distinct.sorted.head})"
          /*
          val annotation = asWeakExmpls match {
            case true => exs.flatMap(x => x.annotation ++ x.suppressedAnnotation).distinct
            case _ => exs.flatMap(x => x.annotation).distinct
          }
          */
          val annotation = exs.flatMap(x => x.annotation).distinct

          val narrative = startTimePredicate :: exs.flatMap(x => x.narrative).filter( p => !p.contains("starttime"))
          val time = exs.map(x => x.time).map(x => x.toInt).distinct.sorted.head.toString
          new Example(annot=annotation,nar=narrative,_time=time,isWeak=markedAsWeak)
      }
    }



    def tranformToExample(examples: List[DBObject], usingWeakExmpls: Boolean = false) = {
      val startTimePredicate = s"starttime(${examples.map(x => f_time(x)).map(x => x.toInt).distinct.sorted.head})"
      val annotation =
         if (!usingWeakExmpls)
           examples.flatMap(x => f_annot(x))
         else examples.tail.flatMap(x => f_annot(x)) // leave the starttime annotation out to enforce learning
      val narrative =
        startTimePredicate :: examples.flatMap(x => f_narrative(x)).filter( p => !p.contains("starttime"))
      //val time = f_time(examples.head)
      val time = examples.map(x => f_time(x)).map(x => x.toInt).distinct.sorted.head.toString
      val suppressedAnnotation = if (usingWeakExmpls) f_annot(examples.head) else List[String]()
      val isWeak = if(examples.length == 2 && f_annot(examples.head).nonEmpty && f_annot(examples.tail.head).nonEmpty) true else false

      new Example(annot=annotation,nar=narrative,_time=time,suppressedAnnotation=suppressedAnnotation,isWeak=isWeak)
    }

    def mergeExamples(_exs: List[Example]) = {
      _exs match {
        case Nil => new Example()
        case _ =>
          val exs  = _exs.filter(x => x!=Example())
          val startTimePredicate = s"starttime(${exs.map(x => x.time).map(x => x.toInt).distinct.sorted.head})"
          val annotation = exs.flatMap(x => x.annotation).distinct
          val narrative = exs.flatMap(x => x.narrative).distinct
          val time = exs.map(x => x.time).map(x => x.toInt).distinct.sorted.head.toString
          new Example(annot=annotation,nar=narrative,_time=time)
      }
    }

    def apply(examples: List[DBObject], usingWeakExmpls: Boolean) = {
      usingWeakExmpls match {
        case false => tranformToExample(examples)
        case _ =>
          val pairs = examples.map(p =>
            if(examples.indexOf(p)+1 < examples.length)
              (p,examples(examples.indexOf(p)+1))
            else (p,p)).filter(x => x._1 != x._2)
          val weaks = for (p <- pairs) yield tranformToExample(List(p._1,p._2), usingWeakExmpls = true)
          new Example(containedExamples = weaks)

      }

    }

    def apply(annotation: List[String],narrative: List[String],time: String) ={}

    def apply(a: scala.collection.mutable.Set[String], n: scala.collection.mutable.Set[String], t: String) = {
      new Example(annot = a.toList, nar = n.toList, _time = t)
    }
  }

  case class Example(e: DBObject = DBObject(),
                     commingFromDB: String = "",
                     private val annot:List[String]=List(),
                     private val nar:List[String]=List(),
                     _time:String="",
                     isWeak: Boolean = false,
                     usingWeakExmpls: Boolean = false,
                     containedExamples: List[Example] = List[Example](),
                     suppressedAnnotation: List[String] = List()) extends Data {

    val annotation: List[String] =
      if (this.annot.isEmpty && this.nar.isEmpty)// then this is really coming from db, so read the DBObject
        if (this.e.nonEmpty)
          e.asInstanceOf[BasicDBObject].get("annotation").asInstanceOf[BasicDBList].toList.map(x => x.toString)
        else this.annot
      else this.annot

    val annotationASP = this.annotation map(x => if(x.contains("example(") || x.contains("negExample(")) s"$x." else s"example($x).")

    val narrative =
      if(this.nar.isEmpty)
        if (this.e.nonEmpty)
          e.asInstanceOf[BasicDBObject].get("narrative").asInstanceOf[BasicDBList].toList.map(x => x.toString)
        else this.nar
      else this.nar

    val narrativeASP = this.narrative map(x => if(x.endsWith(".")) x else s"$x.")

    val time =
      if(this._time == "")
        if (this.e.nonEmpty)
          e.asInstanceOf[BasicDBObject].get("time").toString()
        else this._time
      else this._time

    val isEmpty = e.isEmpty && this.annot.isEmpty && this.nar.isEmpty

    def toMap = Map("annotation" -> this.annotation, "narrative" -> this.narrative)
    def toMapASP = Map("annotation" -> this.annotationASP, "narrative" -> this.narrativeASP)
    def tostring = (annotationASP ++ narrativeASP).mkString("\n")
  }






  case class ExampleBatch(exs: List[DBObject] = List[DBObject]() , usingWeakExmpls: Boolean = false) {

    private val pairs = exs.map(p => if(exs.indexOf(p)+1 < exs.length) (p,exs(exs.indexOf(p)+1)) else (p,p)).filter(x => x._1 != x._2)

    val isEmpty = this.exs == List[DBObject]()

    val examples = for (p <- pairs) yield Example.tranformToExample(List(p._1,p._2), usingWeakExmpls = false)

    val examplesAsWeaks = for (p <- pairs) yield Example.tranformToExample(List(p._1,p._2), usingWeakExmpls = true)

    def weakExmpls = this.examplesAsWeaks.filter(x => x.isWeak)

    def strongExmpls = this.examplesAsWeaks.filter(x => !x.isWeak)

    //def asSingleExmpl = Example(exs,usingWeakExmpls)

    def asSingleExmpl = Example(exs,usingWeakExmpls)

    def without(e: Example): List[Example] = {
      this.examples.filter(x => x.time != e.time)
    }

    // e is a weak example here. It removes its original
    // instance from the batch (which carries annotation
    // by inertia) and adds back its weak version without
    // such annotation
    def withOneFlipped(e: Example) = {
      val asWeakBatch = this.examples map {
        x =>
          new Example(
          annot = x.annotation.filter(y => !e.suppressedAnnotation.contains(y)).distinct,
          nar = x.narrative,
          _time = x.time
          )
      }

      asWeakBatch
      //val without = this.without(e)
      //this.examples :+ e
    }

  }


  /**
    * This class represents a pair of interpretations at two consecutive time points t1, t2 = t1+step.
    * This pair is considered as a single example with narrative consisting of everything true at t1
    * and annotation consisting of the annotation of the second example (the example at t2).
    */
  case class ExamplePair(first: Example = Example(), second: Example = Example(), learnFromWeakExmpls: Boolean = true) {
    val time = first.time
    val isStronglyInitiated = if (first.annotation == List() && second.annotation != List()) true else false
    val isWeaklyInitiated = if (first.annotation != List() && second.annotation != List()) true else false
    val isStronglyTerminated = if (first.annotation != List() && second.annotation == List()) true else false
    val isWeaklyTerminated = if (first.annotation == List() && second.annotation == List()) true else false
    // If we are trying to learn from weak examples also, then
    // we do not need the annotation of of the first example
    val annotation = if (learnFromWeakExmpls) second.annotation else first.annotation ::: second.annotation
    val annotationASP = if (learnFromWeakExmpls) second.annotationASP else first.annotationASP ::: second.annotationASP
    val narrative = first.narrative ::: second.narrative
    val narrativeWithPriorSupervision = first.narrative ::: second.narrative ::: first.annotation
    val narrativeWithPriorSupervisionASP = first.narrativeASP ::: second.narrativeASP ::: first.annotationASP
    val narrativeASP = first.narrativeASP ::: second.narrativeASP ::: first.narrativeASP
    var isCoveredBySomeRule: Boolean = false
  }

  case class LabelledFromMongo(e: DBObject) {
    val commingFromDB = e.get("commingFromDB")
    val headAtomGrnd: String = e.get("headAtomGrnd").toString()
    val headAtomVarbed: String = e.get("headAtomVarbed").toString()
    val timeKey: String = e.get("timeKey").toString()
    val typePreds: String = e.get("typePreds").toString()
    val asGrndInterpretation: List[String] =
      e.asInstanceOf[BasicDBObject].get("asGrndInterpretation").asInstanceOf[BasicDBList].toList.map { x => x.toString() }
    val asVarbedInterpretation: List[String] =
      e.asInstanceOf[BasicDBObject].get("asVarbedInterpretation").asInstanceOf[BasicDBList].toList.map { x => x.toString() }
    var similarExamples = 2.0
  }



}
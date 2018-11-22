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

import ParseCAVIAR.{lleParser1, parseAll}
import com.mongodb.casbah.MongoClient
import com.mongodb.casbah.commons.MongoDBObject
import com.mongodb.casbah.Imports._
import logic.Examples.Example
import logic.{Constant, Literal}

/**
  * Created by nkatz on 6/2/17.
  */

object CopyCAVIAR extends App {


  val numOfCopies = 10
  val idPattern = "id[0-9]+".r
  val originalIds = List("id0", "id1", "id2", "id3", "id4", "id5", "id6", "id7", "id8", "id9").sortBy(x => x.last)
  val IDCopies = originalIds.map(x => x -> (1 until numOfCopies).map(y => x+y)).toMap

  ///*
  val mc = MongoClient()
  val collection = mc("caviar-whole")("examples")

  val newDBName = s"caviarX$numOfCopies"
  val newDB = mc(newDBName)("examples")
  mc.dropDatabase(newDBName)

  for (x <- collection.find()) {
    val e = Example(x)
    val time = e.time.toInt
    println(time)
    val extendedNarrative = copyAtoms(e.narrative, "narrative")
    val extendedAnnotation = copyAtoms(e.annotation, "annotation")
    // Need to get extra annotation (see the generateExtraAnnotation to see what this is about)
    val happensAtoms = e.narrative.filter(x => x.contains("happensAt") && (x.contains("walking") || x.contains("active") || x.contains("inactive")) )
    val extraAnnotation = happensAtoms.flatMap(x => generateExtraAnnotation(x)).distinct
    val entry = MongoDBObject("time" -> time) ++ ("annotation" -> (extendedAnnotation ++ extraAnnotation) ) ++ ("narrative" -> extendedNarrative)
    newDB.insert(entry)
  }
  //*/

  generateExtraAnnotation("happensAt(walking(id1),2347)").foreach(println)

  def extractIds(atom: String) = idPattern.findAllIn(atom).toList

  def replaceIds(atom: String, replaceWith: Map[String, String]) = {
    val ids = idPattern.findAllIn(atom)
    val toLit = if (!atom.contains(".")) Literal.parse(atom) else Literal.parse(atom.split("\\.")(0))
    // CAVIAR-specific stuff.
    // THIS IS FOR ANNOTATION ATOMS ONLY
    val first = replaceWith.head._2
    val second = replaceWith.tail.head._2
    val newLit =
      Literal(functor = toLit.functor,
        terms = List(Literal(
          functor = toLit.terms.head.asInstanceOf[Literal].functor,
          terms = List(Constant(first), Constant(second)) ), toLit.terms.tail.head)).tostring
    newLit
  }

  def copyAtom(atom: String, what: String): List[String] = {
    val ids = extractIds(atom)
    what match {
      case "annotation" =>
        if (ids.length != 2) throw new RuntimeException(s"ids length for annotation atom $atom is != 2")
        val idCopies = ids.flatMap(x => List(x) ++ IDCopies(x))
        // As an extra test for checking that you are producing the annotation correctly,
        // (in addition to just inspecting the outcome), go like this: For n copies of id constants
        // in caviar, you'l have 2n id constants. Then you need to produce C(2n, n)-many (the binomial
        // coefficient -- syndiasmoi 2n ana 2). Multiply that by 2 (because you have a pair (id1, id2)
        // and its reverse (id2, id1)) and you have the correct number. For example, for X3 caviar,
        // instead of a pair of constants toy have 6 constant symbols (id1 -> {id1, id11, id12} and
        // similarly for id2). So you have 2C(6,2) = 30 new pairs of annotation.
        val combinations = idCopies.combinations(2).flatMap(x => List(x, x.reverse))
        combinations.foldLeft(List[String]()){(accum, y) =>
          val idsMap = (ids zip y).toMap
          val newAtom = replaceIds(atom, idsMap)
          accum :+ newAtom
        }
      case "narrative" =>
        if (ids.length != 1) throw new RuntimeException(s"ids length for narrative atom $atom is != 1")
        IDCopies(ids.head).map(x => atom.replace(ids.head, x)) :+ atom toList
      case _ => throw new RuntimeException("Don't know what to do")
    }
  }

  /*
  *
  * If a person is (e.g.) walking alone (without interacting, according to the annotation)
  * with no one else, then that person will be moving with its copies. Similarly when that
  * person is active or inactive, it will then be meeting with its copies. We need to generate
  * the extra annotation, otherwise we'll have a large number of fps.
  *
  * The input to this method is a happensAt atom.
  * */
  def generateExtraAnnotation(atom: String) = {
    val parsed = parseAll(lleParser1, atom).getOrElse(throw new RuntimeException("Can't parse this"))
    val time = parsed.time
    val nextTime = (time.toInt + 40).toString
    val id = parsed.id
    val lle = parsed.what
    val idCopies = List(id) ++ IDCopies(id)
    val combinations = idCopies.combinations(2).flatMap(x => List(x, x.reverse))
    combinations.map { idPair =>
      lle match {
        case "walking" => s"holdsAt(moving(${idPair.head}, ${idPair.tail.head}),$nextTime)"
        case ("active" | "inactive") => s"holdsAt(meeting(${idPair.head}, ${idPair.tail.head}),$nextTime)"
        case _ => ""
      }
    }.toList.filter(x => x != "")
  }

  def copyAtoms(atoms: List[String], what: String) = atoms.flatMap(x => copyAtom(x, what)).distinct



  //copyAtoms(List("holdsAt(meeting(id1, id2),22400)", "holdsAt(meeting(id2, id1),22400)"), "annotation").foreach(println)
  //copyAtoms(List("happensAt(walking(id1),2347)", "coords(id4, 3543, 2342, 75)"), "narrative").foreach(println)

}

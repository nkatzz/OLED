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

package datautils.caviar_data

import app.runutils.Globals
import com.mongodb.casbah.Imports._
import com.mongodb.casbah.MongoClient
import com.mongodb.casbah.commons.MongoDBObject
import logic.Examples.Example
import logic.Literal
import utils.{ASP, Database, Utils}

/**
  * Created by nkatz on 6/20/17.
  */

object GenerateCleanCaviarData {

  def main(args: Array[String]) = {
    generateCleanData("meeting",
      "/home/nkatz/dev/iled/datasets/hand-crafted-rules/caviar/meeting-hand-crafted.lp",
      "/home/nkatz/dev/iled/datasets/Caviar/meeting")
  }

  def generateCleanData(HLE: String, handCraftedRulesPath: String, entryPath: String = "", fromDB: String = ""): Unit = {
    val CaviarDB = "caviar"
    val newDB = s"caviar_${HLE}_clean"

    val mongoClient = MongoClient()
    mongoClient.dropDatabase(newDB)
    val collection = mongoClient(newDB)("examples")

    val gl = new Globals(entryPath, fromDB)
    val (handCraftedRules,show) = HLE match {
      case "meeting" =>
        (handCraftedRulesPath,s"\n#show.\n#show holdsAt($HLE(X,Y),T):holdsAt($HLE(X,Y),T).\n")
      case "moving" =>
        (handCraftedRulesPath,s"\n#show.\n#show holdsAt($HLE(X,Y),T):holdsAt($HLE(X,Y),T).\n")
    }

    val file = Utils.getTempFile("generate",".lp")

    val db = new Database(CaviarDB,"examples")
    db.collection.find().sort(MongoDBObject("time" -> 1)).foldLeft(List[String]()){ (priorAnnotation, newExmpl) =>
      val e = Example(newExmpl)

      if (e.time == "766600") {
        val stop = "stop"
      }

      val narrative = e.narrativeASP
      val in  = narrative ++ priorAnnotation.map(x => x+".") ++ List(s"time(${e.time.toInt+40}).")
      val content = in.mkString("\n") + gl.INCLUDE_BK(gl.BK_WHOLE_EC) + gl.INCLUDE_BK(handCraftedRules) + show
      Utils.writeLine(content,file.getCanonicalPath,"overwrite")
      val out = ASP.solve(task = Globals.INFERENCE, aspInputFile = file)

      val prior =
        if (out.nonEmpty)  {
          out.head.atoms.map( x => (Literal.parse(x).terms(1).tostring,x) ).filter(z => z._1 == e.time).map(_._2)
        } else {
          Nil
        }

      val next =
        if (out.nonEmpty)  {
          out.head.atoms.map( x => (Literal.parse(x).terms(1).tostring,x) ).filter(z => z._1 == (e.time.toInt+40).toString).map(_._2)
        } else {
          Nil
        }

      if (prior.nonEmpty) {
        val stop = "stop"
      }

      val entry = MongoDBObject("time" -> e.time.toInt) ++ ("annotation" -> prior) ++ ("narrative" -> e.narrative)
      println(entry)
      collection.insert(entry)
      next
    }
  }

}

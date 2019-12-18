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

import java.io.File

import ParseCAVIAR._
import com.mongodb.casbah.MongoClient
import com.mongodb.casbah.commons.MongoDBObject
import com.mongodb.casbah.Imports._
import scala.collection.immutable.SortedMap

object ParseCAVIAR_DB_per_video {

  def main(args: Array[String]) = {

    val dataPath = args(0)

    run(dataPath)

  }

  def run(path: String) = {
    //val dbName = if (fixedBorders) fixedBordersDBName else originalDBName
    val mongoClient = MongoClient()

    val d = new File(path)
    val innerFolders = d.listFiles.sortBy(_.getName.split("-")(0).toInt)

    //var lastTime = 0

    var videoCounter = 0

    for (f <- innerFolders) {
      videoCounter += 1
      println(s"Parsing video ${f.getCanonicalPath}")
      val files = f.listFiles.filter(x => dataFileNames.exists(p => x.getName.contains(p)))
      val contents =
        (for (f <- files)
          yield scala.io.Source.fromFile(f).getLines().filter(p => !p.startsWith("%"))).
          toList.flatten.mkString.replaceAll("\\s", "").split("\\."
          ).toList

      val parsed = contents.flatMap(x =>
        //parseAll(caviarParser(lastTime),x).getOrElse(List(""))).filter(_!="").asInstanceOf[List[Atom]]
        parseAll(caviarParser(0), x).getOrElse(List(""))).filter(_ != "").asInstanceOf[List[Atom]]

      val atoms = SortedMap[Int, List[Atom]]() ++ parsed.groupBy(_.time.toInt)

      /*
      for ( (k,v) <- atoms ) {
        val narrative = v.filter(x => !x.annotationAtom).flatMap(z => z.atoms)
        val annotation = v.filter(x => x.annotationAtom).flatMap(z => z.atoms)
        val entry = MongoDBObject("time" -> k) ++ ("annotation" -> annotation) ++ ("narrative" -> narrative)
        collection.insert(entry)
      }
      */

      var hasMeeting = false
      var hasMoving = false

      val dbEntries = atoms.foldLeft(Vector[DBObject]()) { (entries, mapRecord) =>
        val (time, atoms) = (mapRecord._1, mapRecord._2)
        val narrative = atoms.filter(x => !x.annotationAtom).flatMap(z => z.atoms)
        val annotation = atoms.filter(x => x.annotationAtom).flatMap(z => z.atoms)

        if (annotation.exists(p => p.contains("meeting"))) hasMeeting = true
        if (annotation.exists(p => p.contains("moving"))) hasMoving = true

        val entry = MongoDBObject("time" -> time) ++ ("annotation" -> annotation) ++ ("narrative" -> narrative)
        entries :+ entry
      }

      val dbName =
        if (hasMeeting && hasMoving) s"caviar-video-$videoCounter-meeting-moving"
        else if (hasMeeting) s"caviar-video-$videoCounter-meeting"
        else if (hasMoving) s"caviar-video-$videoCounter-moving"
        else s"caviar-video-$videoCounter"

      mongoClient.dropDatabase(dbName)
      val collection = mongoClient(dbName)("examples")

      println(s"Inserting data in $dbName")

      dbEntries foreach (entry => collection.insert(entry))

      //lastTime = atoms.keySet.toList.reverse.head+40 // the last time point
    }
  }
}

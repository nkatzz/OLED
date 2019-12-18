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

package experiments.datautils.maritime_data

import com.mongodb.{BasicDBList, BasicDBObject, DBObject}
import com.mongodb.casbah.Imports._

/**
  * Created by nkatz on 6/23/17.
  */

object Structures {

  trait Entry

  /* An interpretation containing all info for a single time point. */
  case class BDEntry(time: Int, lles: List[String], hles: List[String], vessels: List[String], areas: List[String]) extends Entry

  case class LLE(lleName: String, atom: String, time: Int, vessel: String, area: String) extends Entry

  /*In HLEs vessels are in list because randezvous (for example) involves two vessels*/
  case class HLE(hleName: String, atom: String, time: Int, vessels: List[String], area: String) extends Entry

  class MaritimeExample(val time: Int, val lle: String, val hle: String, val vessels: List[String], val areas: List[String])

  object MaritimeExample {
    def apply(o: DBObject) = {
      val time = o.asInstanceOf[BasicDBObject].get("time").toString.toInt
      val lle = o.asInstanceOf[BasicDBObject].get("lle").toString
      val hle = o.asInstanceOf[BasicDBObject].get("hle").toString
      val vessels = o.asInstanceOf[BasicDBObject].get("vessels").asInstanceOf[BasicDBList].toList.map(_.toString)
      val areas = o.asInstanceOf[BasicDBObject].get("areas").asInstanceOf[BasicDBList].toList.map(_.toString)
      new MaritimeExample(time, lle, hle, vessels, areas)
    }
  }

}

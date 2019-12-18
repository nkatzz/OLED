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

package woled

import java.io.{File, FileWriter}
import java.util.UUID

/**
  * Created by nkatz at 5/10/19
  */

object Utils {

  def dumpToFile(input: Any, file: String = "", howTowrite: String = "overwrite") = {

      /* Write an iterable to file. Usage:
     * listToFile(new File("example.txt")) { p => data.foreach(p.println) }
     */
      def listToFile(f: java.io.File, mode: String)(op: java.io.PrintWriter => Unit) {

        val p = mode match {
          case "append" => new java.io.PrintWriter(new FileWriter(f, true))
          case "overwrite" => new java.io.PrintWriter(new FileWriter(f, false))
          case _ => new java.io.PrintWriter(new FileWriter(f, false)) // default is overwrite
        }
        try { op(p) } finally { p.close() }
      }

    val writeTo =
      if (file == "") File.createTempFile(s"temp-${System.currentTimeMillis()}-${UUID.randomUUID.toString}", "asp")
      else new File(file)

    val deleteOnExit = if (file == "") true else false

    val mode = if (file == "") "overwrite" else howTowrite

    input match {
      case in: Iterable[String] => listToFile(writeTo, mode) { p => in.foreach(p.println) }
      case in: String => listToFile(writeTo, mode) { p => Vector(in).foreach(p.println) }
    }

    if (deleteOnExit) writeTo.deleteOnExit()
    writeTo
  }

}

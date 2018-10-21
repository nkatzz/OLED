package datautils.tracknow

import java.io.{File, PrintWriter}
import scala.io.Source
import util.Try

object TK extends App {

  val dataPath = "/media/nkatz/storage/Zelitron-data/data"
  val rtecDataPath = "/media/nkatz/storage/Zelitron-data/RTEC_data"

  /*
  object MyFile {
    def apply(f: File): MyFile = {
      val split = f.getName.split("_")(2).split("-")
      val (year, month, day) = (split(0).toInt, split(1).toInt, split(2).toInt)
      MyFile(year, month, day, f)
    }
  }
  case class MyFile(year: Int, month: Int, day: Int, file: File)
  */

  def rename(oldName: String, newName: String) = {
    Try(new File(oldName).renameTo(new File(newName))).getOrElse(false)
  }


  def getListOfFiles(dir: String) = {
    val d = new File(dir)
    var i = 0
    val k = d.listFiles
    d.listFiles foreach { x =>

      val files = x.listFiles.sortBy{ f =>
        val split = f.getName.split("_")(2).split("-")
        val (year, month, day) = (split(0).toInt, split(1).toInt, split(2).toInt)
        (year, month, day)
      }

      val newFileName = s"$rtecDataPath/$i.csv"
      val pw = new PrintWriter(new File(newFileName))

      var firstTime = "non-set-yet"
      var endTime = "non-set-yet"

      files foreach { f =>
        val source = Source.fromFile(f.getAbsolutePath)
        try {
          val lines = source.getLines
          lines foreach { line =>
            val split = line.split(";")
            val company = split(0)
            val vehicle = split(1)
            val format = new java.text.SimpleDateFormat("yyyy-MM-dd HH:mm:ss.SSS")
            val ttime = split(2)
            val time = format.parse(ttime).getTime()

            if (firstTime == "non-set-yet") firstTime = time.toString
            endTime = time.toString

            val engineStatus = split(3)
            val speed = split(10)
            val ttype = split(23).replaceAll("\\s", "").replaceAll(",","").toLowerCase
            /*
            if (engineStatus != "0" && engineStatus != "3") {
              val newLine = s"speed|$time|$time|$vehicle|$ttype|$speed"
              pw.write(newLine+"\n")
            }
            */
            val newLine = s"speed|$time|$time|$vehicle|$ttype|$speed"
            pw.write(newLine+"\n")
          }
        } finally { source.close() }
      }
      pw.close()
      rename(newFileName, s"$rtecDataPath/$i-$firstTime-$endTime.csv")
      i += 1
      println(i+" complete.")
    }
  }

  println(getListOfFiles(dataPath))

}

package datautils.tracknow

import java.io.File
import scala.sys.process._

object RunER {

  /* Command to find all files of size larger than 400MBs:
   * find . -size +400M */

  def main(args: Array[String]) = {

    val winSize = args(0) // winsize in millis
    val statsPath = args(1) //
    val pattenrsPath = args(2)

    val RTECDataPath = "/media/nkatz/storage/Zelitron-data/RTEC_data"

    val dir = new File(RTECDataPath)

    val t0 = System.nanoTime()

    var i = 0

    def fileSizeInMBs(f: File) = {
      val fileSizeInBytes = f.length()
      val fileSizeInKBs = fileSizeInBytes / 1024
      val fileSizeInMBs = fileSizeInKBs / 1024
      //val fileSizeInGBs = fileSizeInMBs / 1024
      fileSizeInMBs
    }

    dir.listFiles.sortBy( x => x.getName.split("-")(0).toInt ) foreach { f =>

      println(fileSizeInMBs(f))

      if(fileSizeInMBs(f) <= 100) {
        val fname = f.getName
        val fnameNoExt = fname.split("\\.")(0)
        val split = fnameNoExt.split("-")
        val (first, last) = (split(1), split(2))

        //val query = s"performFullER(['${f.getAbsolutePath}']," +
        //  s"'/home/nkatz/Downloads/RTEC-master/examples/track-know/results/$fnameNoExt-stats.txt'," +
        //  s"'/home/nkatz/Downloads/RTEC-master/examples/track-know/results/$fnameNoExt-patterns.txt',$first,3600000,3600000,$last),halt."


        val query = s"performFullER(['${f.getAbsolutePath}']," +
          s"'$statsPath/$fnameNoExt-stats.txt'," +
          s"'$pattenrsPath/$fnameNoExt-patterns.txt',$first,$winSize,$winSize,$last),halt."



        println(s"PROCESSING BATCH: $i | file: ${f.getName} | size: ${fileSizeInMBs(f)}")

        val cmd = Seq("yap", "-q", "-f", "/home/nkatz/Downloads/RTEC-master/examples/track-know/loader.prolog", "-g", query)
        //println(cmd.mkString(" "))
        //val result = cmd.mkString(" ").lineStream_!
        cmd.mkString(" ").!
        i += 1
      }
    }

    val t1 = System.nanoTime()
    println(s"Total time: ${(t1-t0)/1000000000.0}")
  }


}

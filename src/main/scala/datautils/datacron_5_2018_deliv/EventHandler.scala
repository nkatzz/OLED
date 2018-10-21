package datautils.datacron_5_2018_deliv

object EventHandler {

  //--------------------------------
  // These are the low-level events:
  //--------------------------------
  // change_in_heading|1443694493|1443694493|245257000
  // change_in_speed_start|1443890320|1443890320|259019000
  // change_in_speed_end|1443916603|1443916603|311018500
  // coord|1443686670|1443686670|228041600|-4.47298500000000043286|48.38163999999999731472
  // entersArea|1443875806|1443875806|228017700|18515
  // leavesArea|1443887789|1443887789|235113366|21501
  // gap_start|1444316024|1444316024|246043000
  // gap_end|1445063602|1445063602|269103970
  // proximity|1445357304|1445354425|1445357304|true|227519920|227521960
  // velocity|1443665064|1443665064|228374000|19.30468943370810563920|31.39999999999999857891|35.54927442329611153582
  // slow_motion_start|1444262324|1444262324|228167900
  // slow_motion_end|1444281397|1444281397|258088080
  // stop_start|1444031990|1444031990|227705102
  // stop_end|1444187303|1444187303|259019000

  //----------------------------------------------------------------------------------------
  // For: change_in_heading, change_in_speed_start, change_in_speed_end, gap_start, gap_end,
  //      slow_motion_start, slow_motion_end, stop_start, stop_end
  // we have that:
  // lle = split(0)
  // time = split(1)
  // vessel = split(3)
  // For these the generated event instance should be:
  // happensAt(lle(vessel),time) and
  // HappensAt(lle_vessel, time) (for MLN)
  //----------------------------------------------------------------------------------------

  // mode is either "asp" or "mln"
  def generateLLEInstances(line: String, mode: String) = {
    // These have a different schema
    val abnormalLLEs = Set[String]("coord", "entersArea", "leavesArea", "proximity", "velocity")
    val split = line.split("\\|")
    if (! abnormalLLEs.contains(split(0))) {

      // These have a common simple schema:
      // change_in_heading, change_in_speed_start, change_in_speed_end,
      // gap_start, gap_end, slow_motion_start, slow_motion_end, stop_start, stop_end
      val lle = split(0)
      val time = split(2)
      val vessel = split(1)
      if ("mode" == "asp") s"happensAt($lle($vessel),$time)" else s"HappensAt(${lle.capitalize}_$vessel),$time)"
    } else {

      if (split(0) == "coord") {
        //coord|1443686670|1443686670|228041600|-4.47298500000000043286|48.38163999999999731472
        /*
        val lle = split(0)
        val time = split(1)
        val vessel = split(3)
        val lon = split(4)
        val lat = split(5)
        // Don't return nothing in the MLN case (can't handle the coords syntax)
        if ("mode" == "asp") s"happensAt($lle($vessel,$lon,$lat),$time)" else ""
        */
        // do nothing (we won't use coord).

      } else if (split(0) == "entersArea" || split(0) == "leavesArea") {
        //entersArea|1443875806|1443875806|228017700|18515
        val lle = split(0)
        val time = split(3)
        val vessel = split(1)
        val area = split(2)
        if ("mode" == "asp") s"happensAt($lle($vessel,$area),$time)"
        else s"HappensAt(${lle.capitalize}_${vessel}_$area,$time)"

      } else if (split(0) == "velocity") {
        // do nothing (we won't use velocity)
      } else if (split(0) == "proximity") {
        val vessel1 = split(1)
        val vessel2 = split(2)
        val time = split(3)
        if ("mode" == "asp") s"happensAt(close($vessel1,$vessel1),$time)"
        else s"HappensAt(Close_${vessel1}_$vessel1,$time)"
      } else {
        throw new RuntimeException(s"Unexpected event: $line")
      }
    }
  }

}

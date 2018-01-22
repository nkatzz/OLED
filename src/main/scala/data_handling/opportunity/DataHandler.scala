package data_handling.opportunity

import java.io.File

import scala.io.Source

object DataHandler extends App {

  val labelsPath  = "/home/nkaz/dev/OportunityUCDataset/dataset/label_legend.txt"
  val dataFilePath = "/home/nkaz/dev/OportunityUCDataset/dataset/S1-ADL1.dat"

  val person = new File(dataFilePath).getName.split("-")(0).toLowerCase

  val events = List("0 none","1 stand","2 walk","4 sit","5 lie","101 relaxing","102 coffee_time","103 early_morning","104 cleanup","105 sandwich_time",
    "201 unlock","202 stir","203 lock","204 close","205 reach","206 open","207 sip","208 clean","209 bite","210 cut","211 spread","212 release",
    "213 move","301 bottle","302 salami","303 bread","304 sugar","305 dishwasher","306 switch","307 milk","308 drawer3","309 spoon","310 knife_cheese",
    "311 drawer2","312 table","313 glass","314 cheese","315 chair","316 door1","317 door2","318 plate","319 drawer1","320 fridge","321 cup",
    "322 knife_salami","323 lazychair","401 unlock","402 stir","403 lock","404 close","405 reach","406 open","407 sip","408 clean","409 bite","410 cut",
    "411 spread","412 release","413 move","501 Bottle","502 salami","503 bread","504 sugar","505 dishwasher","506 switch","507 milk","508 drawer3",
    "509 spoon","510 knife_cheese","511 drawer2","512 table","513 glass","514 cheese","515 chair","516 door1","517 door2","518 plate","519 drawer1",
    "520 fridge","521 cup","522 knife_salami","523 lazychair","406516 open_door1","406517 open_door2","404516 close_door1","404517 close_door2",
    "406520 open_fridge","404520 close_fridge","406505 open_dishwasher","404505 close_dishwasher","406519 open_drawer1","404519 close_drawer1",
    "406511 open_drawer2","404511 close_drawer2","406508 open_drawer3","404508 close_drawer3","408512 clean_table","407521 drink_fromCup","405506 toggle_switch")

  val labels = events.map{x => val s = x.split(" ") ; (s(0), s(1))} toMap

  val columns = Map("locomotion" -> 244, "HL_activity" -> 245, "left_arm" -> 246,
    "left_arm_obj" -> 247, "right_arm" -> 248, "right_arm_obj" -> 249, "both_arms" -> 250)

  val lines = Source.fromFile(dataFilePath).getLines.map(x => x.split(" "))

  var time = 0

  lines foreach { line =>
    val a = line(columns("locomotion")-1)
    val b = line(columns("left_arm")-1)
    val c = line(columns("left_arm_obj")-1)
    val d = line(columns("right_arm")-1)
    val e = line(columns("right_arm_obj")-1)
    val f = line(columns("both_arms")-1)
    val h = line(columns("HL_activity")-1)
    val locomotion = if (labels(a) != "none") s"happensAt(${labels(a)}($person),$time)" else ""
    val leftArm = if (labels(b) != "none" && labels(c) != "none") s"happensAt(${labels(b)}($person,${labels(c)}),$time)" else ""
    val rightArm = if (labels(d) != "none" && labels(e) != "none") s"happensAt(${labels(d)}($person,${labels(e)}),$time)" else ""
    val bothArms = if (labels(f) != "none") s"happensAt(${labels(f).split("_")(0)}($person,${labels(f).split("_")(1)}),$time)" else ""
    val highLevelActivity = if (labels(h) != "none") s"holdsAt(${labels(h)}($person),$time)" else ""

    val all = List(locomotion, leftArm, rightArm, bothArms, highLevelActivity).distinct.filter(x => x != "")

    println(all)
    time += 1
  }




}

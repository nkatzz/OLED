package experiments.caviar

object MeetingTrainTestSets {

  /* Train/test sets are identified by the mongo db name of each video */

  /*
  private val train1 =
    Vector("caviar-video-2-meeting-moving", "caviar-video-5",
      "caviar-video-6", "caviar-video-13-meeting", "caviar-video-7", "caviar-video-8",
      "caviar-video-14-meeting-moving", "caviar-video-9", "caviar-video-10",
      "caviar-video-19-meeting-moving", "caviar-video-11", "caviar-video-12-moving",
      "caviar-video-20-meeting-moving", "caviar-video-15", "caviar-video-16",
      "caviar-video-21-meeting-moving", "caviar-video-17", "caviar-video-18",
      "caviar-video-22-meeting-moving", "caviar-video-23-moving", "caviar-video-25",
      "caviar-video-24-meeting-moving", "caviar-video-26", "caviar-video-27",
      "caviar-video-28-meeting", "caviar-video-29", "caviar-video-30")
  private val test1 = Vector("caviar-video-1-meeting-moving", "caviar-video-3", "caviar-video-4")

  val meeting1: (Vector[String], Vector[String]) = (train1, test1)

  private val train2 =
    Vector("caviar-video-1-meeting-moving", "caviar-video-3", "caviar-video-4",
    "caviar-video-13-meeting", "caviar-video-7", "caviar-video-8",
    "caviar-video-14-meeting-moving", "caviar-video-9", "caviar-video-10",
    "caviar-video-19-meeting-moving", "caviar-video-11", "caviar-video-12-moving",
    "caviar-video-20-meeting-moving", "caviar-video-15", "caviar-video-16",
    "caviar-video-21-meeting-moving", "caviar-video-17", "caviar-video-18",
    "caviar-video-22-meeting-moving", "caviar-video-23-moving", "caviar-video-25",
    "caviar-video-24-meeting-moving", "caviar-video-26", "caviar-video-27",
    "caviar-video-28-meeting", "caviar-video-29", "caviar-video-30")
  private val test2 = Vector("caviar-video-2-meeting-moving", "caviar-video-5", "caviar-video-6")

  val meeting2: (Vector[String], Vector[String]) = (train2, test2)

  private val train3 =
    Vector("caviar-video-1-meeting-moving", "caviar-video-3", "caviar-video-4",
    "caviar-video-2-meeting-moving", "caviar-video-5", "caviar-video-6",
      "caviar-video-14-meeting-moving", "caviar-video-9", "caviar-video-10",
      "caviar-video-19-meeting-moving", "caviar-video-11", "caviar-video-12-moving",
      "caviar-video-20-meeting-moving", "caviar-video-15", "caviar-video-16",
      "caviar-video-21-meeting-moving", "caviar-video-17", "caviar-video-18",
      "caviar-video-22-meeting-moving", "caviar-video-23-moving", "caviar-video-25",
      "caviar-video-24-meeting-moving", "caviar-video-26", "caviar-video-27",
      "caviar-video-28-meeting", "caviar-video-29", "caviar-video-30")
  private val test3 = Vector("caviar-video-13-meeting", "caviar-video-7", "caviar-video-8")

  val meeting3: (Vector[String], Vector[String]) = (train3, test3)

  private val train4 =
    Vector("caviar-video-1-meeting-moving", "caviar-video-3", "caviar-video-4",
      "caviar-video-2-meeting-moving", "caviar-video-5", "caviar-video-6",
      "caviar-video-13-meeting", "caviar-video-7", "caviar-video-8",
      "caviar-video-19-meeting-moving", "caviar-video-11", "caviar-video-12-moving",
      "caviar-video-20-meeting-moving", "caviar-video-15", "caviar-video-16",
      "caviar-video-21-meeting-moving", "caviar-video-17", "caviar-video-18",
      "caviar-video-22-meeting-moving", "caviar-video-23-moving", "caviar-video-25",
      "caviar-video-24-meeting-moving", "caviar-video-26", "caviar-video-27",
      "caviar-video-28-meeting", "caviar-video-29", "caviar-video-30")
  private val test4 = Vector("caviar-video-14-meeting-moving", "caviar-video-9", "caviar-video-10")

  val meeting4: (Vector[String], Vector[String]) = (train4, test4)

  private val train5 =
    Vector("caviar-video-1-meeting-moving", "caviar-video-3", "caviar-video-4",
      "caviar-video-2-meeting-moving", "caviar-video-5", "caviar-video-6",
      "caviar-video-13-meeting", "caviar-video-7", "caviar-video-8",
      "caviar-video-14-meeting-moving", "caviar-video-9", "caviar-video-10",
      "caviar-video-20-meeting-moving", "caviar-video-15", "caviar-video-16",
      "caviar-video-21-meeting-moving", "caviar-video-17", "caviar-video-18",
      "caviar-video-22-meeting-moving", "caviar-video-23-moving", "caviar-video-25",
      "caviar-video-24-meeting-moving", "caviar-video-26", "caviar-video-27",
      "caviar-video-28-meeting", "caviar-video-29", "caviar-video-30")
  private val test5 = Vector("caviar-video-19-meeting-moving", "caviar-video-11", "caviar-video-12-moving")

  val meeting5: (Vector[String], Vector[String]) = (train5, test5)

  private val train6 =
    Vector("caviar-video-1-meeting-moving", "caviar-video-3", "caviar-video-4",
      "caviar-video-2-meeting-moving", "caviar-video-5", "caviar-video-6",
      "caviar-video-13-meeting", "caviar-video-7", "caviar-video-8",
      "caviar-video-14-meeting-moving", "caviar-video-9", "caviar-video-10",
      "caviar-video-19-meeting-moving", "caviar-video-11", "caviar-video-12-moving",
      "caviar-video-21-meeting-moving", "caviar-video-17", "caviar-video-18",
      "caviar-video-22-meeting-moving", "caviar-video-23-moving", "caviar-video-25",
      "caviar-video-24-meeting-moving", "caviar-video-26", "caviar-video-27",
      "caviar-video-28-meeting", "caviar-video-29", "caviar-video-30")
  private val test6 = Vector("caviar-video-20-meeting-moving", "caviar-video-15", "caviar-video-16")

  val meeting6: (Vector[String], Vector[String]) = (train6, test6)

  private val train7 =
    Vector("caviar-video-1-meeting-moving", "caviar-video-3", "caviar-video-4",
      "caviar-video-2-meeting-moving", "caviar-video-5", "caviar-video-6",
      "caviar-video-13-meeting", "caviar-video-7", "caviar-video-8",
      "caviar-video-14-meeting-moving", "caviar-video-9", "caviar-video-10",
      "caviar-video-19-meeting-moving", "caviar-video-11", "caviar-video-12-moving",
      "caviar-video-20-meeting-moving", "caviar-video-15", "caviar-video-16",
      "caviar-video-22-meeting-moving", "caviar-video-23-moving", "caviar-video-25",
      "caviar-video-24-meeting-moving", "caviar-video-26", "caviar-video-27",
      "caviar-video-28-meeting", "caviar-video-29", "caviar-video-30")
  private val test7 = Vector("caviar-video-21-meeting-moving", "caviar-video-17", "caviar-video-18")

  val meeting7: (Vector[String], Vector[String]) = (train7, test7)

  private val train8 =
    Vector("caviar-video-1-meeting-moving", "caviar-video-3", "caviar-video-4",
      "caviar-video-2-meeting-moving", "caviar-video-5", "caviar-video-6",
      "caviar-video-13-meeting", "caviar-video-7", "caviar-video-8",
      "caviar-video-14-meeting-moving", "caviar-video-9", "caviar-video-10",
      "caviar-video-19-meeting-moving", "caviar-video-11", "caviar-video-12-moving",
      "caviar-video-20-meeting-moving", "caviar-video-15", "caviar-video-16",
      "caviar-video-21-meeting-moving", "caviar-video-17", "caviar-video-18",
      "caviar-video-24-meeting-moving", "caviar-video-26", "caviar-video-27",
      "caviar-video-28-meeting", "caviar-video-29", "caviar-video-30")
  private val test8 = Vector("caviar-video-22-meeting-moving", "caviar-video-23-moving", "caviar-video-25")

  val meeting8: (Vector[String], Vector[String]) = (train8, test8)

  private val train9 =
    Vector("caviar-video-1-meeting-moving", "caviar-video-3", "caviar-video-4",
      "caviar-video-2-meeting-moving", "caviar-video-5", "caviar-video-6",
      "caviar-video-13-meeting", "caviar-video-7", "caviar-video-8",
      "caviar-video-14-meeting-moving", "caviar-video-9", "caviar-video-10",
      "caviar-video-19-meeting-moving", "caviar-video-11", "caviar-video-12-moving",
      "caviar-video-20-meeting-moving", "caviar-video-15", "caviar-video-16",
      "caviar-video-21-meeting-moving", "caviar-video-17", "caviar-video-18",
      "caviar-video-22-meeting-moving", "caviar-video-23-moving", "caviar-video-25",
      "caviar-video-28-meeting", "caviar-video-29", "caviar-video-30")
  private val test9 = Vector("caviar-video-24-meeting-moving", "caviar-video-26", "caviar-video-27")

  val meeting9: (Vector[String], Vector[String]) = (train9, test9)

  private val train10 =
    Vector("caviar-video-1-meeting-moving", "caviar-video-3", "caviar-video-4",
      "caviar-video-2-meeting-moving", "caviar-video-5", "caviar-video-6",
      "caviar-video-13-meeting", "caviar-video-7", "caviar-video-8",
      "caviar-video-14-meeting-moving", "caviar-video-9", "caviar-video-10",
      "caviar-video-19-meeting-moving", "caviar-video-11", "caviar-video-12-moving",
      "caviar-video-20-meeting-moving", "caviar-video-15", "caviar-video-16",
      "caviar-video-21-meeting-moving", "caviar-video-17", "caviar-video-18",
      "caviar-video-22-meeting-moving", "caviar-video-23-moving", "caviar-video-25",
      "caviar-video-24-meeting-moving", "caviar-video-26", "caviar-video-27")
  private val test10 = Vector("caviar-video-28-meeting", "caviar-video-29", "caviar-video-30")

  val meeting10: (Vector[String], Vector[String]) = (train10, test10)
  */




  private val train1 =
    Vector("caviar-video-2-meeting-moving", "caviar-video-5",
      "caviar-video-6", "caviar-video-13-meeting", "caviar-video-7", "caviar-video-8",
      "caviar-video-14-meeting-moving", "caviar-video-9", "caviar-video-10",
      "caviar-video-19-meeting-moving", "caviar-video-11", "caviar-video-12-moving",
      "caviar-video-20-meeting-moving", "caviar-video-15", "caviar-video-16",
      "caviar-video-21-meeting-moving", "caviar-video-17", "caviar-video-18",
      "caviar-video-22-meeting-moving", "caviar-video-23-moving", "caviar-video-25",
      "caviar-video-24-meeting-moving", "caviar-video-26", "caviar-video-27",
      "caviar-video-28-meeting", "caviar-video-29", "caviar-video-4")
  private val test1 = Vector("caviar-video-1-meeting-moving", "caviar-video-3", "caviar-video-30")

  val meeting1: (Vector[String], Vector[String]) = (train1, test1)


  private val train2 =
    Vector("caviar-video-1-meeting-moving", "caviar-video-3", "caviar-video-6",
      "caviar-video-13-meeting", "caviar-video-7", "caviar-video-8",
      "caviar-video-14-meeting-moving", "caviar-video-9", "caviar-video-10",
      "caviar-video-19-meeting-moving", "caviar-video-11", "caviar-video-12-moving",
      "caviar-video-20-meeting-moving", "caviar-video-15", "caviar-video-16",
      "caviar-video-21-meeting-moving", "caviar-video-17", "caviar-video-18",
      "caviar-video-22-meeting-moving", "caviar-video-23-moving", "caviar-video-25",
      "caviar-video-24-meeting-moving", "caviar-video-26", "caviar-video-27",
      "caviar-video-28-meeting", "caviar-video-5", "caviar-video-30")
  private val test2 = Vector("caviar-video-2-meeting-moving", "caviar-video-29", "caviar-video-4")

  val meeting2: (Vector[String], Vector[String]) = (train2, test2)


  private val train3 =
    Vector("caviar-video-1-meeting-moving", "caviar-video-3", "caviar-video-4",
      "caviar-video-2-meeting-moving", "caviar-video-8", "caviar-video-6",
      "caviar-video-14-meeting-moving", "caviar-video-9", "caviar-video-10",
      "caviar-video-19-meeting-moving", "caviar-video-11", "caviar-video-12-moving",
      "caviar-video-20-meeting-moving", "caviar-video-15", "caviar-video-16",
      "caviar-video-21-meeting-moving", "caviar-video-17", "caviar-video-18",
      "caviar-video-22-meeting-moving", "caviar-video-23-moving", "caviar-video-25",
      "caviar-video-24-meeting-moving", "caviar-video-26", "caviar-video-7",
      "caviar-video-28-meeting", "caviar-video-29", "caviar-video-30")
  private val test3 = Vector("caviar-video-13-meeting", "caviar-video-27", "caviar-video-5")

  val meeting3: (Vector[String], Vector[String]) = (train3, test3)


  private val train4 =
    Vector("caviar-video-1-meeting-moving", "caviar-video-3", "caviar-video-4",
      "caviar-video-2-meeting-moving", "caviar-video-5", "caviar-video-10",
      "caviar-video-13-meeting", "caviar-video-7", "caviar-video-8",
      "caviar-video-19-meeting-moving", "caviar-video-11", "caviar-video-12-moving",
      "caviar-video-20-meeting-moving", "caviar-video-15", "caviar-video-16",
      "caviar-video-21-meeting-moving", "caviar-video-17", "caviar-video-18",
      "caviar-video-22-meeting-moving", "caviar-video-23-moving", "caviar-video-25",
      "caviar-video-24-meeting-moving", "caviar-video-9", "caviar-video-27",
      "caviar-video-28-meeting", "caviar-video-29", "caviar-video-30")
  private val test4 = Vector("caviar-video-14-meeting-moving", "caviar-video-26", "caviar-video-6")

  val meeting4: (Vector[String], Vector[String]) = (train4, test4)

  private val train5 =
    Vector("caviar-video-1-meeting-moving", "caviar-video-3", "caviar-video-4",
      "caviar-video-2-meeting-moving", "caviar-video-5", "caviar-video-6",
      "caviar-video-13-meeting", "caviar-video-12-moving", "caviar-video-8",
      "caviar-video-14-meeting-moving", "caviar-video-9", "caviar-video-10",
      "caviar-video-20-meeting-moving", "caviar-video-15", "caviar-video-16",
      "caviar-video-21-meeting-moving", "caviar-video-17", "caviar-video-18",
      "caviar-video-22-meeting-moving", "caviar-video-23-moving", "caviar-video-11",
      "caviar-video-24-meeting-moving", "caviar-video-26", "caviar-video-27",
      "caviar-video-28-meeting", "caviar-video-29", "caviar-video-30")
  private val test5 = Vector("caviar-video-19-meeting-moving", "caviar-video-25", "caviar-video-7")

  val meeting5: (Vector[String], Vector[String]) = (train5, test5)

  private val train6 =
    Vector("caviar-video-1-meeting-moving", "caviar-video-3", "caviar-video-4",
      "caviar-video-2-meeting-moving", "caviar-video-5", "caviar-video-6",
      "caviar-video-13-meeting", "caviar-video-7", "caviar-video-16",
      "caviar-video-14-meeting-moving", "caviar-video-9", "caviar-video-10",
      "caviar-video-19-meeting-moving", "caviar-video-11", "caviar-video-12-moving",
      "caviar-video-21-meeting-moving", "caviar-video-17", "caviar-video-18",
      "caviar-video-22-meeting-moving", "caviar-video-15", "caviar-video-25",
      "caviar-video-24-meeting-moving", "caviar-video-26", "caviar-video-27",
      "caviar-video-28-meeting", "caviar-video-29", "caviar-video-30")
  private val test6 = Vector("caviar-video-20-meeting-moving", "caviar-video-23-moving", "caviar-video-8")

  val meeting6: (Vector[String], Vector[String]) = (train6, test6)

  private val train7 =
    Vector("caviar-video-1-meeting-moving", "caviar-video-3", "caviar-video-4",
      "caviar-video-2-meeting-moving", "caviar-video-5", "caviar-video-6",
      "caviar-video-13-meeting", "caviar-video-7", "caviar-video-8",
      "caviar-video-14-meeting-moving", "caviar-video-17", "caviar-video-10",
      "caviar-video-19-meeting-moving", "caviar-video-11", "caviar-video-12-moving",
      "caviar-video-20-meeting-moving", "caviar-video-15", "caviar-video-16",
      "caviar-video-22-meeting-moving", "caviar-video-23-moving", "caviar-video-25",
      "caviar-video-24-meeting-moving", "caviar-video-26", "caviar-video-27",
      "caviar-video-28-meeting", "caviar-video-29", "caviar-video-30")
  private val test7 = Vector("caviar-video-21-meeting-moving", "caviar-video-9", "caviar-video-18")

  val meeting7: (Vector[String], Vector[String]) = (train7, test7)

  private val train8 =
    Vector("caviar-video-1-meeting-moving", "caviar-video-3", "caviar-video-4",
      "caviar-video-2-meeting-moving", "caviar-video-5", "caviar-video-6",
      "caviar-video-13-meeting", "caviar-video-7", "caviar-video-8",
      "caviar-video-14-meeting-moving", "caviar-video-9", "caviar-video-25",
      "caviar-video-19-meeting-moving", "caviar-video-11", "caviar-video-12-moving",
      "caviar-video-20-meeting-moving", "caviar-video-15", "caviar-video-16",
      "caviar-video-21-meeting-moving", "caviar-video-23-moving", "caviar-video-18",
      "caviar-video-24-meeting-moving", "caviar-video-26", "caviar-video-27",
      "caviar-video-28-meeting", "caviar-video-29", "caviar-video-30")
  private val test8 = Vector("caviar-video-22-meeting-moving", "caviar-video-17", "caviar-video-10")

  val meeting8: (Vector[String], Vector[String]) = (train8, test8)

  private val train9 =
    Vector("caviar-video-1-meeting-moving", "caviar-video-3", "caviar-video-4",
      "caviar-video-2-meeting-moving", "caviar-video-5", "caviar-video-6",
      "caviar-video-13-meeting", "caviar-video-7", "caviar-video-8",
      "caviar-video-14-meeting-moving", "caviar-video-9", "caviar-video-10",
      "caviar-video-19-meeting-moving", "caviar-video-27", "caviar-video-12-moving",
      "caviar-video-20-meeting-moving", "caviar-video-15", "caviar-video-26",
      "caviar-video-21-meeting-moving", "caviar-video-17", "caviar-video-18",
      "caviar-video-22-meeting-moving", "caviar-video-23-moving", "caviar-video-25",
      "caviar-video-28-meeting", "caviar-video-29", "caviar-video-30")
  private val test9 = Vector("caviar-video-24-meeting-moving", "caviar-video-16", "caviar-video-11")

  val meeting9: (Vector[String], Vector[String]) = (train9, test9)

  private val train10 =
    Vector("caviar-video-1-meeting-moving", "caviar-video-3", "caviar-video-4",
      "caviar-video-2-meeting-moving", "caviar-video-5", "caviar-video-6",
      "caviar-video-13-meeting", "caviar-video-7", "caviar-video-8",
      "caviar-video-14-meeting-moving", "caviar-video-9", "caviar-video-10",
      "caviar-video-19-meeting-moving", "caviar-video-11", "caviar-video-30",
      "caviar-video-20-meeting-moving", "caviar-video-29", "caviar-video-16",
      "caviar-video-21-meeting-moving", "caviar-video-17", "caviar-video-18",
      "caviar-video-22-meeting-moving", "caviar-video-23-moving", "caviar-video-25",
      "caviar-video-24-meeting-moving", "caviar-video-26", "caviar-video-27")
  private val test10 = Vector("caviar-video-28-meeting", "caviar-video-15", "caviar-video-12-moving")

  val meeting10: (Vector[String], Vector[String]) = (train10, test10)







}



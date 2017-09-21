package app.runners.maritime_experiments

/**
  * Created by nkatz on 7/9/17.
  */

object MultiCoreDataOptions {


  def getOptions(hle: String, chunkSize: Int, limit: Int, coresNum: Int) = {
    val prefixes = coresNum match {
      case 2 => List("2-1", "2-2")
      case 4 => List("4-1", "4-2", "4-3", "4-4")
      case 8 => List("8-1", "8-2", "8-3", "8-4", "8-5", "8-6", "8-7", "8-8")
    }

    val joinPrefix = (p: String) => p.split("-").mkString("")

    val llePath = "/home/nkatz/dev/maritime/brest-data/datasets-my-split"
    val hlePath = "/home/nkatz/dev/maritime/brest-data/recognition-my-split"

    val speedLimitsPath = "/home/nkatz/dev/maritime/brest-data/areas_speed_limits.csv"

    val info = prefixes map (x => (s"$llePath/dataset${joinPrefix(x)}.txt", s"brest-$x", s"$hlePath/${joinPrefix(x)}/$hle.csv", s"$hlePath/${joinPrefix(x)}/close_to_ports.csv") )

    info map { x =>
      new MaritimeDataOptions(llePath=x._1, db=x._2, hlePath=x._3, speedLimitsPath=speedLimitsPath, closeToPortsPath=x._4, chunkSize, limit.toDouble, hle)
    }

  }

  /*
  lazy val highSpeedInDataOptions21 = new MaritimeDataOptions(
    llePath = "/home/nkatz/dev/maritime/brest-data/datasets-my-split/dataset21.txt",
    db = "brest-2-1",
    hlePath = "/home/nkatz/dev/maritime/brest-data/recognition-my-split/21/highSpeedIn.csv",
    speedLimitsPath = "/home/nkatz/dev/maritime/brest-data/areas_speed_limits.csv",
    closeToPortsPath = "/home/nkatz/dev/maritime/brest-data/recognition-my-split/21/close_to_ports.csv",
    chunkSize = 10,
    limit = 5000.0,
    targetConcept = "highSpeedIn")

  lazy val highSpeedInDataOptions22 = new MaritimeDataOptions(
    llePath = "/home/nkatz/dev/maritime/brest-data/datasets-my-split/dataset22.txt",
    db = "brest-2-2",
    hlePath = "/home/nkatz/dev/maritime/brest-data/recognition-my-split/22/highSpeedIn.csv",
    speedLimitsPath = "/home/nkatz/dev/maritime/brest-data/areas_speed_limits.csv",
    closeToPortsPath = "/home/nkatz/dev/maritime/brest-data/recognition-my-split/22/close_to_ports.csv",
    chunkSize = 10,
    limit = 5000.0,
    targetConcept = "highSpeedIn")

  lazy val highSpeedInDataOptions41 = new MaritimeDataOptions(
    llePath = "/home/nkatz/dev/maritime/brest-data/datasets-my-split/dataset41.txt",
    db = "brest-4-1",
    hlePath = "/home/nkatz/dev/maritime/brest-data/recognition-my-split/41/highSpeedIn.csv",
    speedLimitsPath = "/home/nkatz/dev/maritime/brest-data/areas_speed_limits.csv",
    closeToPortsPath = "/home/nkatz/dev/maritime/brest-data/recognition-my-split/41/close_to_ports.csv",
    chunkSize = 10,
    limit = 2500.0,
    targetConcept = "highSpeedIn")

  lazy val highSpeedInDataOptions42 = new MaritimeDataOptions(
    llePath = "/home/nkatz/dev/maritime/brest-data/datasets-my-split/dataset42.txt",
    db = "brest-4-2",
    hlePath = "/home/nkatz/dev/maritime/brest-data/recognition-my-split/42/highSpeedIn.csv",
    speedLimitsPath = "/home/nkatz/dev/maritime/brest-data/areas_speed_limits.csv",
    closeToPortsPath = "/home/nkatz/dev/maritime/brest-data/recognition-my-split/42/close_to_ports.csv",
    chunkSize = 10,
    limit = 2500.0,
    targetConcept = "highSpeedIn")


  lazy val highSpeedInDataOptions43 = new MaritimeDataOptions(
    llePath = "/home/nkatz/dev/maritime/brest-data/datasets-my-split/dataset43.txt",
    db = "brest-4-3",
    hlePath = "/home/nkatz/dev/maritime/brest-data/recognition-my-split/43/highSpeedIn.csv",
    speedLimitsPath = "/home/nkatz/dev/maritime/brest-data/areas_speed_limits.csv",
    closeToPortsPath = "/home/nkatz/dev/maritime/brest-data/recognition-my-split/43/close_to_ports.csv",
    chunkSize = 10,
    limit = 2500.0,
    targetConcept = "highSpeedIn")


  lazy val highSpeedInDataOptions44 = new MaritimeDataOptions(
    llePath = "/home/nkatz/dev/maritime/brest-data/datasets-my-split/dataset44.txt",
    db = "brest-4-4",
    hlePath = "/home/nkatz/dev/maritime/brest-data/recognition-my-split/44/highSpeedIn.csv",
    speedLimitsPath = "/home/nkatz/dev/maritime/brest-data/areas_speed_limits.csv",
    closeToPortsPath = "/home/nkatz/dev/maritime/brest-data/recognition-my-split/44/close_to_ports.csv",
    chunkSize = 10,
    limit = 2500.0,
    targetConcept = "highSpeedIn")


  lazy val highSpeedInDataOptions81 = new MaritimeDataOptions(
    llePath = "/home/nkatz/dev/maritime/brest-data/datasets-my-split/dataset81.txt",
    db = "brest-8-1",
    hlePath = "/home/nkatz/dev/maritime/brest-data/recognition-my-split/81/highSpeedIn.csv",
    speedLimitsPath = "/home/nkatz/dev/maritime/brest-data/areas_speed_limits.csv",
    closeToPortsPath = "/home/nkatz/dev/maritime/brest-data/recognition-my-split/81/close_to_ports.csv",
    chunkSize = 10,
    limit = 1250.0,
    targetConcept = "highSpeedIn")

  lazy val highSpeedInDataOptions82 = new MaritimeDataOptions(
    llePath = "/home/nkatz/dev/maritime/brest-data/datasets-my-split/dataset82.txt",
    db = "brest-8-2",
    hlePath = "/home/nkatz/dev/maritime/brest-data/recognition-my-split/82/highSpeedIn.csv",
    speedLimitsPath = "/home/nkatz/dev/maritime/brest-data/areas_speed_limits.csv",
    closeToPortsPath = "/home/nkatz/dev/maritime/brest-data/recognition-my-split/82/close_to_ports.csv",
    chunkSize = 10,
    limit = 1250.0,
    targetConcept = "highSpeedIn")

  lazy val highSpeedInDataOptions83 = new MaritimeDataOptions(
    llePath = "/home/nkatz/dev/maritime/brest-data/datasets-my-split/dataset83.txt",
    db = "brest-8-3",
    hlePath = "/home/nkatz/dev/maritime/brest-data/recognition-my-split/83/highSpeedIn.csv",
    speedLimitsPath = "/home/nkatz/dev/maritime/brest-data/areas_speed_limits.csv",
    closeToPortsPath = "/home/nkatz/dev/maritime/brest-data/recognition-my-split/83/close_to_ports.csv",
    chunkSize = 10,
    limit = 1250.0,
    targetConcept = "highSpeedIn")

  lazy val highSpeedInDataOptions84 = new MaritimeDataOptions(
    llePath = "/home/nkatz/dev/maritime/brest-data/datasets-my-split/dataset84.txt",
    db = "brest-8-4",
    hlePath = "/home/nkatz/dev/maritime/brest-data/recognition-my-split/84/highSpeedIn.csv",
    speedLimitsPath = "/home/nkatz/dev/maritime/brest-data/areas_speed_limits.csv",
    closeToPortsPath = "/home/nkatz/dev/maritime/brest-data/recognition-my-split/84/close_to_ports.csv",
    chunkSize = 10,
    limit = 1250.0,
    targetConcept = "highSpeedIn")

  lazy val highSpeedInDataOptions85 = new MaritimeDataOptions(
    llePath = "/home/nkatz/dev/maritime/brest-data/datasets-my-split/dataset85.txt",
    db = "brest-8-5",
    hlePath = "/home/nkatz/dev/maritime/brest-data/recognition-my-split/85/highSpeedIn.csv",
    speedLimitsPath = "/home/nkatz/dev/maritime/brest-data/areas_speed_limits.csv",
    closeToPortsPath = "/home/nkatz/dev/maritime/brest-data/recognition-my-split/85/close_to_ports.csv",
    chunkSize = 10,
    limit = 1250.0,
    targetConcept = "highSpeedIn")


  lazy val highSpeedInDataOptions86 = new MaritimeDataOptions(
    llePath = "/home/nkatz/dev/maritime/brest-data/datasets-my-split/dataset86.txt",
    db = "brest-8-6",
    hlePath = "/home/nkatz/dev/maritime/brest-data/recognition-my-split/86/highSpeedIn.csv",
    speedLimitsPath = "/home/nkatz/dev/maritime/brest-data/areas_speed_limits.csv",
    closeToPortsPath = "/home/nkatz/dev/maritime/brest-data/recognition-my-split/86/close_to_ports.csv",
    chunkSize = 10,
    limit = 1250.0,
    targetConcept = "highSpeedIn")

  lazy val highSpeedInDataOptions87 = new MaritimeDataOptions(
    llePath = "/home/nkatz/dev/maritime/brest-data/datasets-my-split/dataset87.txt",
    db = "brest-8-7",
    hlePath = "/home/nkatz/dev/maritime/brest-data/recognition-my-split/87/highSpeedIn.csv",
    speedLimitsPath = "/home/nkatz/dev/maritime/brest-data/areas_speed_limits.csv",
    closeToPortsPath = "/home/nkatz/dev/maritime/brest-data/recognition-my-split/87/close_to_ports.csv",
    chunkSize = 10,
    limit = 1250.0,
    targetConcept = "highSpeedIn")

  lazy val highSpeedInDataOptions88 = new MaritimeDataOptions(
    llePath = "/home/nkatz/dev/maritime/brest-data/datasets-my-split/dataset88.txt",
    db = "brest-8-8",
    hlePath = "/home/nkatz/dev/maritime/brest-data/recognition-my-split/88/highSpeedIn.csv",
    speedLimitsPath = "/home/nkatz/dev/maritime/brest-data/areas_speed_limits.csv",
    closeToPortsPath = "/home/nkatz/dev/maritime/brest-data/recognition-my-split/88/close_to_ports.csv",
    chunkSize = 10,
    limit = 1250.0,
    targetConcept = "highSpeedIn")

    */


}

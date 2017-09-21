package app.runners.maritime_experiments

/**
  * Created by nkatz on 7/9/17.
  */

object SingleCoreDataOptions {

  lazy val highSpeedInDataOptionsTraining = new MaritimeDataOptions(
    llePath = "/home/nkatz/dev/maritime/brest-data/datasets/dataset1.txt",
    db = "brest-1",
    hlePath = "/home/nkatz/dev/maritime/brest-data/recognition/1/highSpeedIn.csv",
    speedLimitsPath = "/home/nkatz/dev/maritime/brest-data/areas_speed_limits.csv",
    closeToPortsPath = "/home/nkatz/dev/maritime/brest-data/recognition/1/close_to_ports.csv",
    chunkSize = 50,
    limit = 10000.0,
    targetConcept = "highSpeedIn")




  lazy val stoppedDataOptionsTraining = new MaritimeDataOptions(
    llePath = "/home/nkatz/dev/maritime/brest-data/datasets/dataset1.txt",
    hlePath = "/home/nkatz/dev/maritime/brest-data/recognition/1/stopped-no-infs.csv",
    db = "brest-1",
    speedLimitsPath = "/home/nkatz/dev/maritime/brest-data/areas_speed_limits.csv",
    closeToPortsPath = "/home/nkatz/dev/maritime/brest-data/recognition/1/close_to_ports.csv",
    chunkSize = 10,
    limit = 10000.0,
    targetConcept = "stopped")




  lazy val sailingDataOptionsTraining = new MaritimeDataOptions(
    llePath = "/home/nkatz/dev/maritime/brest-data/datasets/dataset1.txt",
    hlePath = "/home/nkatz/dev/maritime/brest-data/recognition/1/sailing-no-infs.csv",
    db = "brest-1",
    speedLimitsPath = "/home/nkatz/dev/maritime/brest-data/areas_speed_limits.csv",
    closeToPortsPath = "/home/nkatz/dev/maritime/brest-data/recognition/1/close_to_ports.csv",
    chunkSize = 100,
    limit = 100000.0,
    targetConcept = "sailing")


  lazy val lowSpeedDataOptionsTraining = new MaritimeDataOptions(
    llePath = "/home/nkatz/dev/maritime/brest-data/datasets/dataset1.txt",
    hlePath = "/home/nkatz/dev/maritime/brest-data/recognition/1/lowSpeed-no-infs.csv",
    db = "brest-1",
    speedLimitsPath = "/home/nkatz/dev/maritime/brest-data/areas_speed_limits.csv",
    closeToPortsPath = "/home/nkatz/dev/maritime/brest-data/recognition/1/close_to_ports.csv",
    chunkSize = 200,
    limit = 10000.0,
    targetConcept = "lowSpeed")



}

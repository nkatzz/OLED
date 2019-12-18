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

package app.runners.maritime_experiments

/**
  * Created by nkatz on 7/9/17.
  */

object SingleCoreDataOptions {

  lazy val highSpeedInDataOptionsTraining = new MaritimeDataOptions(
    llePath          = "/home/nkatz/dev/maritime/brest-data/datasets/dataset1.txt",
    db               = "brest-1",
    hlePath          = "/home/nkatz/dev/maritime/brest-data/recognition/1/highSpeedIn.csv",
    speedLimitsPath  = "/home/nkatz/dev/maritime/brest-data/areas_speed_limits.csv",
    closeToPortsPath = "/home/nkatz/dev/maritime/brest-data/recognition/1/close_to_ports.csv",
    chunkSize        = 50,
    limit            = 10000.0,
    targetConcept    = "highSpeedIn")

  lazy val stoppedDataOptionsTraining = new MaritimeDataOptions(
    llePath          = "/home/nkatz/dev/maritime/brest-data/datasets/dataset1.txt",
    hlePath          = "/home/nkatz/dev/maritime/brest-data/recognition/1/stopped-no-infs.csv",
    db               = "brest-1",
    speedLimitsPath  = "/home/nkatz/dev/maritime/brest-data/areas_speed_limits.csv",
    closeToPortsPath = "/home/nkatz/dev/maritime/brest-data/recognition/1/close_to_ports.csv",
    chunkSize        = 10,
    limit            = 10000.0,
    targetConcept    = "stopped")

  lazy val sailingDataOptionsTraining = new MaritimeDataOptions(
    llePath          = "/home/nkatz/dev/maritime/brest-data/datasets/dataset1.txt",
    hlePath          = "/home/nkatz/dev/maritime/brest-data/recognition/1/sailing-no-infs.csv",
    db               = "brest-1",
    speedLimitsPath  = "/home/nkatz/dev/maritime/brest-data/areas_speed_limits.csv",
    closeToPortsPath = "/home/nkatz/dev/maritime/brest-data/recognition/1/close_to_ports.csv",
    chunkSize        = 100,
    limit            = 100000.0,
    targetConcept    = "sailing")

  lazy val lowSpeedDataOptionsTraining = new MaritimeDataOptions(
    llePath          = "/home/nkatz/dev/maritime/brest-data/datasets/dataset1.txt",
    hlePath          = "/home/nkatz/dev/maritime/brest-data/recognition/1/lowSpeed-no-infs.csv",
    db               = "brest-1",
    speedLimitsPath  = "/home/nkatz/dev/maritime/brest-data/areas_speed_limits.csv",
    closeToPortsPath = "/home/nkatz/dev/maritime/brest-data/recognition/1/close_to_ports.csv",
    chunkSize        = 200,
    limit            = 10000.0,
    targetConcept    = "lowSpeed")

}

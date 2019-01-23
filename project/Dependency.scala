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

import sbt._

object Dependency {

  object v {
    //final val Akka = "2.5.17"

    final val Akka = "2.5.6"

    final val ScalaLogging = "3.9.0"
    final val Logback = "1.2.3"
    final val SLF4J = "1.7.15"

    final val MongoDB = "3.1.1"

    final val ScalaTest = "3.0.5"

    final val ScalaZ = "7.2.26"
    final val SizeOf = "0.1"
    final val Parboiled = "2.1.5"

    final val Optimus = "2.0.0"
    final val LoMRF = "0.5.5-SNAPSHOT"
  }

  // Akka.io
  lazy val Akka = "com.typesafe.akka" %% "akka-actor" % v.Akka

  // Logging using SLF4J and logback
  lazy val Logging = Seq(
    "com.typesafe.scala-logging" %% "scala-logging" % v.ScalaLogging,
    "ch.qos.logback" % "logback-classic" % v.Logback,
    "org.slf4j" % "slf4j-api" % v.SLF4J
  )

  // MongoDB (update to "org.mongodb.scala" %% "mongo-scala-driver" % "2.1.0")
  lazy val MongoDB = "org.mongodb" %% "casbah" % v.MongoDB

  // ScalaTest for UNIT testing
  lazy val ScalaTest = "org.scalatest" %% "scalatest" % v.ScalaTest % "test"

  // Tools
  lazy val Tools = Seq(
    "org.scalaz" %% "scalaz-core" % v.ScalaZ,
    "com.madhukaraphatak" %% "java-sizeof" % v.SizeOf,
    "org.parboiled" %% "parboiled" % v.Parboiled,
    "com.github.vagmcs" %% "scalatikz" % "0.3.5"
  )

  // Optimus library for linear and quadratic optimization
  lazy val Optimus = Seq(
    "com.github.vagmcs" %% "optimus" % v.Optimus,
    "com.github.vagmcs" %% "optimus-solver-lp" % v.Optimus
  )

  // LoMRF library for Markov Logic Networks
  lazy val LoMRF = "com.github.anskarl" %% "lomrf" % v.LoMRF

  //lazy val vegas = "org.vegas-viz" %% "vegas" % "0.3.12"
}

import sbt._

object Dependency {

  object v {
    final val Akka = "2.5.6"

    final val ScalaLogging = "3.5.0"
    final val Logback = "1.1.7"
    final val SLF4J = "1.7.15"

    final val MongoDB = "3.1.1"

    final val ScalaTest = "2.1.3"

    final val ScalaZ = "7.1.4"
    final val SizeOf = "0.1"
    final val Parser = "2.1.4"

    final val Optimus = "2.0.0"
    final val LoMRF = "0.5.5-SNAPSHOT"
  }

  // Akka.io
  lazy val Akka = "com.typesafe.akka" %% "akka-actor" % v.Akka

  // Logging using SLF4J and logback
  lazy val Logging = Seq(
    "com.typesafe.scala-logging" %% "scala-logging" % v.ScalaLogging,
    "ch.qos.logback" % "logback-classic" % v.Logback,
    "ch.qos.logback" % "logback-core" % v.Logback,
    "org.slf4j" % "slf4j-api" % v.SLF4J
  )

  // MongoDB
  lazy val MongoDB = "org.mongodb" %% "casbah" % v.MongoDB

  // ScalaTest for UNIT testing
  lazy val ScalaTest = "org.scalatest" %% "scalatest" % v.ScalaTest % "test"

  // Tools
  lazy val Tools = Seq(
    "org.scalaz" %% "scalaz-core" % v.ScalaZ,
    "com.madhukaraphatak" %% "java-sizeof" % v.SizeOf,
    "org.parboiled" %% "parboiled" % v.Parser
  )

  // Optimus library for linear and quadratic optimization
  lazy val Optimus = Seq(
    "com.github.vagmcs" %% "optimus" % v.Optimus,
    "com.github.vagmcs" %% "optimus-solver-lp" % v.Optimus
  )

  // LoMRF library for Markov Logic Networks
  lazy val LoMRF = "com.github.anskarl" %% "lomrf" % v.LoMRF
}

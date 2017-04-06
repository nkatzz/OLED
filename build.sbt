import sbt.Tests.Setup

//import AssemblyKeys._

//assemblySettings

jarName in assembly := "d-oled.jar"
//jarName in assembly := "oled.jar"

name := "OLED"
version := "0.1"
organization := "nkatz"
scalaVersion := "2.11.7"
publishTo := Some(Resolver.file("file", new File("/home/nkatz/Desktop/test")))

// Main class for packaging to jar
mainClass in (Compile, packageBin) := Some("app.d_OLEDRunner")
//mainClass in (Compile, packageBin) := Some("app.OLEDRunner")

// MongoDB
libraryDependencies += "org.mongodb" %% "casbah" % "2.8.1"

// Scala-lang
libraryDependencies ++= Seq(
"org.scala-lang" % "scala-library" % scalaVersion.value,
"org.scala-lang" % "scala-reflect" % scalaVersion.value
)

// Scala-modules
libraryDependencies += "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.3"

// ScalaTest
libraryDependencies += "org.scalatest" % "scalatest_2.11" % "2.1.3" % "test"

// Akka lib
resolvers += "Akka Snapshot Repository" at "http://repo.akka.io/snapshots/"
libraryDependencies += "com.typesafe.akka" %% "akka-actor" % "2.3.11"


// Logging
libraryDependencies += "com.typesafe.scala-logging" %% "scala-logging" % "3.1.0"
libraryDependencies += "ch.qos.logback" % "logback-classic" % "1.1.2"
libraryDependencies += "ch.qos.logback" % "logback-core" % "1.1.2"
libraryDependencies += "org.slf4j" % "slf4j-api" % "1.7.15"

// Scalaz
libraryDependencies += "org.scalaz" %% "scalaz-core" % "7.1.4"








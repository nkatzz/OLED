import sbt.Tests.Setup

//import AssemblyKeys._

//assemblySettings

//assemblyJarName in assembly := "d-oled.jar"
assemblyJarName in assembly := "oled.jar"
//assemblyJarName in assembly := "maritime.jar"
//assemblyJarName in assembly := "maritime.jar"

name := "OLED"
version := "0.1"
organization := "nkatz"
scalaVersion := "2.11.8"
publishTo := Some(Resolver.file("file", new File("/home/nkatz/Desktop/test")))

// Main class for packaging to jar
//mainClass in (Compile, packageBin) := Some("app.runners.d_OLEDRunner")
//mainClass in (Compile, packageBin) := Some("app.runners.OLEDRunner")
mainClass in (Compile, packageBin) := Some("data_handling.maritime_data.Tests")

// MongoDB
libraryDependencies += "org.mongodb" %% "casbah" % "3.1.1"

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
libraryDependencies += "com.typesafe.akka" %% "akka-actor" % "2.4.17"

// Logging
libraryDependencies += "com.typesafe.scala-logging" %% "scala-logging" % "3.5.0"
libraryDependencies += "ch.qos.logback" % "logback-classic" % "1.1.7"
libraryDependencies += "ch.qos.logback" % "logback-core" % "1.1.7"
libraryDependencies += "org.slf4j" % "slf4j-api" % "1.7.15"

// Scalaz
libraryDependencies += "org.scalaz" %% "scalaz-core" % "7.1.4"

// Get object's sizes
libraryDependencies += "com.madhukaraphatak" %% "java-sizeof" % "0.1"





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
scalaVersion := "2.11.12"
publishTo := Some(Resolver.file("file", new File("/home/nkatz/Desktop/test")))

// Main class for packaging to jar
//mainClass in (Compile, packageBin) := Some("app.runners.d_OLEDRunner")
//mainClass in (Compile, packageBin) := Some("app.runners.OLEDRunner")
mainClass in (Compile, packageBin) := Some("data_handling.maritime_data.Tests")
//mainClass in (Compile, packageBin) := Some("data_handling.caviar_data.ParseCAVIAR")

// MongoDB
libraryDependencies += "org.mongodb" %% "casbah" % "3.1.1"

// Scala-lang
libraryDependencies ++= Seq(
"org.scala-lang" % "scala-library" % scalaVersion.value,
"org.scala-lang" % "scala-reflect" % scalaVersion.value
)

// Scala-modules
libraryDependencies += "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.6"

dependencyOverrides ++= Set(
  "org.scala-lang" % "scala-compiler" % scalaVersion.value,
  "org.scala-lang" % "scala-library" % scalaVersion.value,
  "org.scala-lang" % "scala-reflect" % scalaVersion.value,
  "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.6",
  "org.scala-lang.modules" %% "scala-xml" % "1.0.6"
)

// ScalaTest
libraryDependencies += "org.scalatest" % "scalatest_2.11" % "2.1.3" % "test"

// Akka lib
resolvers += "Akka Snapshot Repository" at "http://repo.akka.io/snapshots/"
libraryDependencies += "com.typesafe.akka" %% "akka-actor" % "2.5.6"

// Logging
libraryDependencies += "com.typesafe.scala-logging" %% "scala-logging" % "3.5.0"
libraryDependencies += "ch.qos.logback" % "logback-classic" % "1.1.7"
libraryDependencies += "ch.qos.logback" % "logback-core" % "1.1.7"
libraryDependencies += "org.slf4j" % "slf4j-api" % "1.7.15"

// Scalaz
libraryDependencies += "org.scalaz" %% "scalaz-core" % "7.1.4"

// Get object's sizes
libraryDependencies += "com.madhukaraphatak" %% "java-sizeof" % "0.1"

//libraryDependencies += "org.mongodb.scala" %% "mongo-scala-driver" % "2.1.0"

libraryDependencies += "org.parboiled" %% "parboiled" % "2.1.4"

libraryDependencies += "com.github.anskarl" %% "lomrf" % "0.5.5-SNAPSHOT"

libraryDependencies ++= Seq(
  "com.github.vagmcs" %% "optimus" % "2.0.0",
  "com.github.vagmcs" %% "optimus-solver-lp" % "2.0.0"
)

// Avoid the "deduplicate: different file contents found in the following (logback.xml)" error.
// This error started after the merge with LoMRF.
assemblyMergeStrategy in assembly := {
  case PathList("META-INF", xs @ _*) => MergeStrategy.discard
  case x => MergeStrategy.first
}



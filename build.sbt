import sbt._
import sbt.Keys._

lazy val root = Project("OLED", file("."))
  .settings(logLevel in Test := Level.Info)
  .settings(logLevel in Compile := Level.Error)
  .settings(scalaVersion := "2.11.12")
  .settings(assemblyJarName in assembly := s"oled-${version.value}.jar")
  .settings(libraryDependencies += Dependency.Akka)
  .settings(libraryDependencies ++= Dependency.Logging)
  .settings(libraryDependencies ++= Dependency.Tools)
  .settings(libraryDependencies += Dependency.LoMRF)
  .settings(libraryDependencies ++= Dependency.Optimus)
  .settings(libraryDependencies += Dependency.ScalaTest)
  .settings(libraryDependencies += Dependency.MongoDB)
  .settings {
    // Avoid the "deduplicate: different file contents found in the following (logback.xml)" error.
    // This error started after the merge with LoMRF.
    assemblyMergeStrategy in assembly := {
      case PathList("META-INF", _ @ _*) => MergeStrategy.discard
      case _ => MergeStrategy.first
    }
  }
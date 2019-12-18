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
import sbt.Keys._
import sbt.plugins.JvmPlugin
import sbtassembly.AssemblyPlugin
import sbtassembly.AssemblyPlugin.autoImport._
import de.heikoseeberger.sbtheader.HeaderPlugin
import de.heikoseeberger.sbtheader.License._
import de.heikoseeberger.sbtheader.HeaderPlugin.autoImport._

object OLEDBuild extends AutoPlugin {

  private val logger = ConsoleLogger()

  override def requires: Plugins = JvmPlugin && AssemblyPlugin && HeaderPlugin

  // Allow the plug-in to be included automatically
  override def trigger: PluginTrigger = allRequirements

  override def projectSettings: Seq[Setting[_]] = settings

  private val javaVersion: Double = sys.props("java.specification.version").toDouble

  private lazy val settings: Seq[Setting[_]] = {
    logger.info(s"Loading settings for Java $javaVersion or higher.")
    if (javaVersion < 1.8) sys.error("Java 8 or higher is required for building Optimus.")
    else commonSettings ++ assemblySettings ++ javaSettings ++ CodeStyle.formatSettings
  }

  private val commonSettings: Seq[Setting[_]] = Seq(

    name := "OLED",

    organization := "com.github.nkatzz",

    description := "A system for online learning of event definitions.",

    headerLicense := Some(GPLv3("2016", "Nikos Katzouris")),

    scalaVersion := "2.12.9",

    autoScalaLibrary := false,

    managedScalaInstance := true,

    resolvers ++= Seq(
      Resolver.mavenLocal,
      Resolver.typesafeRepo("releases"),
      Resolver.sonatypeRepo("releases"),
      Resolver.sonatypeRepo("snapshots")
    ),

    libraryDependencies ++= Seq(
      "org.scala-lang" % "scala-library" % scalaVersion.value,
      "org.scala-lang" % "scala-reflect" % scalaVersion.value,
      "org.scala-lang.modules" %% "scala-parser-combinators" % "1.1.2"
    ),

    dependencyOverrides ++= Seq(
      "org.scala-lang" % "scala-compiler" % scalaVersion.value,
      "org.scala-lang" % "scala-library" % scalaVersion.value,
      "org.scala-lang" % "scala-reflect" % scalaVersion.value,
      "org.scala-lang.modules" %% "scala-parser-combinators" % "1.1.2",
      "org.scala-lang.modules" %% "scala-xml" % "1.2.0"
    )
  )

  private lazy val assemblySettings: Seq[Setting[_]] = Seq(

    assemblyJarName in assembly := s"${name.value.toLowerCase}-${version.value}.jar",

    /*
     * Avoid the 'deduplicate: different file contents found in the following (logback.xml)' error.
     * This error started after the merging LoMRF.
     */
    assemblyMergeStrategy in assembly := {
      case PathList("META-INF", _ @ _*) => MergeStrategy.discard
      case _ => MergeStrategy.first
    }
  )

  private lazy val javaSettings: Seq[Setting[_]] = Seq(

    javacOptions ++= Seq("-source", "1.8", "-target", "1.8", "-Xlint:unchecked", "-Xlint:deprecation"),

    javaOptions ++= Seq(
      "-XX:+DoEscapeAnalysis",
      "-XX:+UseFastAccessorMethods",
      "-XX:+OptimizeStringConcat",
      "-Dlogback.configurationFile=src/main/resources/logback.xml")
  )
}

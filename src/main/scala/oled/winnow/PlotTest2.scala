package oled.winnow

import scalatikz.graphics.pgf.Figure
import scala.io.Source
import scala.math._

object PlotTest2 extends App {

  //plotMeeting1pass("/home/nkatz/Desktop/oled-winnow-results/meeting-1-pass", "/home/nkatz/Desktop/oled-winnow-results")

  //plotMeeting2passes("/home/nkatz/Desktop/oled-winnow-results/meeting-2-passes", "/home/nkatz/Desktop/oled-winnow-results")

  //plotMeeting1passLogScale("/home/nkatz/Desktop/oled-winnow-results/meeting-1-pass-new", "/home/nkatz/Desktop/oled-winnow-results")

  plotMeeting2passLogScale("/home/nkatz/Desktop/oled-winnow-results/meeting-2-passes-new", "/home/nkatz/Desktop/oled-winnow-results")

  /*
  def plotMeeting1pass(dataPath: String, savePath: String) = {

    val data = Source.fromFile(dataPath).getLines.filter( x => !x.isEmpty && !x.startsWith("%"))//.split(",")

    val winnow = data.next().split(",").map(_.toDouble).toVector
    val _095 = data.next().split(",").map(_.toDouble).toVector
    val _09 = data.next().split(",").map(_.toDouble).toVector
    val _08 = data.next().split(",").map(_.toDouble).toVector
    val _07 = data.next().split(",").map(_.toDouble).toVector
    val _06 = data.next().split(",").map(_.toDouble).toVector
    val noConstraints = data.next().split(",").map(_.toDouble).toVector
    val handCrafted = data.next().split(",").map(_.toDouble).toVector

    Figure("meeting-1-pass").plot(winnow).plot(_095).plot(_09).plot(_08).plot(_07).plot(_06).plot(noConstraints).plot(handCrafted)
      //.plot(domain -> sin _)
      //.plot(lineStyle = DASHED)(domain -> cos _)
      .havingLegends("winnow", "score $\\geq 0.95$", "score $\\geq 0.9$", "score $\\geq 0.8$", "score $\\geq 0.7$", "score $\\geq 0.6$", "all rules", "hand-crafted")
      //.havingLegendPos(SOUTH_WEST)
      .havingXLabel("Data batches (size 50)")
      .havingYLabel("Accumulated \\ Error$")
      .havingTitle("Meeting \\ 1-pass").
      saveAsPDF(savePath)
    //.show()

  }
  */

  def log2(x: Double) = {

    val b = "stop"

    println("")
    println(x)
    println(log(x))
    //println("")

    if (log10(x) == 3.8949802909279687) {
      val stop = "stop"
    }

    try {
      if (log10(x).isInfinity) 0.0 else log10(x)/log10(2.0)
    } catch {
      case _: NoSuchElementException =>
        println(x) ; x
    }

  }

  def toLog(x: Double) = {
    try {
      val y = if (log2(x).isPosInfinity) 0.0 else log2(x)
      y
    } catch {
      case _: NoSuchElementException =>
        println(x) ; x
    }

  }

  ///*
  def plotMeeting1passLogScale(dataPath: String, savePath: String) = {

    val data = Source.fromFile(dataPath).getLines.filter( x => !x.isEmpty && !x.startsWith("%"))//.split(",")

    val winnow = data.next().split(",").map(_.toDouble).toVector.map(x => toLog(x))
    val _095 = data.next().split(",").map(_.toDouble).toVector.map(x => toLog(x))
    val _09 = data.next().split(",").map(_.toDouble).toVector.map(x => toLog(x))
    val _08 = data.next().split(",").map(_.toDouble).toVector.map(x => toLog(x))
    val _07 = data.next().split(",").map(_.toDouble).toVector.map(x => toLog(x))
    val _06 = data.next().split(",").map(_.toDouble).toVector.map(x => toLog(x))
    val noConstraints = data.next().split(",").map(_.toDouble).toVector.map(x => toLog(x))
    val handCrafted = data.next().split(",").map(_.toDouble).toVector.map(x => toLog(x))

    Figure("meeting-1-pass-log-scale-new").plot(winnow).plot(_095).plot(_09).plot(_08).plot(_07).plot(_06).plot(noConstraints).plot(handCrafted)
      //.plot(domain -> sin _)
      //.plot(lineStyle = DASHED)(domain -> cos _)
      .havingLegends("winnow", "score $\\geq 0.95$", "score $\\geq 0.9$", "score $\\geq 0.8$", "score $\\geq 0.7$", "score $\\geq 0.6$", "all rules", "hand-crafted")
      //.havingLegendPos(SOUTH_WEST)
      .havingXLabel("Data batches (size 50)")
      .havingYLabel("Accumulated \\ Error (log-scale)$")
      .havingTitle("Meeting \\ 1-pass").
      saveAsPDF(savePath)
    //.show()

  }
  //*/


  def plotResults(savePath: String,
                  name: String, trueLabels: Vector[Double], wInit: Vector[Double], wNoInit: Vector[Double],
                  wTerm: Vector[Double], wNoTerm: Vector[Double],
                  predictiInt: Vector[Double], predictiTerm: Vector[Double],
                  inert: Vector[Double], holds: Vector[Double]) = {

    Figure(name).plot(trueLabels.map(toLog(_))).
      plot(wInit.map(toLog(_))).plot(wNoInit.map(toLog(_))).
      plot(wTerm.map(toLog(_))).plot(wNoTerm.map(toLog(_))).
      plot(predictiInt.map(toLog(_))).plot(predictiTerm.map(toLog(_))).
      plot(inert.map(toLog(_))).plot(holds.map(toLog(_))).
      havingLegends("True labels","$W_I^+$","$W_I^-$","$W_T^+$","$W_T^-$","$W_I$","$W_T$","$W_{inert}$","$W_{holds}$").
      havingXLabel("Time").havingYLabel("Weights \\ (log-scale)").havingTitle("Meeting \\ 1-pass").
      saveAsPDF(savePath)//show()//.saveAsPDF(savePath)

  }

  ///*
  def plotMeeting2passLogScale(dataPath: String, savePath: String) = {

    val data = Source.fromFile(dataPath).getLines.filter( x => !x.isEmpty && !x.startsWith("%"))//.split(",")

    val winnow = data.next().split(",").map(_.toDouble).toVector.map(x => toLog(x))
    val _095 = data.next().split(",").map(_.toDouble).toVector.map(x => toLog(x))
    val _09 = data.next().split(",").map(_.toDouble).toVector.map(x => toLog(x))
    val _08 = data.next().split(",").map(_.toDouble).toVector.map(x => toLog(x))
    val _07 = data.next().split(",").map(_.toDouble).toVector.map(x => toLog(x))
    val _06 = data.next().split(",").map(_.toDouble).toVector.map(x => toLog(x))


    Figure("meeting-2-passes-log-scale-new").plot(winnow).plot(_095).plot(_09).plot(_08).plot(_07).plot(_06)
      //.plot(domain -> sin _)
      //.plot(lineStyle = DASHED)(domain -> cos _)
      .havingLegends("winnow", "score $\\geq 0.95$", "score $\\geq 0.9$", "score $\\geq 0.8$", "score $\\geq 0.7$", "score $\\geq 0.6$", "all rules", "hand-crafted")
      //.havingLegendPos(SOUTH_WEST)
      .havingXLabel("Data batches (size 50)")
      .havingYLabel("Accumulated \\ Error (log-scale)$")
      .havingTitle("Meeting \\ 1-pass").
      saveAsPDF(savePath)
    //.show()

  }


  def plotMeeting2passes(dataPath: String, savePath: String) = {

    val data = Source.fromFile(dataPath).getLines.filter( x => !x.isEmpty && !x.startsWith("%"))//.split(",")

    val winnow = data.next().split(",").map(_.toDouble).toVector
    val _095 = data.next().split(",").map(_.toDouble).toVector
    val _09 = data.next().split(",").map(_.toDouble).toVector
    val _08 = data.next().split(",").map(_.toDouble).toVector
    val _07 = data.next().split(",").map(_.toDouble).toVector
    val _06 = data.next().split(",").map(_.toDouble).toVector
    //val noConstraints = data.next().split(",").map(_.toDouble).toVector
    //val handCrafted = data.next().split(",").map(_.toDouble).toVector

    Figure("meeting-2-passes").plot(winnow).plot(_095).plot(_09).plot(_08).plot(_07).plot(_06)//.plot(noConstraints).plot(handCrafted)
      //.plot(domain -> sin _)
      //.plot(lineStyle = DASHED)(domain -> cos _)
      .havingLegends("winnow", "score $\\geq 0.95$", "score $\\geq 0.9$", "score $\\geq 0.8$", "score $\\geq 0.7$", "score $\\geq 0.6$")//, , "all rules", "hand-crafted")
      //.havingLegendPos(SOUTH_WEST)
      .havingXLabel("Data batches (size 50)")
      .havingYLabel("Accumulated \\ Error$")
      .havingTitle("Meeting \\ 2-passes").
      saveAsPDF(savePath)
    //.show()

  }







}

package oled.winnow

import scalatikz.graphics.pgf.Figure
import math._
import scalatikz.graphics.pgf.LineStyle._
import scalatikz.graphics.pgf.LegendPos._
import scala.io.Source

object PlotTest2 extends App {

  ///*
  val domain = -2 * Pi to 2 * Pi by 0.1


  val errors = Source.fromFile("/home/nkatz/Desktop/graphs-1").getLines.filter( x => !x.isEmpty && !x.startsWith("%"))//.split(",")



  val hand = errors.next().split(",").map(_.toDouble).toVector
  val _09 = errors.next().split(",").map(_.toDouble).toVector
  val _08 = errors.next().split(",").map(_.toDouble).toVector


  val _07 = errors.next().split(",").map(_.toDouble).toVector
  val _06 = errors.next().split(",").map(_.toDouble).toVector
  val _05 = errors.next().split(",").map(_.toDouble).toVector

  Figure("sin_vs_cosine").plot(hand).plot(_09).plot(_08).plot(_07).plot(_06).plot(_05)//.plot(_09New)//.plot(winnow)
    //.plot(domain -> sin _)
    //.plot(lineStyle = DASHED)(domain -> cos _)
    .havingLegends("hand", "$0.9$", "0.8", "0.7", "0.6", "0.5")//, "$0.75$", "$0.95$", "winnow") //"no constraints")
    //.havingLegendPos(SOUTH_WEST)
    .havingXLabel("$X$")
    .havingYLabel("$Y$")
    //.havingTitle("$\\sin(x)$ vs $\\cos(x)$")
    .show()
  //*/


}

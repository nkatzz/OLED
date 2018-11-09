package oled.winnow

import math._
import scalatikz.graphics.pgf.LineStyle._
import scalatikz.graphics.pgf.LegendPos._
import scalatikz.graphics.pgf.Figure

import scala.io.Source


/**
  * Created by nkatz at 2/11/2018
  */

object PlotTest extends App {

  ///*
  val domain = -2 * Pi to 2 * Pi by 0.1


  val errors = Source.fromFile("/home/nkatz/Desktop/kernel").getLines.filter( x => !x.isEmpty && !x.startsWith("%"))//.split(",")

  val _075 = errors.next().split(",").map(_.toDouble).toVector
  val _08 = errors.next().split(",").map(_.toDouble).toVector
  val _09 = errors.next().split(",").map(_.toDouble).toVector
  val _095 = errors.next().split(",").map(_.toDouble).toVector
  val hand = errors.next().split(",").map(_.toDouble).toVector
  //val noConstraints = errors.next().split(",").map(_.toDouble).toVector

  val winnow = errors.next().split(",").map(_.toDouble).toVector

  Figure("sin_vs_cosine").plot(_075).plot(_08).plot(_09).plot(_095).plot(hand).plot(winnow)
    //.plot(domain -> sin _)
    //.plot(lineStyle = DASHED)(domain -> cos _)
    .havingLegends("$0.75$", "$0.8$", "$0.9$", "$0.95$", "hand", "winnow") //"no constraints")
    //.havingLegendPos(SOUTH_WEST)
    .havingXLabel("$X$")
    .havingYLabel("$Y$")
    //.havingTitle("$\\sin(x)$ vs $\\cos(x)$")
    .show()
    //*/

}

package utils.plotting

import scalatikz.graphics.Compiler.LUALATEX
import scalatikz.graphics.pgf.Figure
import scalatikz.graphics.pgf.enums.Color.{BLACK, BLUE, GREEN, ORANGE, RED}
import scalatikz.graphics.pgf.enums.LegendPos
import scalatikz.graphics.pgf.enums.LegendPos.{NORTH_EAST, NORTH_WEST}
import scalatikz.graphics.pgf.enums.Mark.{ASTERISK, CIRCLE, PLUS, TRIANGLE, X}
import utils.plotting.TPLPExpertsPlots.makeSparse

import scala.io.Source

object Draft extends App {

  plot("/home/nkatz/Desktop/PEA-NEW-RESULTS/Batch-size-100", "/home/nkatz/Desktop/PEA-NEW-RESULTS")



  def plot(dataPath: String, savePath: String) = {

    val data = Source.fromFile(dataPath).getLines.filter(x => !x.isEmpty && !x.startsWith("%")) //.split(",")

    val OLED  = data.next().split(",").map(_.toDouble).toVector
    val OLED_Experts  = data.next().split(",").map(_.toDouble).toVector
    val OLED_MLN = data.next().split(",").map(_.toDouble).toVector
    Figure("meeting-prequential-mistakes")
      //.plot(color = BLACK, marker = X, markStrokeColor = BLACK)(makeSparse(handCrafted))
      .plot(color = BLACK)(OLED)
      .plot(color = BLUE)(OLED_Experts)
      .plot(color = RED)(OLED_MLN)

      .havingLegends("\\footnotesize \\textsf{OLED}", "\\footnotesize \\textsf{Experts}", "\\footnotesize \\textsf{WOLED}")

      /*plot(color = BLUE, marker = TRIANGLE, markStrokeColor = BLUE)(makeSparse(handCraftedExperts)).
      plot(color = GREEN ! 70 ! BLACK, marker = CIRCLE, markStrokeColor = GREEN ! 70 ! BLACK)(makeSparse(OLED)).
      plot(color = ORANGE, marker = PLUS, markStrokeColor = ORANGE)(makeSparse(OLED_MLN)).
      plot(color = RED, marker = ASTERISK, markStrokeColor = RED)(makeSparse(OLED_Experts))
      .havingLegends("\\footnotesize \\textsf{HandCrafted}", "\\footnotesize \\textsf{HandCrafted-EXP}", "\\footnotesize \\textsf{OLED}",
        "\\footnotesize \\textsf{OLED-MLN}", "\\footnotesize \\textsf{OLED-EXP}")*/
      .havingLegendPos(NORTH_EAST)
      .havingXLabel("\\footnotesize Batches (size 1)")
      .havingYLabel("\\footnotesize \\textbf{Average Loss}").
      havingTitle("\\emph{Meeting}").
      //havingTitle("\\emph{Meeting},ybar").
      //havingAxisXLabels(Seq("0","5K","10K","15K","20K","25K")).
      //show(compiler = LUALATEX)
    saveAsPDF(savePath, compiler = LUALATEX)

  }

}

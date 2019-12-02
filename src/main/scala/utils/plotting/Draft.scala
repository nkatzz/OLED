package utils.plotting

/*import scalatikz.graphics.Compiler.LUALATEX
import scalatikz.graphics.pgf.Figure
import scalatikz.graphics.pgf.enums.Color.{BLACK, BLUE, GREEN, ORANGE, RED}
import scalatikz.graphics.pgf.enums.LegendPos
import scalatikz.graphics.pgf.enums.LegendPos.{NORTH_EAST, NORTH_WEST}
import scalatikz.graphics.pgf.enums.Mark.{ASTERISK, CIRCLE, PLUS, TRIANGLE, X}
import utils.plotting.TPLPExpertsPlots.makeSparse*/

import scalatikz.pgf.enums.Color.{BLACK, BLUE, RED}
import scalatikz.pgf.plots.Figure
import scalatikz.pgf.plots.enums.LegendPos.{NORTH_EAST, NORTH_WEST}
import scalatikz.pgf.Compiler.LUA_LATEX

import scala.io.Source

object Draft extends App {

  plot("/home/nkatz/Desktop/PEA-NEW-RESULTS/test", "/home/nkatz/Desktop/PEA-NEW-RESULTS")
  //plotTime("/home/nkatz/Desktop/PEA-NEW-RESULTS/time", "/home/nkatz/Desktop/PEA-NEW-RESULTS")

  /*Figure("secondary_axis")
    .plot(lineColor = RED)((-5 to 5) -> ((x: Double) => 3 * x))
    .havingXLabel("$x$")
    .havingYLabel("$3x$")
    .secondaryAxis { x => x
      .scatter(markStrokeColor = BLUE, markFillColor = BLUE)((-5 to 5) -> ((x: Double) => x * x))
      .havingYLabel("$x^2$")
    }
    .saveAsPDF("/home/nkatz/Desktop/PEA-NEW-RESULTS")*/


  def plot(dataPath: String, savePath: String) = {

    val data = Source.fromFile(dataPath).getLines.filter(x => !x.isEmpty && !x.startsWith("%")) //.split(",")

    //val OLED  = data.next().split(",").map(_.toDouble).toVector

    val OLED_Experts = data.next().split(",").map(_.toDouble).toVector
    val OLED_MLN = data.next().split(",").map(_.toDouble).toVector

    Figure("meeting-prequential-mistakes")
      //.plot(color = BLACK, marker = X, markStrokeColor = BLACK)(makeSparse(handCrafted))
      //.plot(lineColor = BLACK)(OLED)
      .plot(lineColor = RED)(OLED_MLN)
      .plot(lineColor = BLUE)(OLED_Experts)


      .havingLegends("\\footnotesize \\textsf{OLED}", "\\footnotesize \\textsf{WOLED}", "\\footnotesize \\textsf{Experts}")

      //"\\footnotesize \\textsf{OLED}",

      /*plot(color = BLUE, marker = TRIANGLE, markStrokeColor = BLUE)(makeSparse(handCraftedExperts)).
      plot(color = GREEN ! 70 ! BLACK, marker = CIRCLE, markStrokeColor = GREEN ! 70 ! BLACK)(makeSparse(OLED)).
      plot(color = ORANGE, marker = PLUS, markStrokeColor = ORANGE)(makeSparse(OLED_MLN)).
      plot(color = RED, marker = ASTERISK, markStrokeColor = RED)(makeSparse(OLED_Experts))
      .havingLegends("\\footnotesize \\textsf{HandCrafted}", "\\footnotesize \\textsf{HandCrafted-EXP}", "\\footnotesize \\textsf{OLED}",
        "\\footnotesize \\textsf{OLED-MLN}", "\\footnotesize \\textsf{OLED-EXP}")*/
      .havingLegendPos(NORTH_WEST)
      .havingXLabel("\\footnotesize Mini-batches (size 100)")
      .havingYLabel("\\footnotesize \\textbf{Average Loss}").
      //havingTitle("\\emph{Meeting}").
      havingTitle("\\emph{Meeting},ybar").
      //havingAxisXLabels(Seq("0","5K","10K","15K","20K","25K")).
      //show(compiler = LUALATEX)
    saveAsPDF(savePath, compiler = LUA_LATEX)
    //saveAsTeX(savePath)
  }


  def plotTime(dataPath: String, savePath: String) = {

    val data = Source.fromFile(dataPath).getLines.filter(x => !x.isEmpty && !x.startsWith("%")) //.split(",")

    val OLED  = Vector(1.0, 100.0, 500.0) zip data.next().split(",").map(_.toDouble).toVector
    val OLED_MLN = Vector(1.0, 100.0, 500.0) zip data.next().split(",").map(_.toDouble).toVector
    val OLED_Experts = Vector(1.0, 100.0, 500.0) zip data.next().split(",").map(_.toDouble).toVector

    Figure("times-meeting")
      //.plot(color = BLACK, marker = X, markStrokeColor = BLACK)(makeSparse(handCrafted))
      .plot(lineColor = BLACK)(OLED)
      .plot(lineColor = RED)(OLED_MLN)
      .plot(lineColor = BLUE)(OLED_Experts)


      .havingLegends("\\footnotesize \\textsf{OLED}", "\\footnotesize \\textsf{OLED-MLN}", "\\footnotesize \\textsf{OLED-EXP}")

      //"\\footnotesize \\textsf{OLED}",

      /*plot(color = BLUE, marker = TRIANGLE, markStrokeColor = BLUE)(makeSparse(handCraftedExperts)).
      plot(color = GREEN ! 70 ! BLACK, marker = CIRCLE, markStrokeColor = GREEN ! 70 ! BLACK)(makeSparse(OLED)).
      plot(color = ORANGE, marker = PLUS, markStrokeColor = ORANGE)(makeSparse(OLED_MLN)).
      plot(color = RED, marker = ASTERISK, markStrokeColor = RED)(makeSparse(OLED_Experts))
      .havingLegends("\\footnotesize \\textsf{HandCrafted}", "\\footnotesize \\textsf{HandCrafted-EXP}", "\\footnotesize \\textsf{OLED}",
        "\\footnotesize \\textsf{OLED-MLN}", "\\footnotesize \\textsf{OLED-EXP}")*/
      .havingLegendPos(NORTH_WEST)
      .havingXLabel("\\footnotesize Mini-batche size")
      .havingYLabel("\\footnotesize Avg. CPU time per batch (sec)").
      //havingTitle("\\emph{Meeting}").
      havingTitle("\\emph{Moving}").
      havingAxisXLabels(Seq("1","100","500")).
      //havingAxisXLabels(Seq("0","5K","10K","15K","20K","25K")).
      //show(compiler = LUALATEX)
      saveAsPDF(savePath, compiler = LUA_LATEX)
      //saveAsTeX(savePath)
  }


}

package utils.plotting

import scala.io.Source
import scalatikz.graphics.pgf.Figure
import scalatikz.graphics.pgf.enums.Color.{BLACK, BLUE, GREEN, ORANGE, RED, YELLOW}
import scalatikz.graphics.pgf.enums.LegendPos
import scalatikz.graphics.pgf.enums.Mark._
import scalatikz.graphics.pgf.enums.LegendPos.{NORTH_WEST, SOUTH_EAST, SOUTH_WEST}
import scalatikz.graphics.pgf.enums.LineStyle.{DASHED, DENSELY_DOTTED, DOTTED, LOOSELY_DASHED, SOLID}
import scalatikz.graphics.pgf.enums.Mark.DOT

object TPLPExpertsPlots extends App {

  plotMovingF1Scores("/home/nkatz/Desktop/TPLP-2019-results/moving-prequential-comparison-PrequentialF1Score", "/home/nkatz/Desktop/TPLP-2019-results")
  plotMovingMistakes("/home/nkatz/Desktop/TPLP-2019-results/moving-prequential-comparison-MistakeNum", "/home/nkatz/Desktop/TPLP-2019-results")
  plotMeetingMistakes("/home/nkatz/Desktop/TPLP-2019-results/meeting-prequential-comparison-MistakeNum", "/home/nkatz/Desktop/TPLP-2019-results")
  plotMeetingF1Scores("/home/nkatz/Desktop/TPLP-2019-results/meeting-prequential-comparison-PrequentialF1Score", "/home/nkatz/Desktop/TPLP-2019-results")

  def plotMeetingMistakes(dataPath: String, savePath: String) = {
    val data = Source.fromFile(dataPath).getLines.filter( x => !x.isEmpty && !x.startsWith("%"))//.split(",")
    val handCrafted = data.next().split(",").map(_.toDouble).toVector
    val handCraftedExperts = data.next().split(",").map(_.toDouble).toVector
    val OLED = data.next().split(",").map(_.toDouble).toVector
    val OLED_MLN = data.next().split(",").map(_.toDouble).toVector
    val OLED_Experts = data.next().split(",").map(_.toDouble).toVector
    Figure("meeting-prequential-mistakes")
    .plot(color = BLACK, marker = X, markStrokeColor = BLACK)(makeSparse(handCrafted)).
      plot(color = BLUE, marker = TRIANGLE, markStrokeColor = BLUE)(makeSparse(handCraftedExperts)).
      plot(color = GREEN!70!BLACK, marker = CIRCLE, markStrokeColor = GREEN!70!BLACK)(makeSparse(OLED)).
      plot(color = ORANGE, marker = PLUS, markStrokeColor = ORANGE)(makeSparse(OLED_MLN)).
      plot(color = RED, marker = ASTERISK, markStrokeColor = RED)(makeSparse(OLED_Experts))
      .havingLegends("\\footnotesize \\textsf{HandCrafted}", "\\footnotesize \\textsf{HandCrafted-EXP}", "\\footnotesize \\textsf{OLED}",
        "\\footnotesize \\textsf{OLED-MLN}", "\\footnotesize \\textsf{OLED-EXP}")
      .havingLegendPos(NORTH_WEST)
      .havingXLabel("\\textbf{Time} $\\mathbf{(\\times 50)}$")
      .havingYLabel("\\textbf{Acummulated Mistakes}").
      havingTitle("\\emph{Meeting}").
      saveAsPDF(savePath)
    //.show()
  }

  def plotMeetingF1Scores(dataPath: String, savePath: String) = {
    val data = Source.fromFile(dataPath).getLines.filter( x => !x.isEmpty && !x.startsWith("%"))//.split(",")
    val handCrafted = data.next().split(",").map(_.toDouble).toVector
    val handCraftedExperts = data.next().split(",").map(_.toDouble).toVector
    val OLED = data.next().split(",").map(_.toDouble).toVector
    val OLED_MLN = data.next().split(",").map(_.toDouble).toVector
    val OLED_Experts = data.next().split(",").map(_.toDouble).toVector
    Figure("meeting-prequential-fscore")
      .plot(color = BLACK, marker = X, markStrokeColor = BLACK)(makeSparse(handCrafted)).
      plot(color = BLUE, marker = TRIANGLE, markStrokeColor = BLUE)(makeSparse(handCraftedExperts)).
      plot(color = GREEN!70!BLACK, marker = CIRCLE, markStrokeColor = GREEN!70!BLACK)(makeSparse(OLED)).
      plot(color = ORANGE, marker = PLUS, markStrokeColor = ORANGE)(makeSparse(OLED_MLN)).
      plot(color = RED, marker = ASTERISK, markStrokeColor = RED)(makeSparse(OLED_Experts))
      .havingLegends("\\footnotesize \\textsf{HandCrafted}", "\\footnotesize \\textsf{HandCrafted-EXP}", "\\footnotesize \\textsf{OLED}",
        "\\footnotesize \\textsf{OLED-MLN}", "\\footnotesize \\textsf{OLED-EXP}")
      .havingLegendPos(SOUTH_EAST)
      .havingXLabel("\\textbf{Time} $\\mathbf{(\\times 50)}$")
      .havingYLabel("\\textbf{Prequential $F_1$-score}").
      havingTitle("\\emph{Meeting}").
      saveAsPDF(savePath)
    //.show()
  }

  def plotMovingF1Scores(dataPath: String, savePath: String) = {
    val data = Source.fromFile(dataPath).getLines.filter( x => !x.isEmpty && !x.startsWith("%"))//.split(",")
    val handCrafted = data.next().split(",").map(_.toDouble).toVector
    val handCraftedExperts = data.next().split(",").map(_.toDouble).toVector
    val OLED = data.next().split(",").map(_.toDouble).toVector
    val OLED_MLN = data.next().split(",").map(_.toDouble).toVector
    val OLED_Experts = data.next().split(",").map(_.toDouble).toVector
    Figure("moving-prequential-fscore")
      .plot(color = BLACK, marker = X, markStrokeColor = BLACK)(makeSparse(handCrafted)).
      plot(color = BLUE, marker = TRIANGLE, markStrokeColor = BLUE)(makeSparse(handCraftedExperts)).
      plot(color = GREEN!70!BLACK, marker = CIRCLE, markStrokeColor = GREEN!70!BLACK)(makeSparse(OLED)).
      plot(color = ORANGE, marker = PLUS, markStrokeColor = ORANGE)(makeSparse(OLED_MLN)).
      plot(color = RED, marker = ASTERISK, markStrokeColor = RED)(makeSparse(OLED_Experts))
      .havingLegends("\\footnotesize \\textsf{HandCrafted}", "\\footnotesize \\textsf{HandCrafted-EXP}", "\\footnotesize \\textsf{OLED}",
      "\\footnotesize \\textsf{OLED-MLN}", "\\footnotesize \\textsf{OLED-EXP}")
      .havingLegendPos(SOUTH_EAST)
      .havingXLabel("\\textbf{Time} $\\mathbf{(\\times 50)}$")
      .havingYLabel("\\textbf{Prequential $F_1$-score}").
      havingTitle("\\emph{Moving}").
      saveAsPDF(savePath)
    //.show()
  }

  def plotMovingMistakes(dataPath: String, savePath: String) = {
    val data = Source.fromFile(dataPath).getLines.filter( x => !x.isEmpty && !x.startsWith("%"))//.split(",")
    val handCrafted = data.next().split(",").map(_.toDouble).toVector
    val handCraftedExperts = data.next().split(",").map(_.toDouble).toVector
    val OLED = data.next().split(",").map(_.toDouble).toVector
    val OLED_MLN = data.next().split(",").map(_.toDouble).toVector
    val OLED_Experts = data.next().split(",").map(_.toDouble).toVector
    Figure("moving-prequential-mistakes")
    .plot(color = BLACK, marker = X, markStrokeColor = BLACK)(makeSparse(handCrafted)).
      plot(color = BLUE, marker = TRIANGLE, markStrokeColor = BLUE)(makeSparse(handCraftedExperts)).
      plot(color = GREEN!70!BLACK, marker = CIRCLE, markStrokeColor = GREEN!70!BLACK)(makeSparse(OLED)).
      plot(color = ORANGE, marker = PLUS, markStrokeColor = ORANGE)(makeSparse(OLED_MLN)).
      plot(color = RED, marker = ASTERISK, markStrokeColor = RED)(makeSparse(OLED_Experts))
      .havingLegends("\\footnotesize \\textsf{HandCrafted}", "\\footnotesize \\textsf{HandCrafted-EXP}", "\\footnotesize \\textsf{OLED}",
        "\\footnotesize \\textsf{OLED-MLN}", "\\footnotesize \\textsf{OLED-EXP}")
      .havingLegendPos(NORTH_WEST)
      .havingXLabel("\\textbf{Time} $\\mathbf{(\\times 50)}$")
      .havingYLabel("\\textbf{Acummulated Mistakes}").
      havingTitle("\\emph{Moving}").
      saveAsPDF(savePath)
    //.show()
  }

  def makeSparse(input: Vector[Double]): Vector[(Double, Double)] = {
    val l = input.length
    input.zipWithIndex.foldLeft(Vector.empty[(Double, Double)]) {
      case (output, (x, i)) =>
        if (output.isEmpty) output :+ (i.toDouble, x)
        else if (i == l - 1) output :+ (i.toDouble, x)
        else if (output.last._2 != x) output :+ (i.toDouble, x)
        else output
    }
  }

}

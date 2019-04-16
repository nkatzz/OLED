package utils.plotting

import scala.io.Source
import scalatikz.graphics.pgf.Figure
import scalatikz.graphics.pgf.enums.LegendPos
import scalatikz.graphics.pgf.enums.LegendPos.{NORTH_WEST, SOUTH_EAST, SOUTH_WEST}

object TPLPExpertsPlots extends App {

  //plotMovingF1Scores("/home/nkatz/Desktop/TPLP-2019-results/moving-prequential-comparison-PrequentialF1Score", "/home/nkatz/Desktop/TPLP-2019-results")
  //plotMovingMistakes("/home/nkatz/Desktop/TPLP-2019-results/moving-prequential-comparison-MistakeNum", "/home/nkatz/Desktop/TPLP-2019-results")
  plotMeetingMistakes("/home/nkatz/Desktop/TPLP-2019-results/meeting-prequential-comparison-MistakeNum", "/home/nkatz/Desktop/TPLP-2019-results")

  def plotMeetingMistakes(dataPath: String, savePath: String) = {
    val data = Source.fromFile(dataPath).getLines.filter( x => !x.isEmpty && !x.startsWith("%"))//.split(",")
    val handCrafted = data.next().split(",").map(_.toDouble).toVector
    val handCraftedExperts = data.next().split(",").map(_.toDouble).toVector
    val OLED_MLN = data.next().split(",").map(_.toDouble).toVector
    val OLED = data.next().split(",").map(_.toDouble).toVector
    val OLED_Experts = data.next().split(",").map(_.toDouble).toVector
    Figure("meeting-prequential-mistakes").plot(handCrafted).plot(handCraftedExperts).plot(OLED_MLN).plot(OLED).plot(OLED_Experts)
      .havingLegends("HandCrafted", "HandCrafted-SE", "OLED-MLN",
        "OLED", "OLED-SE")
      .havingLegendPos(NORTH_WEST)
      .havingXLabel("Time ($\\times 50$)")
      .havingYLabel("Acummulated Mistakes").
      havingTitle("Meeting").
      saveAsPDF(savePath)
    //.show()
  }

  def plotMovingF1Scores(dataPath: String, savePath: String) = {
    val data = Source.fromFile(dataPath).getLines.filter( x => !x.isEmpty && !x.startsWith("%"))//.split(",")
    val handCrafted = data.next().split(",").map(_.toDouble).toVector
    val handCraftedExperts = data.next().split(",").map(_.toDouble).toVector
    val OLED_MLN = data.next().split(",").map(_.toDouble).toVector
    val OLED = data.next().split(",").map(_.toDouble).toVector
    val OLED_Experts = data.next().split(",").map(_.toDouble).toVector
    Figure("moving-prequential-fscore").plot(handCrafted).plot(handCraftedExperts).plot(OLED_MLN).plot(OLED).plot(OLED_Experts)
      .havingLegends("HandCrafted", "HandCrafted-SE", "OLED-MLN",
      "OLED", "OLED-SE")
      .havingLegendPos(SOUTH_EAST)
      .havingXLabel("Time ($\\times 50$)")
      .havingYLabel("Prequential $F_1$-score").
      havingTitle("Moving").
      saveAsPDF(savePath)
    //.show()
  }

  def plotMovingMistakes(dataPath: String, savePath: String) = {
    val data = Source.fromFile(dataPath).getLines.filter( x => !x.isEmpty && !x.startsWith("%"))//.split(",")
    val handCrafted = data.next().split(",").map(_.toDouble).toVector
    val handCraftedExperts = data.next().split(",").map(_.toDouble).toVector
    val OLED_MLN = data.next().split(",").map(_.toDouble).toVector
    val OLED = data.next().split(",").map(_.toDouble).toVector
    val OLED_Experts = data.next().split(",").map(_.toDouble).toVector
    Figure("moving-prequential-mistakes").plot(handCrafted).plot(handCraftedExperts).plot(OLED_MLN).plot(OLED).plot(OLED_Experts)
      .havingLegends("HandCrafted", "HandCrafted-SE", "OLED-MLN",
        "OLED", "OLED-SE")
      .havingLegendPos(NORTH_WEST)
      .havingXLabel("Time ($\\times 50$)")
      .havingYLabel("Acummulated Mistakes").
      havingTitle("Moving").
      saveAsPDF(savePath)
    //.show()
  }

}

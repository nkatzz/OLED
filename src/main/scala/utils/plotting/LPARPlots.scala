package utils.plotting

import scalatikz.pgf.enums.Color.{BLACK, BLUE, RED}
import scalatikz.pgf.plots.Figure
import scalatikz.pgf.plots.enums.FontSize.VERY_LARGE
import scalatikz.pgf.plots.enums.LegendPos.{NORTH_EAST, NORTH_WEST}
import scalatikz.pgf.plots.enums.Mark.{TRIANGLE, X}

/**
 * Created by nkatz at 27/2/20
 */
object LPARPlots extends App {

  //plotCrossValBothCEs("/home/nkatz/Dropbox/PapersAll/LPAR-2020/woled-asp")
  //plotTrainingTimes("/home/nkatz/Dropbox/PapersAll/LPAR-2020/woled-asp")
  //plotTheorySizes("/home/nkatz/Dropbox/PapersAll/LPAR-2020/woled-asp")


  //plotMapInferenceMeet("/home/nkatz/Dropbox/PapersAll/KR-2020/KR20_authors_kit_v1.2/paper")
  //plotMapInferenceMove("/home/nkatz/Dropbox/PapersAll/KR-2020/KR20_authors_kit_v1.2/paper")
  //plotMapInferenceRendezVous("/home/nkatz/Dropbox/PapersAll/KR-2020/KR20_authors_kit_v1.2/paper")
  //plotMapInferenceDangerousDriving("/home/nkatz/Dropbox/PapersAll/KR-2020/KR20_authors_kit_v1.2/paper")

  //plotMapInferenceDangerousDriving("/home/nkatz/Dropbox/PapersAll/ACCEPTED/KR-2020/scalatikz-graphs")
  //plotMapInferenceNonEconomicDriving("/home/nkatz/Dropbox/PapersAll/ACCEPTED/KR-2020/scalatikz-graphs")
  //plotMapInferenceRefuelOpportunity("/home/nkatz/Dropbox/PapersAll/ACCEPTED/KR-2020/scalatikz-graphs")

  //plotMapInferenceRendezVous("/home/nkatz/Dropbox/PapersAll/ACCEPTED/KR-2020/scalatikz-graphs")
  //plotMapInferencePilotOps("/home/nkatz/Dropbox/PapersAll/ACCEPTED/KR-2020/scalatikz-graphs")

  plotCrossValRendevousPilotOps("/home/nkatz/Dropbox/PapersAll/ACCEPTED/KR-2020/scalatikz-graphs")

  def plotMapInferenceDangerousDriving(savePath: String) = {
    val groundingSolvingASP = Vector(0.041, 0.072, 0.098, 0.568)
    val groundingSolvingMLN = Vector(0.043, 0.183, 2.324, 10.234)
    Figure("map-scalability-dangerous-driving")
      .plot(lineColor       = RED, marker = X, markStrokeColor = RED)(groundingSolvingASP).
      plot(lineColor       = BLUE, marker = TRIANGLE, markStrokeColor = BLUE)(groundingSolvingMLN)
      .havingLegends("\\large{WOLED-ASP}", "\\large{WOLED-MLN}")
      .havingLegendPos(NORTH_WEST)
      .havingXLabel("\\Large{Avg. #atoms in ground program}")
      .havingYLabel("\\Large{Grounding + solving (sec)}")
      .havingTitle("\\emph{\\large{Dangerous Driving}}")
      //havingTitle("\\emph{Meeting},ybar").
      .havingAxisXLabels(Seq("1.8K", "2.9K", "12K", "16K")).
      havingFontSize(VERY_LARGE).
      saveAsPDF(savePath)
    //.show()
  }

  def plotMapInferenceNonEconomicDriving(savePath: String) = {
    val groundingSolvingASP = Vector(0.024, 0.032, 0.076, 0.468)
    val groundingSolvingMLN = Vector(0.086, 0.283, 3.675, 12.922)
    Figure("map-scalability-non-economic-driving")
      .plot(lineColor       = RED, marker = X, markStrokeColor = RED)(groundingSolvingASP).
      plot(lineColor       = BLUE, marker = TRIANGLE, markStrokeColor = BLUE)(groundingSolvingMLN)
      .havingLegends("\\large{WOLED-ASP}", "\\large{WOLED-MLN}")
      .havingLegendPos(NORTH_WEST)
      .havingXLabel("\\Large{Avg. #atoms in ground program}")
      .havingYLabel("\\Large{Grounding + solving (sec)}")
      .havingTitle("\\emph{\\large{Non-Economic Driving}}")
      //havingTitle("\\emph{Meeting},ybar").
      .havingAxisXLabels(Seq("2K", "3.3K", "14K", "17K")).
      havingFontSize(VERY_LARGE).
      saveAsPDF(savePath)
    //.show()
  }

  def plotMapInferenceRefuelOpportunity(savePath: String) = {
    val groundingSolvingASP = Vector(0.022, 0.031, 0.06, 0.426)
    val groundingSolvingMLN = Vector(0.112, 0.344, 4.234, 11.423)
    Figure("map-scalability-refuel")
      .plot(lineColor       = RED, marker = X, markStrokeColor = RED)(groundingSolvingASP).
      plot(lineColor       = BLUE, marker = TRIANGLE, markStrokeColor = BLUE)(groundingSolvingMLN)
      .havingLegends("\\large{WOLED-ASP}", "\\large{WOLED-MLN}")
      .havingLegendPos(NORTH_WEST)
      .havingXLabel("\\Large{Avg. #atoms in ground program}")
      .havingYLabel("\\Large{Grounding + solving (sec)}")
      .havingTitle("\\emph{\\large{Re-Fuel Opportunity}}")
      //havingTitle("\\emph{Meeting},ybar").
      .havingAxisXLabels(Seq("2K", "3K", "13K", "16.3K")).
      havingFontSize(VERY_LARGE).
      saveAsPDF(savePath)
    //.show()
  }


  def plotMapInferenceRendezVous(savePath: String) = {
    val groundingSolvingASP = Vector(0.041, 0.072, 0.289, 0.734)
    val groundingSolvingMLN = Vector(0.043, 0.183, 3.824, 19.234)
    Figure("map-scalability-rendezvous")
      .plot(lineColor       = RED, marker = X, markStrokeColor = RED)(groundingSolvingASP).
      plot(lineColor       = BLUE, marker = TRIANGLE, markStrokeColor = BLUE)(groundingSolvingMLN)
      .havingLegends("\\large{WOLED-ASP}", "\\large{WOLED-MLN}")
      .havingLegendPos(NORTH_WEST)
      .havingXLabel("\\Large{Avg. #atoms in ground program}")
      .havingYLabel("\\Large{Grounding + solving (sec)}")
      .havingTitle("\\emph{\\large{Vessel RendezVous}}")
      //havingTitle("\\emph{Meeting},ybar").
      .havingAxisXLabels(Seq("3K", "5K", "20K", "30K")).
      havingFontSize(VERY_LARGE).
      saveAsPDF(savePath)
    //.show()
  }

  def plotMapInferencePilotOps(savePath: String) = {
    val groundingSolvingASP = Vector(0.045, 0.068, 0.178, 0.456)
    val groundingSolvingMLN = Vector(0.042, 0.243, 4.624, 22.228)
    Figure("map-scalability-pilotops")
      .plot(lineColor       = RED, marker = X, markStrokeColor = RED)(groundingSolvingASP).
      plot(lineColor       = BLUE, marker = TRIANGLE, markStrokeColor = BLUE)(groundingSolvingMLN)
      .havingLegends("\\large{WOLED-ASP}", "\\large{WOLED-MLN}")
      .havingLegendPos(NORTH_WEST)
      .havingXLabel("\\Large{Avg. #atoms in ground program}")
      .havingYLabel("\\Large{Grounding + solving (sec)}")
      .havingTitle("\\emph{\\large{Pilot Ops}}")
      //havingTitle("\\emph{Meeting},ybar").
      .havingAxisXLabels(Seq("3K", "5K", "20K", "30K")).
      havingFontSize(VERY_LARGE).
      saveAsPDF(savePath)
    //.show()
  }

  def plotCrossValRendevousPilotOps(savePath: String) = {
    val fscoresRendezVous = Vector(0.837, 0.724) //, 0.782
    val fscoresPilotOps = Vector (0.848, 0.725) //, 0.704
    Figure("cross-val").
      bar(barColor = BLUE ! 50 ! BLACK, barWidth = 0.1)(fscoresRendezVous). //
      bar(barColor = RED ! 50 ! BLACK, barWidth = 0.1)(fscoresPilotOps). //, barWidth = 0.2
      havingYLabel("\\textbf{$F_1$-score (test set)}").havingYLimits(0.5, 1.0).
      havingAxisXLabels(Seq(
        "\\textsf{WOLED-ASP}",
        "\\textsf{WOLED-MLN}",
        //"\\textsf{\\scriptsize OLED}"
      ))//.rotateXTicks(20)
      .havingTitle("\\emph{},ybar")
      .havingLegends("\\emph{RendezVous}", "\\emph{PilotOps}")
      .havingLegendPos(NORTH_EAST)
      .saveAsPDF(savePath)
  }


  def plotMapInferenceMove(savePath: String) = {
    val groundingSolvingASP = Vector(0.032, 0.068, 0.187, 0.634)
    val groundingSolvingMLN = Vector(0.029, 0.073, 2.023, 11.025)
    Figure("map-scalability-move")
      .plot(lineColor       = RED, marker = X, markStrokeColor = RED)(groundingSolvingASP).
      plot(lineColor       = BLUE, marker = TRIANGLE, markStrokeColor = BLUE)(groundingSolvingMLN)
      .havingLegends("\\large{WOLED-ASP}", "\\large{WOLED-MLN}")
      .havingLegendPos(NORTH_WEST)
      .havingXLabel("\\Large{Avg. #atoms in ground program}")
      .havingYLabel("\\Large{Grounding + solving (sec)}")
      //.havingTitle("\\emph{\\large{Moving}}")
      //havingTitle("\\emph{Meeting},ybar").
      .havingAxisXLabels(Seq("1.3K", "2.6K", "10K", "19.2K")).
      havingFontSize(VERY_LARGE).
      saveAsPDF(savePath)
    //.show()
  }

  def plotMapInferenceMeet(savePath: String) = {
    val groundingSolvingASP = Vector(0.029, 0.043, 0.147, 0.257)
    val groundingSolvingMLN = Vector(0.028, 0.062, 1.65, 9.26)
    Figure("map-scalability-meet")
      .plot(lineColor = RED, marker = X, markStrokeColor = RED)(groundingSolvingASP)
      .plot(lineColor = BLUE, marker = TRIANGLE, markStrokeColor = BLUE)(groundingSolvingMLN)
      .havingLegends("\\large{WOLED-ASP}", "\\large{WOLED-MLN}")
      .havingLegendPos(NORTH_WEST)
      .havingXLabel("\\Large{Avg. #atoms in ground program}")
      .havingYLabel("\\Large{Grounding + solving (sec)}")
      //.havingTitle("\\emph{\\large{Meeting}}")
      //havingTitle("\\emph{Meeting},ybar").
      .havingAxisXLabels(Seq("1.1K", "2.2K", "9K", "15K"))
      .havingFontSize(VERY_LARGE)
      .saveAsPDF(savePath)
    //.show()
  }


  def plotCrossValBothCEs(savePath: String) = {
    val fscoresMeeting = Vector(0.887, 0.841, 0.782, 0.801, 0.735, 0.762)
    val fscoresMoving = Vector (0.856, 0.802, 0.704, 0.688, 0.624, 0.644)
    Figure("cross-val-both-CEs").bar(barColor = BLUE ! 50 ! BLACK, barWidth = 0.2)(fscoresMoving).
      bar(barColor = RED ! 50 ! BLACK, barWidth = 0.2)(fscoresMeeting)
      .havingYLabel("\\textbf{Average $F_1$-score (test set)}").havingYLimits(0.5, 1.0).
      havingAxisXLabels(Seq(
        "\\textsf{\\scriptsize WOLED-ASP}",
        "\\textsf{\\scriptsize WOLED-MLN}",
        "\\textsf{\\scriptsize OLED}",
        "\\textsf{\\scriptsize XHAIL}",
        "\\textsf{\\scriptsize HandCrafted}",
        "\\textsf{\\scriptsize HandCrafted-W}"
      )).rotateXTicks(20)
      .havingTitle("\\emph{},ybar").havingLegends("\\emph{Moving}", "\\emph{Meeting}").havingLegendPos(NORTH_EAST)
      .saveAsPDF(savePath)
  }

  def plotTrainingTimes(savePath: String) = {
    val fscoresMeeting = Vector(1.14, 4.823, 0.918, 0.812) //26.234
    val fscoresMoving = Vector (1.223, 4.989, 1.014, 0.842) //38.645
    Figure("cross-val-training-times").bar(barColor = BLUE ! 50 ! BLACK, barWidth = 0.2)(fscoresMoving).
      bar(barColor = RED ! 50 ! BLACK, barWidth = 0.2)(fscoresMeeting)
      .havingYLabel("\\textbf{Average Training times (min)}").//.havingYLimits(0.5, 1.0).
      havingAxisXLabels(Seq(
        "\\textsf{\\scriptsize WOLED-ASP}",
        "\\textsf{\\scriptsize WOLED-MLN}",
        "\\textsf{\\scriptsize OLED}",
        "\\textsf{\\scriptsize HandCrafted-W}"
      )).rotateXTicks(20)
      .havingTitle("\\emph{},ybar").havingLegends("\\emph{Moving}", "\\emph{Meeting}").havingLegendPos(NORTH_EAST)
      .saveAsPDF(savePath)
  }

  def plotTheorySizes(savePath: String) = {
    val fscoresMeeting = Vector(52, 62, 50, 25) //26.234
    val fscoresMoving = Vector (58, 69, 53, 22) //38.645
    Figure("cross-val-theory-size").bar(barColor = BLUE ! 50 ! BLACK, barWidth = 0.2)(fscoresMoving).
      bar(barColor = RED ! 50 ! BLACK, barWidth = 0.2)(fscoresMeeting)
      .havingYLabel("\\textbf{Average Theory Size}").//.havingYLimits(0.5, 1.0).
      havingAxisXLabels(Seq(
        "\\textsf{\\scriptsize WOLED-ASP}",
        "\\textsf{\\scriptsize WOLED-MLN}",
        "\\textsf{\\scriptsize OLED}",
        "\\textsf{\\scriptsize XHAIL}"
      )).rotateXTicks(20)
      .havingTitle("\\emph{},ybar").havingLegends("\\emph{Moving}", "\\emph{Meeting}").havingLegendPos(NORTH_EAST)
      .saveAsPDF(savePath)
  }

}

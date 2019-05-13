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

  plotMeetingMistakesInertiaNoInertia("/home/nkatz/Desktop/TPLP-2019-results/meeting-inertia-experiments-mistakes", "/home/nkatz/Desktop/TPLP-2019-results")
  plotMovingMistakesInertiaNoInertia("/home/nkatz/Desktop/TPLP-2019-results/moving-inertia-experiments-mistakes", "/home/nkatz/Desktop/TPLP-2019-results")

  //plotMovingF1Scores("/home/nkatz/Desktop/TPLP-2019-results/moving-prequential-comparison-PrequentialF1Score", "/home/nkatz/Desktop/TPLP-2019-results")
  //plotMovingMistakes("/home/nkatz/Desktop/TPLP-2019-results/moving-prequential-comparison-MistakeNum", "/home/nkatz/Desktop/TPLP-2019-results")
  //plotMeetingMistakes("/home/nkatz/Desktop/TPLP-2019-results/meeting-prequential-comparison-MistakeNum", "/home/nkatz/Desktop/TPLP-2019-results")
  //plotMeetingF1Scores("/home/nkatz/Desktop/TPLP-2019-results/meeting-prequential-comparison-PrequentialF1Score", "/home/nkatz/Desktop/TPLP-2019-results")

  //plotLimitedFeedbackMeeting("/home/nkatz/Desktop/TPLP-2019-results")
  //plotLimitedFeedbackMoving("/home/nkatz/Desktop/TPLP-2019-results")

  //plotLimitedFeedbackMovingBarChart("/home/nkatz/Desktop/TPLP-2019-results")
  //plotLimitedFeedbackMeetingBarChart("/home/nkatz/Desktop/TPLP-2019-results")

  //plotPrequentialTimeMeeting("/home/nkatz/Desktop/TPLP-2019-results")
  //plotPrequentialTimeMoving("/home/nkatz/Desktop/TPLP-2019-results")

  //plotRulesNumMeeting("/home/nkatz/Desktop/TPLP-2019-results")
  //plotRulesNumMoving("/home/nkatz/Desktop/TPLP-2019-results")

  //plotMeetingRulesNum("/home/nkatz/Desktop/TPLP-2019-results/meeting-rules-number", "/home/nkatz/Desktop/TPLP-2019-results")

  //plotMovingCrossVal("/home/nkatz/Desktop/TPLP-2019-results")
  //plotMeetingCrossVal("/home/nkatz/Desktop/TPLP-2019-results")

  def plotMeetingCrossVal(savePath: String) = {
    val fscores = Vector(0.762, 0.863, 0.861, 0.822, 0.843, 0.889, 0.906)
    Figure("meeting-cross-val")
      //.stem(color = BLUE!50!BLACK, marker = CIRCLE)(fscores)
      .bar(color = BLUE!50!BLACK, barWidth = 0.2)(fscores)
      .havingYLabel("\\textbf{Average $F_1$-score (test set)}").havingYLimits(0.5, 1.0).
      havingAxisXLabels(Seq("\\textsf{\\scriptsize HC}",
        "\\textsf{\\scriptsize HC-MM}",
        "\\textsf{\\scriptsize XHAIL}",
        "\\textsf{\\scriptsize HC-EXP}",
        "\\textsf{\\scriptsize OLED}",
        "\\textsf{\\scriptsize OLED-MLN}",
        "\\textsf{\\scriptsize OLED-EXP}")).rotateXTicks(20)
      .havingTitle("\\emph{Meeting}")
      .saveAsPDF(savePath)
  }

  def plotMovingCrossVal(savePath: String) = {
    val fscores = Vector(0.751, 0.890, 0.841, 0.802, 0.789, 0.857, 0.847)
    Figure("moving-cross-val")
      //.stem(color = BLUE!50!BLACK, marker = CIRCLE)(fscores)
    .bar(color = BLUE!50!BLACK, barWidth = 0.2)(fscores)
      .havingYLabel("\\textbf{Average $F_1$-score (test set)}").havingYLimits(0.5, 1.0).
      havingAxisXLabels(Seq("\\textsf{\\scriptsize HC}",
        "\\textsf{\\scriptsize HC-MM}",
        "\\textsf{\\scriptsize XHAIL}",
        "\\textsf{\\scriptsize HC-EXP}",
        "\\textsf{\\scriptsize OLED}",
        "\\textsf{\\scriptsize OLED-MLN}",
        "\\textsf{\\scriptsize OLED-EXP}")).rotateXTicks(20)
      .havingTitle("\\emph{Moving}")
      .saveAsPDF(savePath)
  }

  def plotRulesNumMeeting(savePath: String) = {
    val times = Vector(88.0, 118.0, 79.0)
    val _times = Vector(9.0, 19.0, 15.0)
    Figure("meeting-prequential-rules-num").
      bar(color = BLUE!50!BLACK, barWidth = 0.2)(times).bar(color = RED!50!BLACK, barWidth = 0.2)(_times).
      havingYLabel("\\textbf{Number of Rules}").
      havingAxisXLabels(Seq("\\textsf{\\footnotesize OLED}", "\\textsf{\\footnotesize OLED-MLN}",
        "\\textsf{\\footnotesize OLED-EXP}")).
      havingTitle("\\emph{Meeting},ybar").havingLegends("Average", "Useful").havingLegendPos(NORTH_WEST).
      saveAsPDF(savePath)
  }

  def plotRulesNumMoving(savePath: String) = {
    val times = Vector(75.0, 92.0, 65.0)
    val _times = Vector(10.0, 18.0, 14.0)
    Figure("moving-prequential-rules-num").
      bar(color = BLUE!50!BLACK, barWidth = 0.2)(times).bar(color = RED!50!BLACK, barWidth = 0.2)(_times).
      havingYLabel("\\textbf{Number of Rules}").
      havingAxisXLabels(Seq("\\textsf{\\footnotesize OLED}", "\\textsf{\\footnotesize OLED-MLN}",
        "\\textsf{\\footnotesize OLED-EXP}")).
      havingTitle("\\emph{Moving},ybar").havingLegends("Average", "Useful").havingLegendPos(NORTH_WEST).
      saveAsPDF(savePath)
  }

  def plotPrequentialTimeMeeting(savePath: String) = {
    val times = Vector(12.0, 43.0, 118.0, 62.0)
    Figure("meeting-prequential-time").
      bar(color = BLUE!50!BLACK, barWidth = 0.3)(times).
      havingYLabel("\\textbf{Time (sec)}").
      havingAxisXLabels(Seq("\\textsf{\\footnotesize HandCrafted-EXP}",
        "\\textsf{\\footnotesize OLED}", "\\textsf{\\footnotesize OLED-MLN}",
        "\\textsf{\\footnotesize OLED-EXP}")).
      havingTitle("\\emph{Meeting}").
      saveAsPDF(savePath)
  }

  def plotPrequentialTimeMoving(savePath: String) = {
    val times = Vector(14.0, 48.0, 104.0, 58.0)
    Figure("moving-prequential-time").
      bar(color = BLUE!50!BLACK, barWidth = 0.3)(times).
      havingYLabel("\\textbf{Time (sec)}").
      havingAxisXLabels(Seq("\\textsf{\\footnotesize HandCrafted-EXP}",
        "\\textsf{\\footnotesize OLED}", "\\textsf{\\footnotesize OLED-MLN}",
        "\\textsf{\\footnotesize OLED-EXP}")).
      havingTitle("\\emph{Moving}").
      saveAsPDF(savePath)
  }

  def plotLimitedFeedbackMovingBarChart(savePath: String) = {
    val feedbackProbs = Vector(0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1.0)
    val fscores = Vector(0.834, 0.843, 0.896, 0.934, 0.948, 0.963, 0.966, 0.968, 0.968, 0.968)
    val t = feedbackProbs zip fscores
    Figure("moving-limited-feedback")
      .stem(color = BLUE!50!BLACK, marker = CIRCLE)(t)
      .havingXLabel("\\textbf{Feedback probability}").
      havingYLabel("\\textbf{Prequential $F_1$-score (final)}").havingYLimits(0.5, 1.0).
      havingAxisXLabels(Seq("0.1","0.2","0.3","0.4","0.5","0.6","0.7","0.8","0.9","1.0")).
      havingTitle("\\emph{Moving}")
      .saveAsPDF(savePath)
  }

  def plotLimitedFeedbackMeetingBarChart(savePath: String) = {
    val feedbackProbs = Vector(0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1.0)
    val fscores = Vector(0.822, 0.845, 0.883, 0.905, 0.948, 0.963, 0.966, 0.968, 0.968, 0.968)
    val t = feedbackProbs zip fscores
    Figure("meeting-limited-feedback")
      .stem(color = BLUE!50!BLACK, marker = CIRCLE)(t)
      //.bar(t)
      .havingXLabel("\\textbf{Feedback probability}").
      havingYLabel("\\textbf{Prequential $F_1$-score (final)}").havingYLimits(0.5, 1.0).
      havingAxisXLabels(Seq("0.1","0.2","0.3","0.4","0.5","0.6","0.7","0.8","0.9","1.0")).
      havingTitle("\\emph{Meeting}")
      .saveAsPDF(savePath)
  }

  def plotLimitedFeedbackMoving(savePath: String) = {
    val feedbackProbs = Vector(0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1.0)
    val fscores = Vector(0.834, 0.843, 0.896, 0.934, 0.948, 0.963, 0.966, 0.968, 0.968, 0.968)

    val t = feedbackProbs zip fscores

    Figure("moving-limited-feedback").plot(t).havingLegendPos(LegendPos.NORTH_EAST).
      //havingLegends("sync","async").
      havingXLabel("Feedback probability").
      havingYLabel("Prequential $F_1$-score (final)").havingYLimits(0.5, 1.0).
      havingAxisXLabels(Seq("0.1","0.2","0.3","0.4","0.5","0.6","0.7","0.8","0.9","1.0")).
      havingTitle("\\emph{Moving}").
      saveAsPDF(savePath)
  }

  def plotLimitedFeedbackMeeting(savePath: String) = {
    val feedbackProbs = Vector(0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1.0)
    val fscores = Vector(0.822, 0.845, 0.883, 0.905, 0.948, 0.963, 0.966, 0.968, 0.968, 0.968)

    val t = feedbackProbs zip fscores

    Figure("meeting-limited-feedback").plot(t).havingLegendPos(LegendPos.NORTH_EAST).
      //havingLegends("sync","async").
      havingXLabel("Feedback probability").
      havingYLabel("Prequential $F_1$-score (final)").havingYLimits(0.5, 1.0).
      havingAxisXLabels(Seq("0.1","0.2","0.3","0.4","0.5","0.6","0.7","0.8","0.9","1.0")).
      havingTitle("\\emph{Meeting}").saveAsPDF(savePath)
  }


  def plotMeetingMistakesInertiaNoInertia(dataPath: String, savePath: String) = {
    val data = Source.fromFile(dataPath).getLines.filter( x => !x.isEmpty && !x.startsWith("%"))//.split(",")
    val inertia = data.next().split(",").map(_.toDouble).toVector
    val noInertia = data.next().split(",").map(_.toDouble).toVector

    Figure("meeting-inertia-no-inertia-mistakes")
      .plot(color = RED, marker = X, markStrokeColor = RED)(makeSparse(inertia)).
      plot(color = BLUE, marker = TRIANGLE, markStrokeColor = BLUE)(makeSparse(noInertia))
      .havingLegends("\\footnotesize \\textsf{OLED-EXP-inertia}", "\\footnotesize \\textsf{OLED-EXP-no-inertia}")
      .havingLegendPos(NORTH_WEST)
      .havingXLabel("\\textbf{Time} $\\mathbf{(\\times 50)}$")
      .havingYLabel("\\textbf{Acummulated Mistakes}").
      havingTitle("\\emph{Meeting}").
      //havingTitle("\\emph{Meeting},ybar").
      //havingAxisXLabels(Seq("0","5K","10K","15K","20K","25K")).
      saveAsPDF(savePath)
    //.show()
  }

  def plotMovingMistakesInertiaNoInertia(dataPath: String, savePath: String) = {
    val data = Source.fromFile(dataPath).getLines.filter( x => !x.isEmpty && !x.startsWith("%"))//.split(",")
    val inertia = data.next().split(",").map(_.toDouble).toVector
    val noInertia = data.next().split(",").map(_.toDouble).toVector

    Figure("moving-inertia-no-inertia-mistakes")
      .plot(color = RED, marker = X, markStrokeColor = RED)(makeSparse(inertia)).
      plot(color = BLUE, marker = TRIANGLE, markStrokeColor = BLUE)(makeSparse(noInertia))
      .havingLegends("\\footnotesize \\textsf{OLED-EXP-inertia}", "\\footnotesize \\textsf{OLED-EXP-no-inertia}")
      .havingLegendPos(NORTH_WEST)
      .havingXLabel("\\textbf{Time} $\\mathbf{(\\times 50)}$")
      .havingYLabel("\\textbf{Acummulated Mistakes}").
      havingTitle("\\emph{Moving}").
      //havingTitle("\\emph{Meeting},ybar").
      //havingAxisXLabels(Seq("0","5K","10K","15K","20K","25K")).
      saveAsPDF(savePath)
    //.show()
  }


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
      //havingTitle("\\emph{Meeting},ybar").
      //havingAxisXLabels(Seq("0","5K","10K","15K","20K","25K")).
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
      //havingAxisXLabels(Seq("0","5K","10K","15K","20K","25K")).
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
      //havingAxisXLabels(Seq("0","5K","10K","15K","20K","25K")).
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

package oled.mwua

class RuntimeVars {

  var totalTPs = Set[String]()
  var totalFPs = Set[String]()
  var totalFNs = Set[String]()
  var totalTNs = Set[String]()

  var groundingTimes = Vector[Double]()

  def updateGrndsTimes(t: Double) = {
    groundingTimes = groundingTimes :+ t
  }

  /*
  private var totalBatchProcessingTime = 0.0
  private var totalRuleScoringTime = 0.0
  private var totalNewRuleTestTime = 0.0
  private var totalCompressRulesTime = 0.0
  private var totalExpandRulesTime = 0.0
  private var totalNewRuleGenerationTime = 0.0

  private var totalWeightsUpdateTime = 0.0

  private var totalPredictionTime = 0.0
  */

}

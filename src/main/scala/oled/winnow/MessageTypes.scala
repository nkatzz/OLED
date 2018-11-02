package oled.winnow

import logic.Examples.Example
import logic.Theory

/**
  * Created by nkatz at 1/11/2018
  */

object MessageTypes {

  class ProcessBatchMsg(val theory: Theory, val batch: Example, val targetClass: String = "")

  class FinishedBatchMsg(val theory: Theory, val ruleScoringTime: Double, val newRuleTestTime: Double,
                         val compressRulesTime: Double, val expandRulesTime: Double,
                         val newRuleGenerationTime: Double, val BatchProcessingTime: Double, val targetClass: String)



}



package app.runutils

object Debug {

  /*
  *
  * Collect total timings for certain operations
  *
  * */

  var newRulesTime: Double = 0
  var mapInferenceTime: Double = 0
  var adgradTime: Double = 0
  var groundNetworkTime: Double = 0
  var expandRulesTime: Double = 0
  var totalILPSolverTime: Double = 0

}

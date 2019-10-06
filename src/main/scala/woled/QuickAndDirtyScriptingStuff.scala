package woled

/**
 * Created by nkatz at 6/10/19
 */

object QuickAndDirtyScriptingStuff extends App {

  val constants = (0 to 9) map (x => s"id$x") toSet
  val events = Vector("inactive", "active", "walking", "abrupt", "running", "appear", "disappear")
  val fluent1= "meeting"
  val fluent2 = "meeting"

  val constPairs = {
    val products = constants.subsets(2) map { x =>
      x.toVector.permutations.map(x => x.map(_.capitalize).mkString("_")).toVector
    }
    products.toVector.flatten
  }

  // const += "event" -> "Inactive_A"
  // evidenceBuilder.functions += new FunctionMapping("Inactive_A", "inactive", Vector("A"))

  val eventConsts = for (event <- events ; constant <- constants) yield s"${event.capitalize}_${constant.capitalize}"
  val meetFluentConsts = for (fluent <- Vector(fluent1) ; pair <- constPairs) yield s"${fluent.capitalize}_${pair.capitalize}"

  val p = eventConsts.map(x => s"""const += "event" -> "$x"""").mkString("\n")

  woled.Utils.dumpToFile(p, "/home/nkatz/Desktop/kernel")

  val q = eventConsts.map { x =>
    val y = x.split("_")
    val _functionSymbol = y(0)
    val argument = y(1)
    val functionSymbol = _functionSymbol.head.toString.toLowerCase + _functionSymbol.tail.mkString("")
    s"""evidenceBuilder.functions += new FunctionMapping("$x", "$functionSymbol", Vector("$argument"))"""
  }.mkString("\n")

  woled.Utils.dumpToFile(q, "/home/nkatz/Desktop/kernel")

  //woled.Utils.dumpToFile(eventConsts ++ meetFluentConsts, "/home/nkatz/dev/BKExamples/BK-various-taks/DevTest/caviar-bk/meeting/MLN/constants")






}

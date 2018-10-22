package app.runutils

import com.typesafe.scalalogging.LazyLogging

/**
  * Created by nkatz on 6/20/17.
  */


/*
*
* To Add a new CMD
*
* */

object CMDArgs extends LazyLogging {


  val map = scala.collection.mutable.Map[String, String]()

  def getOLEDInputArgs(args: Array[String]) = {

    val split = args map { x => val z = x.replaceAll("\\s", "").split("=")  ; (z(0),z(1)) }

    def getMatchingArgumentValue(argname: String): Any = {
      val arg = arguments.find(x => x.name == argname).getOrElse(throw new RuntimeException("Argument not found."))
      val value = arg.valueType match {
        case "String" => split.find(x => x._1 == arg.name).getOrElse( ("", arg.default) )._2.toString
        case "Int" => split.find(x => x._1 == arg.name).getOrElse( ("", arg.default) )._2.toInt
        case "Double" => split.find(x => x._1 == arg.name).getOrElse( ("", arg.default) )._2.toDouble
        case "Boolean" => split.find(x => x._1 == arg.name).getOrElse( ("", arg.default) )._2.toBoolean
        case _ => throw new RuntimeException("Don't know what to do with these arguments...")
      }
      map += argname -> value.toString
      value
    }

    val evaluate_existing = getMatchingArgumentValue("--evalth")
    map += "--evalth" -> evaluate_existing.toString

    val with_jep = getMatchingArgumentValue("--wjep")
    val fromDB = getMatchingArgumentValue("--db")
    val entryPath = getMatchingArgumentValue("--inpath")

    val delta = getMatchingArgumentValue("--delta")
    val pruningThreshold = getMatchingArgumentValue("--prune")
    val minSeenExmpls = getMatchingArgumentValue("--minseen")
    val specializationDepth = getMatchingArgumentValue("--spdepth")
    val breakTiesThreshold = getMatchingArgumentValue("--ties")
    val repeatFor = getMatchingArgumentValue("--repfor")
    val chunkSize = getMatchingArgumentValue("--chunksize")
    val onlinePruning = getMatchingArgumentValue("--onlineprune")
    val withPostPruning = getMatchingArgumentValue("--postprune")
    val tryMoreRules = getMatchingArgumentValue("--try-more-rules")
    val targetConcept = getMatchingArgumentValue("--target")
    val trainSetNum = getMatchingArgumentValue("--trainset")
    val randomOrder = getMatchingArgumentValue("--randorder")
    val scoringFun = getMatchingArgumentValue("--scorefun")
    val minEvaluatedOn = getMatchingArgumentValue("--eval-atleast-on")
    val cores = getMatchingArgumentValue("--coresnum")
    val compress_new_rules = getMatchingArgumentValue("--compress-new-rules")
    val mintps = getMatchingArgumentValue("--min-pos-covered")
    val processBatchBeforeMailBox = getMatchingArgumentValue("--mailbox-check")
    val shuffleData = getMatchingArgumentValue("--shuffle-data")
    val showRefs = getMatchingArgumentValue("--showrefs")
    val pruneAfter = getMatchingArgumentValue("--prune-after")
    val mongoCol = getMatchingArgumentValue("--mongo-collection")
    val dataLimit = getMatchingArgumentValue("--data-limit")
    val tpWeight = getMatchingArgumentValue("--tps-weight")
    val fpWeight = getMatchingArgumentValue("--fps-weight")
    val fnWeight = getMatchingArgumentValue("--fns-weight")
    val withInertia = getMatchingArgumentValue("--with-inertia")
    val weightLearn = getMatchingArgumentValue("--weight-learning")
    val mlnWeightThreshold = getMatchingArgumentValue("--mln-weight-at-least")
    val parallelClauseEval = getMatchingArgumentValue("--parallel-clause-eval")
    val adagradDelta = getMatchingArgumentValue("--ada-delta")
    val adaLearnRate = getMatchingArgumentValue("--ada-learn-rate")
    val adaRegularization = getMatchingArgumentValue("--ada-regularization")
    val adaLossFunction = getMatchingArgumentValue("--ada-loss-function")
    val withEventCalculus = getMatchingArgumentValue("--with-ec")
    val showStats = getMatchingArgumentValue("--show-stats")
    val saveTheoryTo = getMatchingArgumentValue("--saveto")

    //-------------
    // Global sets:
    //-------------
    Globals.glvalues("with-jep") = with_jep.toString
    Globals.glvalues("specializationDepth") = specializationDepth.toString
    Globals.scoringFunction = scoringFun.toString
    Globals.glvalues("tp-weight") = tpWeight.toString
    Globals.glvalues("fp-weight") = fpWeight.toString
    Globals.glvalues("fn-weight") = fnWeight.toString
    Globals.glvalues("with-inertia") = withInertia.toString
    Globals.glvalues("weight-learning") = weightLearn.toString
    Globals.glvalues("with-ec") = withEventCalculus.toString

    // Define this here so that all values in Globals.glvalues be already set.
    val globals = new Globals(entryPath.toString,fromDB.toString)

    // show the params:
    logger.info(s"\nRunning with options:\n${map.map{ case (k, v) => s"$k=$v" }.mkString(" ")}\n")

    new RunningOptions(entryPath.toString, delta.toString.toDouble, pruningThreshold.toString.toDouble,
      minSeenExmpls.toString.toInt, specializationDepth.toString.toInt, breakTiesThreshold.toString.toDouble,
      repeatFor.toString.toInt, chunkSize.toString.toInt, processBatchBeforeMailBox.toString.toInt,
      onlinePruning.toString.toBoolean, withPostPruning.toString.toBoolean, targetConcept.toString,
      compress_new_rules.toString.toBoolean, mintps.toString.toInt, tryMoreRules.toString.toBoolean,
      trainSetNum.toString.toInt, randomOrder.toString.toBoolean, scoringFun.toString, with_jep.toString.toBoolean,
      evaluate_existing.toString, fromDB.toString, globals, minEvaluatedOn.toString.toInt, cores.toString.toInt,
      shuffleData.toString.toBoolean, showRefs.toString.toBoolean, pruneAfter.toString.toInt, mongoCol.toString,
      dataLimit.toString.toInt, tpWeight.toString.toInt, fpWeight.toString.toInt, fnWeight.toString.toInt,
      withInertia.toString.toBoolean, weightLearn.toString.toBoolean, mlnWeightThreshold.toString.toDouble,
      parallelClauseEval.toString.toBoolean, adagradDelta.toString.toDouble,adaLearnRate.toString.toDouble,
      adaRegularization.toString.toDouble, adaLossFunction.toString, withEventCalculus.toString.toBoolean,
      showStats.toString.toBoolean, saveTheoryTo.toString)
  }

  val arguments = Vector(
    Arg(name = "--inpath", valueType = "String", text = "The path to the background knowledge files.", default = "Mandatory"),
    Arg(name = "--db", valueType = "String", text = "The name of a mongodb containing the training data.", default = "Mandatory"),
    Arg(name = "--delta", valueType = "Double", text = "Delta for the Hoeffding test.", default = "0.05"),
    Arg(name = "--evalth", valueType = "String", text = "If true a hand-crafted theory in a file whose path is given by this parameter is evaluated (no learning).", default = "None"),
    Arg(name = "--wjep", valueType = "Boolean", text = "If true the ASP solver is called via the java-embedded-python (jep) interface.", default = "false"),
    Arg(name = "--prune", valueType = "Double", text = "Pruning threshold. Clauses with a lower score are removed. Set to 0.0 to disable pruning.", default = "0.0"),
    Arg(name = "--minseen", valueType = "Int", text = "Minimum number of examples to evaluate on before breaking ties.", default = "1000"),
    Arg(name = "--spdepth", valueType = "Int", text = "Specialization depth. All specializations of a rule up to this length are tried simultaneously.", default = "1"),
    Arg(name = "--ties", valueType = "Double", text = "Tie-breaking threshold.", default = "0.05"),
    Arg(name = "--repfor", valueType = "Int", text = "Re-see the data this-many times. ", default = "1"),
    Arg(name = "--chunksize", valueType = "Int", text = "Mini-batch size. ", default = "1"),
    Arg(name = "--onlineprune", valueType = "Boolean", text = "If true bad rules are pruned in an online fashion.", default = "false"),
    Arg(name = "--postprune", valueType = "Boolean", text = "If true bad rules are pruned after learning terminates.", default = "true"),
    Arg(name = "--try-more-rules", valueType = "Boolean", text = "If true, a larger number of rules will be generated.", default = "false"),
    Arg(name = "--target", valueType = "String", text = "The target concept. This is used in case the training data contain more than one target concept", default = "None"),
    Arg(name = "--trainset", valueType = "Int", text = "Number of training-testing set pair (this is used in a cross-validation setting).", default = "1"),
    Arg(name = "--randorder", valueType = "Boolean", text = "If true the training data are given in random order.", default = "true"),
    Arg(name = "--scorefun", valueType = "String", text = "Specify a scoring function. Available values are 'default' (uses precision-recall), 'foilgain', 'fscore'.", default = "default"),
    Arg(name = "--eval-atleast-on", valueType = "Int", text = "Minimum number of examples on which a rule must be evaluated in order to be included in an output theory.", default = "1000"),
    Arg(name = "--coresnum", valueType = "Int", text = "Number of cores. This is used by the distributed version.", default = "1"),
    Arg(name = "--compress-new-rules", valueType = "Boolean", text = "If true new rules originating from bottom clauses that have already been generated previously are dropped.", default = "true"),
    Arg(name = "--min-pos-covered", valueType = "Int", text = "Require of a rule to cover a minimum number of positives (set to zero to ignore).", default = "0"),
    Arg(name = "--mailbox-check", valueType = "Int", text = "Number of mini batches to check before returning to idle state to check mailbox (for the distributed version).", default = "1"),
    Arg(name = "--shuffle-data", valueType = "Boolean", text = "If true the data are shuffled each time they are seen (used for order effects).", default = "false"),
    Arg(name = "--showrefs", valueType = "Boolean", text = "If true all candidate refinements are printed out during learning.", default = "false"),
    Arg(name = "--prune-after", valueType = "Int", text = "Minimum number of examples after which a bad rule may be pruned.", default = "100000"),
    Arg(name = "--mongo-collection", valueType = "String", text = "A mongo collection containing the data.", default = "examples"),
    Arg(name = "--data-limit", valueType = "Int", text = "Fetch that-many data from the db to learn from (default is max integer).", default = s"${Double.PositiveInfinity.toInt}"),
    Arg(name = "--tps-weight", valueType = "Int", text = "Weight on true positive instances.", default = "1"),
    Arg(name = "--fps-weight", valueType = "Int", text = "Weight on false positive instances.", default = "1"),
    Arg(name = "--fns-weight", valueType = "Int", text = "Weight on false negative instances.", default = "10"),
    Arg(name = "--with-inertia", valueType = "Boolean", text = "If true learns with inertia from edge interval points only.", default = "false"),
    Arg(name = "--weight-learning", valueType = "Boolean", text = "If true use AdaGrad to learn weighted clauses.", default = "false"),
    Arg(name = "--mln-weight-at-least", valueType = "Double", text = "Remove rules with mln-weight lower than this.", default = "0.1"),
    Arg(name = "--parallel-clause-eval", valueType = "Boolean", text = "Evaluate clauses in parallel during weight learning.", default = "false"),
    Arg(name = "--ada-delta", valueType = "Double", text = "Delta parameter for AdaGrad (weight learning).", default = "1.0"),
    Arg(name = "--ada-learn-rate", valueType = "Double", text = "Learning rate parameter (eta) for AdaGrad (weight learning).", default = "1.0"),
    Arg(name = "--ada-regularization", valueType = "Double", text = "Regularization parameter (lambda) for AdaGrad (weight learning).", default = "0.01"),
    Arg(name = "--ada-loss-function", valueType = "String", text = "Loss function for AdaGrad. Either 'default' (for predictive loss) or 'custom'", default = "default"),
    Arg(name = "--with-ec", valueType = "Boolean", text = "Learning using the Event Calculus in the Background knowledge.", default = "true"),
    Arg(name = "--show-stats", valueType = "Boolean", text = "If true performance stats are printed out.", default = "false"),
    Arg(name = "--saveto", valueType = "String", text = "Path to a file to wtite the learnt theory to.", default = "")
  )
//--with-ec

  def splitString(s: String, l: Int, chunks: Vector[String]): Vector[String] = {
    s.length > l match {
      case true =>
        val first = s.splitAt(l)
        splitString(first._2, l, chunks :+ first._1)
      case _ => chunks :+ s
    }
  }

  def helpMesg = {
    val msg = (x: Arg) => s"${x.name}=<${x.valueType}> | default=<${x.default}>"
    val maxLength = arguments.map(x => msg(x).length).max
    val thisLength = (x: Arg) => msg(x).length
    val message = (x: Arg) => s"  ${msg(x)} ${" " * (maxLength - thisLength(x))} : ${x.text}"
    //val message = (x: Arg) => s"  ${msg(x)} ${" " * (maxLength - thisLength(x))} : ${splitString(x.text, 30, Vector[String]())}"
    (List("\nOLED options:\n") ++ arguments.map(x => message(x))).mkString("\n")
  }

  /*Checks if mandatory arguments are in place. Returns (msg, false) if they are not else ("", true)*/
  def argsOk(args: Array[String]): (Boolean, String) = {
    if (args.isEmpty) {
      (false, "Missing options. Run with --help.")
    } else if (args.exists(x => x.contains("--help"))) {
      (false, helpMesg)
    } else if (!args.exists(x => x.contains("--inpath")) ) {
      (false, "A mandatory option is missing (e.g. path to bk/mode declarations files or the name of a database with training examples)." +
        " Re-run with --help to see options")
    } else {
      (true, "")
    }
  }


}

case class Arg(name: String, valueType: String, text: String, default: String)

class RunningOptions(val entryPath: String,
                     val delta: Double,
                     val pruneThreshold: Double,
                     val minSeenExmpls: Int,
                     val specializationDepth: Int,
                     val breakTiesThreshold: Double,
                     val repeatFor: Int,
                     val chunkSize: Int,
                     val processBatchBeforeMailBox: Int,
                     val onlinePruning: Boolean,
                     val withPostPruning: Boolean,
                     val targetHLE: String,
                     val compressNewRules: Boolean,
                     val minTpsRequired: Int,
                     val tryMoreRules: Boolean,
                     val trainSetNum: Int,
                     val randomOrder: Boolean,
                     val scoringFun: String,
                     val wjep: Boolean,
                     val evalth: String,
                     val db: String,
                     val globals: Globals,
                     val minEvalOn: Int,
                     val cores: Int,
                     val shuffleData: Boolean,
                     val showRefs: Boolean,
                     val pruneAfter: Int,
                     val mongoCollection: String,
                     val dataLimit: Int,
                     val tpWeight: Int,
                     val fpWeight: Int,
                     val fnWeight: Int,
                     val withInertia: Boolean,
                     val weightLean: Boolean,
                     val mlnWeightThreshold: Double,
                     val parallelClauseEval: Boolean,
                     val adaGradDelta: Double,
                     val adaLearnRate: Double,
                     val adaRegularization: Double,
                     val adaLossFunction: String,
                     val withEventCalculs: Boolean,
                     val showStats: Boolean,
                     val saveTheoryTo: String)



package app

/**
  * Created by nkatz on 4/6/17.
  */


object TestRunner {


  def main(args: Array[String]) = {

    val split = args map { x => val z = x.replaceAll("\\s", "").split("=")  ; (z(0),z(1)) }

    val db = split.find(x => x._1 == "db").getOrElse( ("","None") )._2
    val inputPath = split.find(x => x._1 == "inpath").getOrElse(throw new RuntimeException("No datapath provided"))._2
    val delta = split.find(x => x._1 == "delta").getOrElse( ("","0.005") )._2.toDouble
    val ties = split.find(x => x._1 == "ties").getOrElse( ("","0.05") )._2.toDouble
    val prune = split.find(x => x._1 == "prune").getOrElse( ("","0.0") )._2.toDouble // default is no pruning
    val minseen = split.find(x => x._1 == "minseen").getOrElse( ("","1000") )._2.toInt
    val repeat = split.find(x => x._1 == "repfor").getOrElse( ("","1") )._2.toInt
    val depth = split.find(x => x._1 == "spdepth").getOrElse( ("","1") )._2.toInt
    val multcounts = split.find(x => x._1 == "multcounts").getOrElse( ("","false") )._2.toBoolean
    val trainSize = split.find(x => x._1 == "trainsize").getOrElse( ("","all") )._2
    val evalOnWhole = split.find(x => x._1 == "evalonhole").getOrElse( ("","false") )._2.toBoolean
    val evalth = split.find(x => x._1 == "evalth").getOrElse( ("","false") )._2.toBoolean

  }


}

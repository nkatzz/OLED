package app

/**
  * Created by nkatz on 9/14/16.
  */

import java.util.concurrent.CountDownLatch

import akka.actor.{ActorSystem, Props}
import com.mongodb.casbah.MongoClient
import oled.MasterActor
import oled.whole_caviar_data.{MeetingTrainingData, MovingTrainingData}
import utils.DataUtils.DataFunction
import utils.Database

object OLEDRunner extends {

  /*
  *
  * For CAVIAR:
  *
  * inpath=/home/nkatz/dev/ILED/datasets/Caviar/meeting delta=0.00001 prune=0.8 db=CAVIAR_Real_FixedBorders wjep=false chunksize=100 hle=meeting repfor=1 minseen=1000
  *
  *
  * For Maritime data:
  *
  * inpath=/home/nkatz/dev/ILED/datasets/MaritimeAegean delta=0.00001 prune=0.8 db=Maritime-Aegean-whole wjep=false chunksize=1 hle=highSpeedIn repfor=1 minseen=50000
  *
  *
  * */

  def main(args: Array[String]) = {

    val split = args map { x => val z = x.replaceAll("\\s", "").split("=")  ; (z(0),z(1)) }

    // default is false
    val EVALUATE_EXISTING = split.find(x => x._1 == CMDArgsNames.EVAL_EXISTING).getOrElse( ("","false") )._2.toBoolean

    // default is false
    val WITH_JEP = split.find(x => x._1 == CMDArgsNames.WITH_JEP).getOrElse( ("","false") )._2.toBoolean
    Globals.glvalues("with-jep") = WITH_JEP.toString

    // default is None
    val fromDB = split.find(x => x._1 == CMDArgsNames.DB).getOrElse( ("","None") )._2
    //val fromDB = "Big1"

    val entryPath = split.find(x => x._1 == CMDArgsNames.INPUT_PATH).getOrElse(throw new RuntimeException("No datapath provided"))._2
    val globals = new Globals(entryPath,fromDB)

    // default is 0.005
    val delta = split.find(x => x._1 == CMDArgsNames.DELTA).getOrElse( ("","0.005") )._2.toDouble // used this for caviar: 0.00001

    // default is no pruning
    val pruningThreshold = split.find(x => x._1 == CMDArgsNames.PRUNING_THRESHOLD).getOrElse( ("","0.0") )._2.toDouble //0.7

    // default is 1000
    val minSeenExmpls = split.find(x => x._1 == CMDArgsNames.MIN_SEEN).getOrElse( ("","1000") )._2.toInt //1000

    // default is 1
    val specializationDepth = split.find(x => x._1 == CMDArgsNames.SPECIALIZATION_DEPTH).getOrElse( ("","1") )._2.toInt //1

    // default is 0.05
    val breakTiesThreshold = split.find(x => x._1 == CMDArgsNames.TIE_BREAKING_THRESHOLD).getOrElse( ("","0.05") )._2.toDouble //0.05

    // default is 1
    val repeatFor = split.find(x => x._1 == CMDArgsNames.REPEAT_FOR).getOrElse( ("","1") )._2.toInt //1

    val chunkSize = split.find(x => x._1 == CMDArgsNames.CHUNK_SIZE).getOrElse( ("","2") )._2.toInt //10

    // this is useless needs to be removed
    val trainingSetSize = 1500 // WHOLE_DATA_SET_VALE

    // default is false
    val onlinePruning = split.find(x => x._1 == CMDArgsNames.ONLINE_PRUNING).getOrElse( ("","false") )._2.toBoolean

    val withPostPruning = split.find(x => x._1 == CMDArgsNames.POST_PRUNING).getOrElse( ("","true") )._2.toBoolean

    val withInertia = false

    // This adds a new clause from each new example (if the corresponding bottom clause does not already exist).
    // Currently set a globals. It needs to be false to have the normal behaviour.
    val tryMoreRules = split.find(x => x._1 == "try-more-rules").getOrElse( ("","false") )._2.toBoolean

    // !!!!!!!!!!!!!!!!!!!!!!!!!!!
    val HLE = split.find(x => x._1 == CMDArgsNames.HLE).getOrElse( throw new RuntimeException("No target HLE provided") )._2
    //val HLE = "moving"
    // !!!!!!!!!!!!!!!!!!!!!!!!!!!-

    val trainSetNum = split.find(x => x._1 == "trainset").getOrElse( throw new RuntimeException("No training set provided") )._2.toInt //1

    val randomOrder = split.find(x => x._1 == "randorder").getOrElse( ("", "false") )._2.toBoolean

    val msg = s"δ=$delta-prune=$pruningThreshold-minseen=$minSeenExmpls-depth=$specializationDepth"

    /*This is for CAVIAR*/
    val trainingSets =
      if (HLE == "meeting")
        List(MeetingTrainingData.getMeetingTrainingData(trainSetNum, randomOrder = randomOrder))
      else
        List(MovingTrainingData.getMovingTrainingData(trainSetNum, randomOrder = randomOrder))
    //List(MovingTrainingData.wholeCAVIAR1)


    /*
    val trainingSets = List(new DataFunction(oled_distributed.Utils.getDataFromDB))
    */


    // for debugging
    //trainingSets.foreach(x => x.showTrainingIntervals())


    /* For maritime data */
    /*
    val trainingSets = {
      //val data = oled_distributed.Utils.getDataFromDB("Maritime-Aegean-whole", HLE, 1)
      //List(new DataAsExmpls(trainingSet = data, testingSet = data).asInstanceOf[TrainingSet])

      /*
      val mc = MongoClient()
      val examplesCollection = mc(fromDB)("examples")
      examplesCollection.createIndex("time" -> 1)
      */

      val data = oled_distributed.Utils.getDataFromDB("Maritime-Aegean-whole", HLE, 1)
      List(new DataAsExmplIter(data, data))
    }
    */





    Globals.glvalues("specializationDepth") = specializationDepth.toString
    // !!!!!!!!!!!!!!!!!!!!!!!!!!!
    val HAND_CRAFTED = if(HLE=="meeting"){
      //globals.HAND_CRAFTED_RULES+"/meeting-hand-crafted.lp"
      globals.HAND_CRAFTED_RULES+"/distributed-oled-test-theories.lp"
    } else {
      globals.HAND_CRAFTED_RULES+"/moving-hand-crafted.lp"
    }

    val DB = new Database(globals.fromDB, "examples")




    val system = ActorSystem("HoeffdingLearningSystem")
    val latch = new CountDownLatch(1)

    val startMsg = if (EVALUATE_EXISTING) "EvaluateHandCrafted" else "start"

    val actor =
      system.actorOf(Props(
        new MasterActor(DB,delta,breakTiesThreshold,pruningThreshold,minSeenExmpls,
          trainingSetSize,repeatFor,chunkSize,withInertia,withPostPruning,onlinePruning,trainingSets,HLE,HAND_CRAFTED, msg, globals, tryMoreRules)
      ), name = "Master-Actor") !  startMsg


  }




}

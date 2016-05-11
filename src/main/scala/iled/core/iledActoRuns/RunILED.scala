package iled.core.iledActoRuns

import akka.actor.{Props, ActorSystem}
import experiments.iledWeakStrongCleanData.RTEC_CAVIAR_Annotation
import iled.core.Core
import iled.core.noisyILED.{MovingCleanTrainingData, MeetingCleanTrainingData}
import iled.utils.{Utils, Database}
import jep.Jep
import iled.core.TrainingSetImplicits._

/**
  * Created by nkatz on 12/20/15.
  */
object RunILED {

  val allHLEs = List("moving", "meeting", "fighting", "leavingObject")
  val step = 40
  //val withBacktr = false


  def main(args: Array[String]): Unit = {
    val DB = new Database(Core.fromDB)


    //val HLE = "meeting"//args(1) //"meeting"//
    val HLE = "moving"//args(1) //"meeting"//

    val pruningPercent = 5//args(2).toInt //5//
    val batchSize = 100
    val step = 40
    val withBacktr = false
    val weaksSelectionStrategy = Right(50)//Right(args(3).toInt) //Right(50)

    // to convert an input TrainingSet (OLED format) into a DataSet instance


    val datasets = List(MovingCleanTrainingData.moveTrainingSet10)
    //val datasets = List(MeetingCleanTrainingData.meetTrainingSet2)


    val system = ActorSystem("ExperimentsRunningActor")
    val actor =
      system.actorOf(
        Props(
          // to omit weaks run set runWithWeaksAlso to false
          new Collector(DB=DB,HLE=HLE,batchSize=batchSize,withBacktr=withBacktr,weaksSelectionStrategy=weaksSelectionStrategy,
            pruningPercent=pruningPercent,step=step,datasets=datasets,runWithWeaksAlso=false,runWithStrongs=true)
        ), name = "ExperimentsrActor")
  }




  def runSequential(DB: Database, HLE: String, withWeaks: Boolean,
                    batchSize: Int, withBacktr: Boolean = false,
                    weaksSelectionStrategy: Either[String, Int] = Right(50),
                    pruningPercent: Int = 10, trainingPercent: Int = 10, step: Int, jep: Jep) = {

    val data = new DataSet(
      trainingSet =
        List((834920,835600), (732600,745040), (718200,721600), (121600,125000), (652560,655960), (491520,492160), (701120,704520), (447240,450640), (931400,934800), (508400,511800), (533760,536360), (333720,337120), (901920,905320), (295360,298760), (247200,250600), (897560,898480), (70000,73400), (6200,6800), (806880,810280), (968960,969800), (728520,730320), (426600,430000))
      ,
      testingSet =
        List((834920,835600), (732600,745040), (718200,721600), (121600,125000), (652560,655960), (491520,492160), (701120,704520), (447240,450640), (931400,934800), (508400,511800), (533760,536360), (333720,337120), (901920,905320), (295360,298760), (247200,250600), (897560,898480), (70000,73400), (6200,6800), (806880,810280), (968960,969800), (728520,730320), (426600,430000))
    )
    val system = ActorSystem("Test")
    val actor = system.actorOf(Props(
      new Runner(DB = DB, HLE = HLE, withWeaks = withWeaks,
        batchSize = batchSize, withBacktr = withBacktr,
        weaksSelectionStrategy = weaksSelectionStrategy,
        pruningPercent = pruningPercent, data = data, jep=jep, step = step)), name = "TestRunner")

    actor ! "go"

    //hle.collectKernels.foreach(x => println(x.tostring))
  }

}

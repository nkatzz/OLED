package app

import akka.actor.{Props, ActorSystem}
import com.typesafe.scalalogging.LazyLogging
import iled.globalValues.GlobalValues
import oled.{Master, OLED}

/**
  * Created by nkatz on 4/11/16.
  */


object OLEDApp extends App with LazyLogging {


  // path=/home/nkatz/dev/CAVIAR_MLN/CAVIAR_MLN/meet/fold_0 delta=0.0001 prune=0.7 minSeenExmpls=1000 repeatFor=1 hle=meeting d=2 chunkSize=10 withInert=false evalorLearn=learn fragment=mln-fragment db=CAVIAR_Real_FixedBorders

  // just an example call
  val cmd =
    """
      |path=/home/nkatz/dev/CAVIAR_MLN/CAVIAR_MLN/meet/fold_0 delta=0.0001 prune=0.7 minSeenExmpls=1000
      |    repeatFor=1 hle=meeting d=2 chunkSize=10 withInert=false evalorLearn=learn fragment=mln-fragment db=CAVIAR_Real_FixedBorders
    """.stripMargin
/*
  val PATH = "path"
  val DELTA = "delta"
  val TIES = "ties"
  val PRUNE = "prune"
  val MINSEEN = "minSeenExmpls"
  val REPREATFOR = "repeatFor"
  val HLE = "hle"
  val SPECIALIZATION_DEPTH = "d"
  val CHUNKSIZE = "chunkSize"
  val WITHINERTIA = "withInert"
  val EVALORLEARN = "evalorLearn"
  val EXPRMTYPE = "fragment"
  val DBNAME = "db"

  val split = args map { x => val z = x.split("=")  ; (z(0),z(1)) }

  val path = split.find(x => x._1 == PATH).getOrElse(throw new RuntimeException("No datapath provided"))._2
  val delta = split.find(x => x._1 == DELTA).getOrElse( ("","0.005") )._2.toDouble
  val ties = split.find(x => x._1 == TIES).getOrElse( ("","0.05") )._2.toDouble
  val prune = split.find(x => x._1 == PRUNE).getOrElse( ("","0.0") )._2.toDouble // default is no pruning
  val minseen = split.find(x => x._1 == MINSEEN).getOrElse( ("","1000") )._2.toInt
  val repeat = split.find(x => x._1 == REPREATFOR).getOrElse( ("","1") )._2.toInt
  val hle = split.find(x => x._1 == HLE).getOrElse(throw new RuntimeException("No target HLE provided"))._2
  val depth = split.find(x => x._1 == SPECIALIZATION_DEPTH).getOrElse( ("","1") )._2.toInt
  GlobalValues.glvalues("specializationDepth") = depth.toString
  val chunkSize = split.find(x => x._1 == CHUNKSIZE).getOrElse( ("","10") )._2.toInt
  val inert = split.find(x => x._1 == WITHINERTIA).getOrElse( ("","false") )._2.toBoolean
  val evalorlearn = split.find(x => x._1 == EVALORLEARN).getOrElse( ("","eval") )._2
  val expmtype = split.find(x => x._1 == EXPRMTYPE).getOrElse( ("","mln-fragment") )._2
  val db = split.find(x => x._1 == DBNAME).getOrElse( ("","None") )._2

  val allArgs = List(path,delta,ties,prune,minseen,repeat,hle,depth,chunkSize,inert,evalorlearn,expmtype)
  val names = List("path","delta","ties","prune","minSeenExmpls","repeatFor","hle","specializationDepth","chunkSize","withInert","evalorLearn","fragment","db")

  val msg = names.zip(allArgs).map(x => s"${x._1} = ${x._2}").mkString("\n")
  //val msg = args.mkString("\n")
  logger.info("\n"+msg)
*/
  //val db = "CAVIAR_Real_FixedBorders"
  val db = "CAVIAR-MERGED-COPIES"

  //val path = "/home/nkatz/dev/CAVIAR_MLN/CAVIAR_MLN/meet"
  val chunkSize = 100
  val delta = args(0).toDouble//0.00001
  val prune = args(1).toDouble//0.7
  val depth = args(2).toInt//2
  val path = args(3)
  val ties = 0.05
  val minseen = 5000
  val repeat = 1
  val hle = "moving"
  GlobalValues.glvalues("specializationDepth") = depth.toString
  val inert = false
  val evalorlearn = "learn"
  val expmtype = "whole-caviar"//"mln-fragment"
  //val msg = s"δ: $delta prune: $prune depth: $depth"


  val allArgs = List(path,delta,ties,prune,minseen,repeat,hle,depth,chunkSize,inert,evalorlearn,expmtype)
  val names = List("path","delta","ties","prune","minSeenExmpls","repeatFor","hle","specializationDepth","chunkSize","withInert","evalorLearn","fragment","db")

  val msg = names.zip(allArgs).map(x => s"${x._1} = ${x._2}").mkString("\n")
  //val msg = args.mkString("\n")
  logger.info("\n"+msg)

  val system = ActorSystem("HoeffdingLearningSystem")
  val oledActor =
    system.actorOf(Props(new Master(db,path,chunkSize,delta,ties,prune,minseen,repeat,hle,depth,inert,evalorlearn,expmtype,msg=msg)), name = "Learner") ! evalorlearn




}

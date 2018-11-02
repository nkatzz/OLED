package oled.winnow

import akka.actor.Actor
import app.runutils.RunningOptions
import oled.functions.SingleCoreOLEDFunctions._
import oled.winnow.MessageTypes.{FinishedBatchMsg, ProcessBatchMsg}
import org.slf4j.LoggerFactory


/**
  * Created by nkatz at 1/11/2018
  */

class Worker(val inps: RunningOptions) extends Actor {

  private val logger = LoggerFactory.getLogger(self.path.name)

  def receive = {

    case msg: ProcessBatchMsg =>
      val p = utils.Utils.time { processExample(msg.theory, msg.batch, msg.targetClass, inps, logger) }
      val (r, batchTime) = (p._1, p._2)
      val fmsg = new FinishedBatchMsg(r._1, r._2, r._3, r._4, r._5, r._6, batchTime, msg.targetClass)
      sender ! fmsg
  }









}

package woled

import akka.actor.Actor
import app.runutils.RunningOptions
import logic.Examples.Example
import org.slf4j.LoggerFactory

class MAPActor(inps: RunningOptions) extends Actor {

  private val logger = LoggerFactory.getLogger(self.path.name)

  def receive = {
    case exmpl: Example =>

  }

}

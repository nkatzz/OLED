
package experiments

import com.mongodb.casbah.commons.MongoDBObject
import iled.core.Core
import iled.structures.Examples.Example
import iled.structures.PriorTheory
import iled.utils.Database
import jep.Jep

/**
  * Created by nkatz on 12/21/15.
  */
object ArindamTest extends App{

  // YOU NEED TO FIX BACKTRACKING. THE CODE BELOW ONLY WORKS WITH THE
  // ORIGINAL VERSION (GOING BACK AT EACH NEW GENERALIZATION)
  // CHECK Iled.run TO SEE WHY

  val db = new Database(Core.fromDB)
  val jep = new Jep()
  db.collection.find().sort(MongoDBObject("time" -> 1)).foldLeft(List[Example](), new PriorTheory(), 0.0) {
    (x, y) => iled.core.Iled.iledTop(x._1, Example(y), x._2, jep = jep)
  }



}

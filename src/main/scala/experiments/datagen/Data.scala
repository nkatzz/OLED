package experiments.datagen

import experiments.datagen.CaviarDataGenerator._

/**
  * Created by nkatz on 11/7/15.
  */
object Data {




  def behaviours(from:Int,until:Int,what:String): List[String]  =
    (for (i <- from to until) yield s"$what${i.toString}").toList


  val ids = for (i <- 1 to 10) yield s"id$i" // persons
  val objects = for (i <- 1 to 10) yield s"object$i" // persons

  val f_orients = (x: List[Int], y: List[Int], z: List[Int]) => if (x == y) x else z.filter(p => !y.contains(p))
  val f_holds = (x: List[(String, String)], y: List[(String, String)]) => x.filter(p => !y.contains(p))
  val f_thresholds = (x: List[Int], y: List[Int]) => x.filter(p => !y.contains(p))

  class Fighting() extends HighLevelEvent{
    override val name = "fighting"
    override val initiateBehaviours = Fighting.initiateBehaviours
    override val terminateBehaviours = Fighting.terminateBehaviours
    override val initCoords = Fighting.initCoords
    override val termCoords = Fighting.termCoords
    override val initOrientations = Fighting.initOrientations
    override val termOrientations = Fighting.termOrientations
    override val holdsBehaviours = f_holds(allBehaviours,Fighting.terminateBehaviours ++ Moving.initiateBehaviours ++ Meeting.initiateBehaviours++ LeavingObject.initiateBehaviours)
    override val notHoldsBehaviors = f_holds(allBehaviours,Fighting.initiateBehaviours)
    //override val holdsBehaviours = Fighting.holdsBehaviours
    //override val notHoldsBehaviors = Fighting.notHoldsBehaviors
  }

  class Meeting() extends HighLevelEvent{
    override val name = "meeting"
    override val initiateBehaviours = Meeting.initiateBehaviours
    override val terminateBehaviours = Meeting.terminateBehaviours
    override val initCoords = Meeting.initCoords
    override val termCoords = Meeting.termCoords
    override val initOrientations = Meeting.initOrientations
    override val termOrientations = Meeting.termOrientations
    override val holdsBehaviours = f_holds(allBehaviours,Meeting.terminateBehaviours ++ Moving.initiateBehaviours ++ Fighting.initiateBehaviours++ LeavingObject.initiateBehaviours)
    override val notHoldsBehaviors = f_holds(allBehaviours,Meeting.initiateBehaviours)
  }

  class Moving() extends HighLevelEvent{
    override val name = "moving"
    override val initiateBehaviours = Moving.initiateBehaviours
    override val terminateBehaviours = Moving.terminateBehaviours
    override val initCoords = Moving.initCoords
    override val termCoords = Moving.termCoords
    override val initOrientations = Moving.initOrientations
    override val termOrientations = Moving.termOrientations
    override val holdsBehaviours = f_holds(allBehaviours,Moving.terminateBehaviours ++ Meeting.initiateBehaviours ++ Fighting.initiateBehaviours++ LeavingObject.initiateBehaviours)
    override val notHoldsBehaviors = f_holds(allBehaviours,Moving.initiateBehaviours)
  }

  class LeavingObject() extends HighLevelEvent {
    override val name = "leavingObject"
    override val initiateBehaviours = LeavingObject.initiateBehaviours
    override val terminateBehaviours = LeavingObject.terminateBehaviours
    override val initCoords = LeavingObject.initCoords
    override val termCoords = LeavingObject.termCoords
    override val initOrientations = LeavingObject.initOrientations
    override val termOrientations = LeavingObject.termOrientations
    override val holdsBehaviours = f_holds(allBehaviours,
      Fighting.initiateBehaviours ++ Moving.initiateBehaviours ++ Meeting.initiateBehaviours ++ LeavingObject.initiateBehaviours ++ LeavingObject.terminateBehaviours)
    override val notHoldsBehaviors = f_holds(allBehaviours,LeavingObject.initiateBehaviours)
  }

  class DummyHLE() extends HighLevelEvent {
    override val name = "dummy"
  }

  object DownTime {
    //val behaviours = Fighting.
    //val coords = List(60,70,80) map (p => getCoords(coordsUpper,p,"greaterThan"))
    val coords = List((100,23,56,200),(23,256,654,2))
    //val orientations = List(60,90,180) map (p => getOrientations(orientUpper,p,"greaterThan"))
    val orientations = List((180,0),(90,270))
  }

  val getInitBehavs = (init: List[String]) => init.toSet.subsets(2).toList.map(x => x.toList).map(p=>(p.head,p(1))).++(init.map(x => (x,x)))
  val getTermBehavs = (term: List[String],initBehavs:List[(String,String)]) =>
    term.toSet.subsets(2).toList.map(x => x.toList).map(p=>(p.head,p(1))).++(term.map(x => (x,x))).filter(z => !initBehavs.contains(z))


  object LeavingObject {

    val initiateBehaviours =
      List(("walking","inactive"),("standing","inactive"),("active","inactive"),
           ("walking","appears"),("standing","appears"),("active","appears"))

    val terminateBehaviours =
      List(("walking","disappears"),("standing","disappears"),("active","disappears"))

    val initCoords = List(10) map (p => getCoords(coordsUpper,p,"smallerThan"))
    val termCoords = initCoords
    val initOrientations = List(0,30,45,60,90,180) map (p => getOrientations(orientUpper,p,"smallerThan"))
    val termOrientations = initOrientations
  }



  object Fighting {

    //val _init = List(behaviours(1,2,"abrupt"),behaviours(1,2,"kicking"), behaviours(1,2,"punching")).flatten
    //val _term = List(behaviours(1,2,"inactive"), behaviours(1,2,"lying"),behaviours(1,2,"running")).flatten

    //val initiateBehaviours = getInitBehavs(_init)
    //val terminateBehaviours = getTermBehavs(_term,initiateBehaviours)


    val initiateBehaviours =
      List(("abrupt","abrupt"),("abrupt","punching"),("punching","abrupt"),
           ("abrupt","kicking"),("kicking","abrupt"),("kicking","kicking"))
           //("punching","kicking"),("punching","punching"),("kicking","punching"))

    val terminateBehaviours =
      List(("lying","abrupt"),("lying","lying"),("running","standing"),
           ("lying","running"),("running","lying"))



    /*
    val initCoords = List(20,21,22,23,24,25) map (p => getCoords(coordsUpper,p,"smallerThan"))
    val termCoords = List(25,26,27,28,29,30) map (p => getCoords(coordsUpper,p,"greaterThan"))
    */
    // reduce the number of different thresholds
    val initCoords = List(25) map (p => getCoords(coordsUpper,p,"smallerThan"))
    val termCoords = List(25) map (p => getCoords(coordsUpper,p,"greaterThan"))

    val initOrientations = List(0,30,45,60,90,180) map (p => getOrientations(orientUpper,p,"smallerThan"))
    val termOrientations = List(0,30,45,60,90,180) map (p => getOrientations(orientUpper,p,"smallerThan"))

  }





  object Meeting  {
    //private val _init = List(behaviours(1,2,"active"),behaviours(1,2,"inactive"), behaviours(1,2,"standing")).flatten
    //private val _term =  List(behaviours(1,2,"running"),behaviours(1,2,"active"),behaviours(1,2,"walking")).flatten

    //private val _init = List(behaviours(1,2,"active2")).flatten
    //private val _term =  List(behaviours(1,2,"running2")).flatten

    //val initiateBehaviours = getInitBehavs(_init)
    //val terminateBehaviours = getTermBehavs(_term,initiateBehaviours)


    val initiateBehaviours =
      List(("standing","standing"),("active","standing"),("standing","active"),
           ("standing1","standing1"),("standing2","standing2"),("standing3","standing3"))
    val terminateBehaviours =
      List(("walking","walking"),("walking2","walking2"),
           ("running","walking"),("walking","running"))



    /*
    val initCoords = List(25,26,27,28,29,30) map (p => getCoords(coordsUpper,p,"smallerThan"))
    val termCoords = List(30,31,32,33,34,35) map (p => getCoords(coordsUpper,p,"greaterThan"))
    */

    val initCoords = List(30) map (p => getCoords(coordsUpper,p,"smallerThan"))
    val termCoords = List(30) map (p => getCoords(coordsUpper,p,"greaterThan"))

    val initOrientations = List(0,30,45,60,90,180) map (p => getOrientations(orientUpper,p,"smallerThan"))
    val termOrientations = List(0,30,45,60,90,180) map (p => getOrientations(orientUpper,p,"smallerThan"))

  }


  object Moving {

    private val _init = List(behaviours(1,2,"walking")).flatten
    private val _term = List(behaviours(1,2,"running"),behaviours(1,2,"active"),behaviours(1,2,"walking")).flatten

    //private val _init = List(behaviours(1,1,"walking3")).flatten
    //private val _term = List(behaviours(1,1,"running3")).flatten

    //val initiateBehaviours = getInitBehavs(_init)
    //val terminateBehaviours = getTermBehavs(_term,initiateBehaviours)


    val initiateBehaviours =
      List(("walking5","walking5"),("walking6","walking6"),("walking7","walking7"))
        //("standing1","standing1"),("standing2","standing2"),("standing3","standing3"))
    val terminateBehaviours =
          List(("running","running"),("standing","standing"),("running1","running1"))
            //("running1","walking1"),("walking1","running1"),("walking2","running1"))


    /*
    val initCoords = List(35,36,37,38,39,40) map (p => getCoords(coordsUpper,p,"smallerThan"))
    val termCoords = List(40,41,42,43,44,45) map (p => getCoords(coordsUpper,p,"greaterThan"))
    */

    val initCoords = List(40) map (p => getCoords(coordsUpper,p,"smallerThan"))
    val termCoords = List(40) map (p => getCoords(coordsUpper,p,"greaterThan"))

    val initOrientations = List(0,30,45) map (p => getOrientations(orientUpper,p,"smallerThan"))
    val termOrientations = List(45) map (p => getOrientations(orientUpper,p,"greaterThan"))
  }




}

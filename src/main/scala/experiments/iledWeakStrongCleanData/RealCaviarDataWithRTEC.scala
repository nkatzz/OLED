package experiments.iledWeakStrongCleanData

import com.mongodb.casbah.Imports._
import com.mongodb.casbah.commons.MongoDBObject
import iled.core.Core
import iled.structures.Examples.Example
import iled.utils.Database
import scala.io.Source
import scala.collection.immutable.SortedMap
import RTEC_CAVIAR_Annotation._


import scala.util.parsing.combinator.JavaTokenParsers

/**
  * Created by nkatz on 11/27/15.
  */
object RealCaviarDataWithRTEC extends App {

  trait CaviarExpression {
    val tostring = ""
    val time = ""
  }

  case class STB(id: String, lle: String, override val time: String) extends CaviarExpression {
    override val tostring = s"happensAt($lle($id),$time)"
  }

  case class Coord(id:String, x: String, y: String, override val time: String) extends CaviarExpression {
    override val tostring = s"coords($id,$x,$y,$time)"
  }

  case class Orientation(id: String, value: String, override val time: String) extends CaviarExpression {
    override val tostring = s"orientation($id,$value,$time)"
  }

  case class Appearance(holdsOrHappens: String, id: String, visibility: String, override val time: String) extends CaviarExpression {
    override val tostring = holdsOrHappens match {
      case "holdsAtIE" => s"holdsAt($visibility($id),$time)" // e.g. holdsAtIE( orientation( id0 )=169, 2120  )
      case "happensAtIE" => s"happensAt($visibility($id),$time)"  // happensAtIE( disappear( id2 ),209320 )
    }
  }

  class DataContainer(val time: Int, val annotation: List[String], val narrative: List[String])

  class CaviarParser extends JavaTokenParsers {
    //assert( holdsAtIE( orientation( id0 )=169, 2120  ) ),  assert(  holdsAtIE( appearance( id0 )=visible,  2120 ) ),   (or assert(  happensAtIE( disappear( id2 ),209320 ) ),)

    def word: Parser[String] = """[a-z][a-zA-Z0-9_]*""".r
    def number: Parser[String] = floatingPointNumber

    def lle: Parser[STB] = "assert"~"("~"holdsAtIE"~"("~word~"("~word~")"~"="~word~","~number~")"~")" ^^ {
      case "assert"~"("~"holdsAtIE"~"("~lle~"("~id~")"~"="~_true~","~time~")"~")" => STB(id,lle,time)
    }

    def coord: Parser[Coord] =
      "assert"~"("~"holdsAtIE"~"("~"coord"~"("~word~","~number~","~number~")"~"="~word~","~number~")"~")" ^^ {
        case "assert"~"("~"holdsAtIE"~"("~"coord"~"("~id~","~x~","~y~")"~"="~_true~","~time~")"~")" => Coord(id,x,y,time)
      }

    def orientation: Parser[Orientation] = "assert"~"("~"holdsAtIE"~"("~"orientation"~"("~word~")"~"="~number~","~number~")"~")" ^^ {
      case "assert"~"("~"holdsAtIE"~"("~"orientation"~"("~id~")"~"="~value~","~time~")"~")" => Orientation(id,value,time)
    }

    def appearanceHolds: Parser[Appearance] = "assert"~"("~"holdsAtIE"~"("~"appearance"~"("~word~")"~"="~word~","~number~")"~")" ^^ {
      case "assert"~"("~"holdsAtIE"~"("~"appearance"~"("~id~")"~"="~visibility~","~time~")"~")" => Appearance("holdsAtIE",id,visibility,time)
    }

    def appearanceHappens: Parser[Appearance] = "assert"~"("~"happensAtIE"~"("~word~"("~word~")"~","~number~")"~")" ^^ {
      case "assert"~"("~"happensAtIE"~"("~visibility~"("~id~")"~","~time~")"~")" => Appearance("happensAtIE",id,visibility,time)
    }

    def line1: Parser[(STB,Coord)] = lle ~ "," ~ coord ~ (","|".") ^^ { case x ~ "," ~ y ~ (","|".") => (x,y)}

    def line2: Parser[(Orientation,Appearance)] = orientation ~ "," ~ (appearanceHolds | appearanceHappens) ~ (","|".") ^^ { case x ~ "," ~ y ~ (","|".") => (x,y) }

    def line: Parser[(CaviarExpression,CaviarExpression)] = (line1 | line2) ^^ { case x => x }

    def _parse(parser: Parser[(CaviarExpression,CaviarExpression)], expression: String): Option[(CaviarExpression,CaviarExpression)] = {
      parseAll(parser, expression) match {
        case Success(result, _) => Some(result)
        case f                  => None
      }
    }

  }






  def fileToList(f: String) = Source.fromFile(f).getLines.toList

  object AnnotationHandler {
    def hles(hle: String, ids:(String,String), _intervals: List[(Int,Int)]) = {
      //val intervals = _intervals.filter(x => x._2 - x._1 > 40) // filter out instantaneous HLEs
      val intervals = _intervals
      val f = (x: (String,String)) => if (hle == "leavingObject") List((x._1,x._2)) else List((x._1,x._2),(x._2,x._1))
      // For an SDF fluent (one that is defined via holdsFor) the annotation is [s+40,e+40) for each RTEC-produced interval.
      // For a regular fluent (defined via initiatedAt) the annotation is [s,e)
      val times = intervals flatMap (x => if (SDFs.contains(hle)) List.range(x._1 + 40,x._2 + 40,40) else List.range(x._1,x._2-40,40))
      val t = times flatMap (x => f(ids) map (y => (x,s"holdsAt($hle(${y._1},${y._2}),$x)") ) ) //groupBy(_._1) map { case (k,v) => (k,v map (p => p._2)) }
      t
    }
    val _doo = (HLEs map {case (k,v) => v map {case (k1,v1) => hles(k,k1,v1)}}).toList.flatten.flatten.groupBy(_._1).toList.map(x => (x._1, for (y <- x._2) yield y._2 ))
    val sorted = SortedMap[Int, List[String]]() ++ _doo
  }

  object NarrativeHandler extends CaviarParser {
    def persons(id: String, intervals: List[(Int,Int)]) = {
      // person in RTEC is an SDF (it is defined via holdsFor). So we generate a person predicate
      // following the same logic that holds for SDF's annotation.
      val times = intervals flatMap (x =>  List.range(x._1,x._2,40))
      times map (time => (time,s"person($id,$time)")) //groupBy(_._1) map { case (k,v) => (k,v map (p => p._2)) }
    }
    def distances(ids: (String,String,Int), intervals: List[(Int,Int)]) = {
      val f = (x: (String,String,Int)) => List((x._1,x._2,x._3),(x._2,x._1,x._3))
      val times = intervals flatMap (x => List.range(x._1+40,x._2+40,40))
      times flatMap (x => f(ids) map (y => (x,s"close(${y._1},${y._2},${y._3},$x)") ) ) //groupBy(_._1) map { case (k,v) => (k,v map (p => p._2)) }
    }
    /*
    def distSymm(ids: (String,String,Int), intervals: List[(Int,Int)]) = {
      val f = (x: (String,String,Int)) => List((x._1,x._2,x._3),(x._2,x._1,x._3))
      val times = intervals flatMap (x => List.range(x._1+40,x._2+40,40))
      times flatMap (x => f(ids) map (y => (x,s"closeSymm1(${y._1},${y._2},${y._3},$x)") ) ) //groupBy(_._1) map { case (k,v) => (k,v map (p => p._2)) }
    }
    */
    val l1 = fileToList("/home/nkatz/Desktop/CAVIAR-FROM-RTEC/RTEC/examples/caviar/data/complete caviar/movementB.prolog")
    val l2 = fileToList("/home/nkatz/Desktop/CAVIAR-FROM-RTEC/RTEC/examples/caviar/data/complete caviar/appearance.prolog")
    val l3 = l1 ++ l2
    val _parsed = l3 map (x => _parse(line, x)) flatMap {
      case Some(y) => List((y._1.time.toInt,y._1.tostring),(y._2.time.toInt,y._2.tostring))
      case _ => List()
    } groupBy(_._1) map { case (k,v) => (k,v map (p => p._2)) }

    //val perns = (RTEC_CAVIAR_Annotation.persons map {case (k,v) => persons(k,v)}).toList.flatten.groupBy(_._1).toList.map(x => (x._1, for (y <- x._2) yield y._2 ))
    //val dist = (RTEC_CAVIAR_Annotation.dists map {case (k,v) => distances(k,v)}).toList.flatten.groupBy(_._1).toList.map(x => (x._1, for (y <- x._2) yield y._2 ))
    //val distSymmetric = (RTEC_CAVIAR_Annotation.distsSymmetric map {case (k,v) => distSymm(k,v)}).toList.flatten.groupBy(_._1).toList.map(x => (x._1, for (y <- x._2) yield y._2 ))
    val parsed = _parsed.toList
    //val joined = (parsed ++ perns ++ dist)  groupBy(_._1) map { case (k,v) => (k,v flatMap (p => p._2)) }
    val joined = parsed groupBy(_._1) map { case (k,v) => (k,v flatMap (p => p._2)) }
    // add start-end points
    val startEnds = getStartingEndingPoints
    val narrative = for ( (k,v) <- joined; updatedVal = if (startEnds.keySet.contains(k)) v ++ startEnds(k) else v ) yield k -> updatedVal
    val sorted = SortedMap[Int, List[String]]() ++ narrative
  }


  def toMongo(data: SortedMap[Int, Map[String, List[String]]]) = {
    val mongoClient = MongoClient()
    mongoClient.dropDatabase("CAVIAR_RTEC_CLEAN")
    val collection = mongoClient("CAVIAR_RTEC_CLEAN")("examples")
    for ( (k,v) <- data) {
      val annotation = if (v.keySet.contains("annotation")) v("annotation") else List[String]()
      val narrative = if (v.keySet.contains("narrative")) v("narrative") else List[String]()
      val entry = MongoDBObject("time" -> k) ++ ("annotation" -> annotation) ++ ("narrative" -> narrative)
      collection.insert(entry)
      println(s"inserted $entry")
    }
    mongoClient.close()
  }

/*
  val narrative = NarrativeHandler.sorted.toList map { x => (x._1, "narrative" -> x._2) }
  val annotation = AnnotationHandler.sorted.toList map { x => (x._1, "annotation" -> x._2) }
  val merged = (narrative ++ annotation) groupBy(_._1) map { case (k,v) => (k,(v map (p => p._2)).toMap) }
  val sorted = SortedMap[Int, Map[String, List[String]]]() ++ merged
  toMongo(sorted)
*/


  def getStartingEndingPoints = {
    val lines = Source.fromFile("/home/nkatz/Desktop/caviar").getLines.toList
    def show(s:List[String]) = println(s)
    val points = for (x <- lines;
           inner = x.split("\\[")(1).split("\\]")(0);
           parsed = ParseStoreCAVIAR._getParseResult(ParseStoreCAVIAR._parse(ParseStoreCAVIAR.line, inner));
           startEnds = parsed.map(_.tostring) filter(x => x.contains("start") || x.contains("end") ) ;
           //y = show(startEnds);
           time = parsed.head.terms.last.tostring.toInt)
        yield time -> startEnds
    points filter (_._2 != List()) toMap
  }




  def modifyExisting(annotation: SortedMap[Int, Map[String, List[String]]] ) = {

    def createUpdatedDBObject(correct: List[String], existing: Example) = {
      val time = existing.time.toInt
      val nar = existing.narrative
      MongoDBObject("time" -> time) ++ ("annotation" -> correct) ++ ("narrative" -> nar)
    }

    def getUpdatedObjects(db: Database) = {
      val updated =
        for ( x <- db.collection.find().sort(MongoDBObject("time" -> 1)) ;
            e = Example(x) ;
            time = e.time.toInt ;
            correctAnnot = if (annotation.keySet.contains(time)) annotation(time)("annotation") else List[String]()
      ) yield (time, createUpdatedDBObject(correctAnnot, e))
      updated.toList
    }

    val db = new Database(Core.fromDB)
    val newObjects = getUpdatedObjects(db)
    newObjects foreach {
      x => {
        val update = db.collection.update( MongoDBObject("time" -> x._1), x._2)
        //println("update", update)
      }
    }
  }

  val annotation = AnnotationHandler.sorted.toList map { x => (x._1, "annotation" -> x._2) }
  val merged = annotation groupBy(_._1) map { case (k,v) => (k,(v map (p => p._2)).toMap) }
  val sorted = SortedMap[Int, Map[String, List[String]]]() ++ merged
  modifyExisting(sorted)

}

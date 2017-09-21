package data_handling.caviar_data

import java.io.File

import com.mongodb.casbah.Imports._
import com.mongodb.casbah.MongoClient
import com.mongodb.casbah.commons.MongoDBObject
import logic.Literal
import utils.parsers.ClausalLogicParser

import scala.collection.immutable.SortedMap


/**
  * Created by nkatz on 3/13/17.
  *
  * Parse the CAVIAR dataset into mongodb, see further notes below.
  *
  */

object ParseCAVIAR extends ClausalLogicParser {



  /*
  Example({ "_id" : { "$oid" : "56d58f08e4b0842fb35754c8"} , "time" : 1077520 , "annotation" : [ ] ,
   "narrative" : [ "orientation(id1,128,1077520)" , "happensAt(disappear(id1),1077520)" , "orientation(id3,164,1077520)" , "holdsAt(visible(id3),1077520)" ,
    "orientation(id4,166,1077520)" , "holdsAt(visible(id4),1077520)" , "happensAt(running(id1),1077520)" , "coords(id1,77,60,1077520)" , "happensAt(walking(id3),1077520)" ,
     "coords(id3,98,201,1077520)" , "happensAt(inactive(id4),1077520)" , "coords(id4,82,206,1077520)"]},,List(),List(),,false,false,List(),List())
  */



  val fixedBordersDBName = "CAVIAR_Real_FixedBorders"
  val originalDBName = "CAVIAR_Real_original"

  def main(args: Array[String]) = {

    // with fixed borders
    //val dataPath = "/home/nkatz/dev/CAVIAR-abrupt-corrected-borderlines"
    //val fixedBorders = true

    // original version
    //val dataPath = "/home/nkatz/dev/CAVIAR-abrupt-original"
    //val dbname = "caviar"

    val dataPath = args(0)
    val dbname = args(1)

    run(dataPath, dbname)
  }

  /**
    * The number of training interpretations for each video is
    * the number of distinct time points in that video times
    * the 2-combinations of distinct ids.
    *
    * @param path
    */
  def countInterpretations(path: String) = {
    val d = new File(path)
    val idPattern = "id[0-9]+".r
    val innerFolders = d.listFiles.sortBy(_.getName.split("-")(0).toInt)
    var totalSize = 0
    for (f <- innerFolders) {
      println(s"Video ${f.getCanonicalPath}")
      val files = f.listFiles.filter(x => movementOnly.exists(p => x.getName.contains(p)))
      val contents =
        (for (f <- files)
          yield scala.io.Source.fromFile(f).getLines().filter(p => !p.startsWith("%"))).toList.flatten.mkString.replaceAll("\\s","").split("\\.").toList
      val parsed = contents.flatMap(x => parseAll(caviarParser(0),x).getOrElse(List(""))).filter(_!="").asInstanceOf[List[Atom]]
      //println(parsed map (_.atoms))
      val allAtoms = parsed flatMap (_.atoms) map (x => Literal.toLiteral(x))
      val times = allAtoms.map(_.terms.reverse.head.tostring).distinct.length
      //println(times.length)
      val ids = parsed.flatMap(_.atoms).flatMap(z => idPattern.findAllIn(z).toList).distinct.length
      val size = if (ids > 1) (times * utils.Utils.combinations(ids,2)).toInt else times
      println(s"current video size: $size")
      totalSize = totalSize + size
    }
    println(s"Total size: $totalSize")
  }

  // We'll maintain two versions of CAVIAR: The first will be the corrected one (where examples at "borderlines"
  // where a fluent changes its value, have been corrected -- pushed a frame forward for initiation, add extra
  // annotated frame for termination). This has happened everywhere (consulting the narrative at the same time),
  // except in cases of more than two persons participating in an HLE (e.g. happensAt( moving( grp_ID0, [ id0, id2, id3 ]), 13440).)
  // The corrected version of CAVIAR is under /dev/CAVIAR-abrupt-corrected-borderlines.
  // The second version is the original one, nothing has been tweaked. It is located under /dev/CAVIAR-abrupt-original

  def run(path: String, dbName: String) = {
    //val dbName = if (fixedBorders) fixedBordersDBName else originalDBName
    val mongoClient = MongoClient()
    mongoClient.dropDatabase(dbName)
    val collection = mongoClient(dbName)("examples")

    val d = new File(path)
    val innerFolders = d.listFiles.sortBy(_.getName.split("-")(0).toInt)
    var lastTime = 0
    for (f <- innerFolders) {
      println(s"Parsing video ${f.getCanonicalPath}")
      val files = f.listFiles.filter(x => dataFileNames.exists(p => x.getName.contains(p)))
      val contents =
        (for (f <- files)
          yield scala.io.Source.fromFile(f).getLines().filter(p => !p.startsWith("%"))).toList.flatten.mkString.replaceAll("\\s","").split("\\.").toList

      val parsed = contents.flatMap(x => parseAll(caviarParser(lastTime),x).getOrElse(List(""))).filter(_!="").asInstanceOf[List[Atom]]
      val atoms = SortedMap[Int, List[Atom]]() ++ parsed.groupBy(_.time.toInt)
      for ( (k,v) <- atoms ) {
        val narrative = v.filter(x => !x.annotationAtom).flatMap(z => z.atoms)
        val annotation = v.filter(x => x.annotationAtom).flatMap(z => z.atoms)
        val entry = MongoDBObject("time" -> k) ++ ("annotation" -> annotation) ++ ("narrative" -> narrative)
        collection.insert(entry)
        //println(s"inserted $entry")
      }
      lastTime = atoms.keySet.toList.reverse.head+40 // the last time point
    }
  }

  val hleMapping = Map("moving" -> "moving", "fighting" -> "fighting", "leaving_object" -> "leavingObject", "interacting" -> "meeting")
  val correctedCaviarPath = "/home/nkatz/dev/CAVIAR-abrupt-corrected-borderlines"
  val originalCaviarPath = "/home/nkatz/dev/CAVIAR-abrupt-original"
  val dataFileNames = List("AppearenceIndv","MovementIndv","SituationGrp")
  val movementOnly = List("MovementIndv")

  def word: Parser[String] = """[A-Za-z0-9_]*""".r ^^ { x => x }
  def person: Parser[Person] = "id" ~ number ^^ { case x ~ y => new Person(x + y) }
  def persons: Parser[List[Person]] = "[" ~> repsep(person, ",") <~ "]"
  def time: Parser[String] = number
  def orientationValue: Parser[String] = number
  def appearanceValue: Parser[String] = word
  def coordinates: Parser[(String, String)] = "(" ~ number ~ "," ~ number ~ ")" ^^ { case "(" ~ x ~ "," ~ y ~ ")" => (x, y) }
  def meeting: Parser[String] = "interacting"
  def moving: Parser[String] = "moving"
  def fighting: Parser[String] = "fighting"
  def leavingObject: Parser[String] = "leaving_object"
  def walking: Parser[String] = "walking"
  def active: Parser[String] = "active"
  def inactive: Parser[String] = "inactive"
  def running: Parser[String] = "running"
  def abrupt: Parser[String] = "abrupt"
  def happens: Parser[String] = "happensAt"
  def holds: Parser[String] = "holdsAt"
  def orientation: Parser[String] = "orientation"
  def appearance: Parser[String] = "appearance"
  def coords: Parser[String] = "coord"

  def annotationParser(pastTime: Int): Parser[AnnotationAtom] =
    happens ~ "(" ~ (meeting | moving | fighting | leavingObject) ~ "(" ~ word ~ "," ~ persons ~ ")" ~ "," ~ time ~ ")" ^^ {
      case _~"("~x~"("~_~","~y~")"~","~z~")" => new AnnotationAtom(x, y, (z.toInt+pastTime).toString)
    }
  def lleParser(pastTime: Int): Parser[NarrativeAtom] = happens ~ "(" ~ (walking | active | inactive | running | abrupt) ~ "(" ~ person ~ ")" ~ "," ~ time ~ ")" ^^ {
    case _~"("~lle~"("~p~")"~","~t~")" => new NarrativeAtom(what = lle, id = p.id, time = (t.toInt+pastTime).toString)
  }

  /* This is a utility parser used by data_handling.CopyCAVIAR. It doesn't push time forward*/
  def lleParser1: Parser[NarrativeAtom] = happens ~ "(" ~ (walking | active | inactive | running | abrupt) ~ "(" ~ person ~ ")" ~ "," ~ time ~ ")" ^^ {
    case _~"("~lle~"("~p~")"~","~t~")" => new NarrativeAtom(what = lle, id = p.id, time = t)
  }

  def orientationParser(pastTime: Int): Parser[NarrativeAtom] = holds ~ "(" ~ orientation ~ "(" ~ person ~ ")" ~ "=" ~ number ~ "," ~ time ~ ")" ^^ {
    case _~"("~_~"("~p~")"~"="~v~","~t~")" => new NarrativeAtom(what = "orientation", id = p.id, orientation = v, time = (t.toInt+pastTime).toString)
  }
  def appearanceParser(pastTime: Int): Parser[NarrativeAtom] = holds ~ "(" ~ appearance ~ "(" ~ person ~ ")" ~ "=" ~ word ~ "," ~ time ~ ")" ^^ {
    case _~"("~_~"("~p~")"~"="~v~","~t~")" => new NarrativeAtom(what = "appearance", id = p.id, appearance = v, time = (t.toInt+pastTime).toString)
  }
  def coordsParser(pastTime: Int): Parser[NarrativeAtom] = holds ~ "(" ~ coords ~ "(" ~ person ~ ")" ~ "=" ~ coordinates ~ "," ~ time ~ ")" ^^ {
    case _~"("~_~"("~p~")"~"="~c~","~t~")" => new NarrativeAtom(what = "coord", id = p.id, xcoord = c._1, ycoord = c._2, time = (t.toInt+pastTime).toString)
  }

  def caviarAtomParser(pastTime: Int): Parser[Atom] = annotationParser(pastTime)|lleParser(pastTime)|orientationParser(pastTime)|appearanceParser(pastTime)|coordsParser(pastTime)
  def caviarParser(pastTime: Int): Parser[List[Atom]] = rep(caviarAtomParser(pastTime))
  class Person(val id: String)

  trait Atom {
    val annotationAtom: Boolean
    val atoms: List[String]
    val time: String
  }

  class AnnotationAtom(val HLE: String, val persons: List[Person], val time: String) extends Atom{
    val annotationAtom = true
    val atoms =
      if (HLE == "leaving_object") {
        List(s"holdsAt(${hleMapping(HLE)}(${persons.head.id},${persons(1).id}),$time)")
      } else {
        persons.toSet.subsets(2).flatMap(y => for (z <- y.toList.permutations) yield s"holdsAt(${hleMapping(HLE)}(${z.head.id},${z(1).id}),$time)").toList
      }
  }

  // what is either an LLE, or orientation, appearance, coord
  class NarrativeAtom(val what: String = "none", val id: String, val xcoord: String = "none",
                      val ycoord: String = "none", val orientation: String = "none",
                      val appearance: String = "none", val time: String) extends Atom{

    val annotationAtom = false
    val atoms = what match {
      case ("walking" | "active" | "inactive" | "running" | "abrupt") => List(s"happensAt($what($id),$time)")
      case "coord" => List(s"coords($id,$xcoord,$ycoord,$time)")
      case "appearance" => appearance match {
        case "appear" | "disappear" => List(s"happensAt($appearance($id),$time)")
        case _ => List(s"holdsAt($appearance($id),$time)")
      }
      case "orientation" => List(s"orientation($id,$orientation,$time)")
    }
  }

}
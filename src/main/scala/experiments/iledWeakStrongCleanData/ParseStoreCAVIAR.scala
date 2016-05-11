package experiments.iledWeakStrongCleanData

import com.mongodb.casbah.Imports._
import com.mongodb.casbah.commons.MongoDBObject
import iled.parsers.ClausalLogicParser
import iled.structures.Exceptions.MyParsingException
import iled.structures.Literal
import iled.structures.Expression
import scala.collection.immutable.SortedMap
import scala.io.Source
import scala.util.Try

/**
  * Created by nkatz on 14/12/2015.
  */
object ParseStoreCAVIAR extends ClausalLogicParser{

  case class LiteralListAsExpression(val x: String) extends Expression

  def line: Parser[List[Literal]] = repsep(literal, ",")

  def _parse(parser: Parser[List[Literal]], expression: String): Option[List[Literal]] = {
    parseAll(parser, expression) match {
      case Success(result, _) => Some(result)
      case f                  => None
    }
  }

  def _getParseResult(x: Option[List[Literal]]): List[Literal] = x match {
    case Some(y) => y
    case _       => throw new MyParsingException(x.toString)
  }


  def main(args: Array[String]) {
    def show(s:String) = println(s)
    val mongoClient = MongoClient()
    mongoClient.dropDatabase("CAVIAR_RTEC_CLEAN")
    val collection = mongoClient("CAVIAR_RTEC_CLEAN")("examples")

    val lines = Source.fromFile("/home/nkatz/Desktop/caviar").getLines.toList

    var _id = 0

    for (x <- lines) {
      _id = _id+1
      val s = x.split(" ")
      val time = s.head.toInt
      val narrative = s.tail
      val m = MongoDBObject("count" -> _id) ++ ("time" -> time) ++ ("annotation" -> List[String]()) ++ ("narrative" -> narrative)
      println(m)
      collection.insert(m)
    }

    /*
    val entries =
      for (x <- lines;
           inner = x.split("\\[")(1).split("\\]")(0);
           y = show(x);
           parsed = _getParseResult(_parse(line, inner));
           toStr = parsed.map(_.tostring);
           annotation = toStr.filter(
             x => x.contains("meeting") || x.contains("moving") || x.contains("fighting") || x.contains("leavingObject"));
           narrative = toStr.filter(x => !annotation.contains(x));
           time = parsed.head.terms.last.tostring.toInt;
           _id = lines.indexOf(x))
        yield MongoDBObject("_id" -> _id) ++ ("time" -> time) ++ ("annotation" -> annotation) ++ ("narrative" -> narrative)




        val sorted = entries.sortBy(_.asInstanceOf[BasicDBObject].get("_id").asInstanceOf[Int])
        for (s <- sorted) {
          val t = s.asInstanceOf[BasicDBObject].get("time")
          if (t.toString == "3360") {
            val stop = "stop"
          }
          println(s.asInstanceOf[BasicDBObject].get("time"))
          collection.insert(s)
        }
*/

    mongoClient.close()
  }





}

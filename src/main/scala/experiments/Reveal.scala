package experiments

import java.io.File

import iled.parsers.ClausalLogicParser

import scala.collection.immutable.SortedMap
import scala.io.Source

/**
  * Created by nkatz on 1/5/16.
  */


object Reveal extends ClausalLogicParser {

  /*
  Ground truth examples:

  happensAt( end_influence( 156290470,low ),89415 ).
  happensAt( start_influence( 20829720,low ),1 ).
  happensAt( topic( 10904,this_morning ),70151 ).


  Annotation examples:

  holdsFor( influential( 1000039248 )=true,[(55764,68606)] ).
  holdsFor( trust( 1000164918,Allahu_Akbar )=true,[(66070,68607)] ).
  holdsFor( influential( 22602800 )=true,[(1,7944),(7945,14898),(14899,22750),(37215,55763),(55764,68606),(68607,78426),(78427,89415)] ).

  "happensAt( end_influence( 568206581,low ),89415 )."

"happensAt(topic(2334762298,GK_Conference),3)".replaceAll("\\.","").replaceAll("("," ").replaceAll(")"," ").replaceAll(","," ")

  */

  def main(args: Array[String]): Unit = {
    //val data = getGrndTruth("/home/nkatz/Desktop/Reveal-learning/ground-truth")  //  save the data in file

    /*
    val m = Source.fromFile("/home/nkatz/Desktop/Reveal-learning/draft").getLines.toList.
      filter(z => z.replaceAll("\\s","")!="").
      map(x => x.split("\\s")).sortBy(_(5).toInt).
      map(_v => s"${_v(5)} ${_v(0)}(${_v(1)}(${_v(2)},${_v(3).replace(_v(3).head,_v(3).head.toLower)}),${_v(5)})").mkString("\n")

    iled.utils.Utils.writeLine(m,"/home/nkatz/Desktop/Reveal-learning/narrative","append")
    */
    val influence = getAnnotation(Source.fromFile(influenceData).getLines.toList,inflAtom)
    println("Done with influence")
    val trust = getAnnotation(Source.fromFile(trustData).getLines.toList,trustAtom)
    println("Done with trust")
    val narrative = Source.fromFile(narrativeData).getLines.toList.map(x =>{
      val split = x.split(" ")
      (split(0).toInt,split(1))
    } )
    val all = SortedMap[Int, List[String]]() ++ (influence ::: trust ::: narrative).groupBy(_._1).map{case (k,v) => (k,v.map(_._2))}
    all foreach println
  }

  def getGrndTruth(dir: String): List[String] = {
    val d = new File(dir)
    var data = List[String]()
    val files = d.listFiles.filter(_.isFile).toList.sortBy(f => f.getName.split("\\.")(0).split("_")(2).toInt) map (_.getCanonicalPath)
    for (x <- files) {
      val contents = Source.fromFile(x).getLines.toList.map(_.replaceAll("\\s","")).mkString("\n")
      data  = data :+ contents
    }
    iled.utils.Utils.writeLine(data.mkString("\n"),"/home/nkatz/Desktop/Reveal-learning/all-narrative.txt","append")
    data
  }

  def getAnnotation(data: List[String], parser: Parser[Atom]) = {
    data.map( x => _parse(parser,x) ).flatMap(z => {println(z.get.atoms.length); z.get.atoms} )
  }

  val influenceData = "/home/nkatz/Desktop/Reveal-learning/annotation/influence.prolog"
  val trustData = "/home/nkatz/Desktop/Reveal-learning/annotation/trust.prolog"
  val narrativeData = "/home/nkatz/Desktop/Reveal-learning/narrative"

  trait Atom {
   def times: List[Int]
   def atoms: List[(Int,String)]
  }

  class InfluenceAtom(val id: String, val intervals: List[(Int,Int)]) extends Atom {
    //def times = intervals.flatMap(x => List.range(x._1,x._2-1,1))
    def times = List(1,2)
    def atoms = List((1,"test"))
    //def atoms = intervals.flatMap(x => for (x <- x._1 to x._2 - 1) yield (x,s"holdsAt(influential($id),$x)") )
    for (x <- intervals) {
      val test = (for (x <- x._1 to x._2 - 1) yield (x,s"holdsAt(influential($id),$x)")).mkString("\n")
      println(test)
      iled.utils.Utils.writeLine(test,"/home/nkatz/Desktop/data","append")
    }
  }

  class TrustAtom(val id: String, val topic: String, val intervals: List[(Int,Int)]) extends Atom {
    val _topic = topic.replace(topic.head,topic.head.toLower)
    def times = intervals.flatMap(x => List.range(x._1,x._2-1,1))
    def atoms = for (x <- times) yield (x,s"holdsAt(trust($id,${_topic}),$x))")
  }

  def id: Parser[String] = number
  def topic: Parser[String] = anyWord
  def interval: Parser[(Int,Int)] = "("~number~","~(number|"inf")~")" ^^ {
    case "("~x~","~"inf"~")" => (x.toInt,89415)
    case "("~x~","~y~")" => (x.toInt,y.toInt)
  }

  def intervals: Parser[List[(Int,Int)]] = "[" ~> repsep(interval, ",") <~ "]"

  def inflAtom: Parser[InfluenceAtom] =
    "holdsFor"~"("~"influential"~"("~id~")"~"="~"true"~","~intervals~")"~"." ^^ {
      case "holdsFor"~"("~"influential"~"("~_id~")"~"="~"true"~","~intrvl~")"~"." => new InfluenceAtom(_id,intrvl)
    }

  def trustAtom: Parser[TrustAtom] = "holdsFor"~"("~"trust"~"("~id~","~topic~")"~"="~"true"~","~intervals~")"~"."^^{
    case "holdsFor"~"("~"trust"~"("~id~","~topic~")"~"="~"true"~","~intervals~")"~"." => new TrustAtom(id,topic,intervals)
  }

  def _parse(parser: Parser[Atom], atom: String): Option[Atom] = {
    parseAll(parser, atom) match {
      case Success(result, _) => Some(result)
      case f                  => None
    }
  }


}

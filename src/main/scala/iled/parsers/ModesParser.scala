package iled.parsers

import scala.util.parsing.combinator.JavaTokenParsers
import iled.structures._
import iled.structures.Modes._
import iled.structures.Exceptions._

class ModesParser extends ClausalLogicParser {
   def mh: Parser[String] = "modeh" ^^ { x => x }
   def mb: Parser[String] = "modeb" ^^ { x => x }
   def ep: Parser[String] = "examplePattern" ^^ { x => x }
   def posplmrk: Parser[PosPlmrk] = "+" ~ lowerCaseIdent ^^ { case "+" ~ x => PosPlmrk(x) }
   def negplmrk: Parser[NegPlmrk] = "-" ~ lowerCaseIdent ^^ { case "-" ~ x => NegPlmrk(x) }
   def constplmrk: Parser[ConstPlmrk] = "#" ~ lowerCaseIdent ^^ { case "#" ~ x => ConstPlmrk(x) }
   def placemarker: Parser[Expression] = (posplmrk | negplmrk | constplmrk) ^^ { x => x }
   def inner: Parser[List[Expression]] = "(" ~> repsep(modeAtom | placemarker, ",") <~ ")"
   //def modeAtom: Parser[ModeAtom] = lowerCaseIdent ~ inner ^^ { case x ~ y => new ModeAtom(x.toString, y) }

   def modeAtom: Parser[ModeAtom] =
      (  naf ~ lowerCaseIdent ~ inner ^^ { case not ~ x ~ y => new ModeAtom(functor = x.toString, args = y, isNAF = true) }
       | lowerCaseIdent ~ inner ^^ { case x ~ y => new ModeAtom(functor = x.toString, args = y, isNAF = false) }  )

   def modeh: Parser[ModeAtom] = mh ~ "(" ~ modeAtom ~ (")"|").") ^^ { case mh ~ "(" ~ m ~ (")"|").") => m }
   def modeb: Parser[ModeAtom] = mb ~ "(" ~ modeAtom ~ (")"|").") ^^ { case mb ~ "(" ~ m ~ (")"|").") => m }
   def mode: Parser[ModeAtom] = modeh | modeb
   def exmplPattern: Parser[ModeAtom] = ep ~ "(" ~ modeAtom ~ (")"|").") ^^ { case ep ~ "(" ~ m ~ (")"|").") => m }

   def parseModes(parser: Parser[ModeAtom], expression: String): Option[ModeAtom] = {
      parseAll(parser, expression) match {
         case Success(x, _) => Some(x)
         case Failure(msg, _) =>
            println("FAILURE: " + msg)
            println(expression)
            None
         case Error(msg, _) => println("ERROR: " + msg); None
         //case _ => None
      }
   }

   def getParseResult(x: Option[ModeAtom]): ModeAtom = x match {
      case Some(y) => y
      case _       => throw new MyParsingException
   }
}

package parsers

class ASPResultsParser extends ClausalLogicParser {
   
   def aspResult: Parser[List[String]] = repsep(literal, "") ^^ { case x => for (y <- x) yield y.tostring }
   
   def parseASP(parser: Parser[Any], expression: String): Option[Any] = {
      parseAll(parser, expression) match {
         case Success(result, _) => Some(result) 
         case Failure(msg, _) =>
            println("FAILURE: " + msg); None
         case Error(msg, _) => println("ERROR: " + msg); None
      }
   }
   
   def parsed(x: Option[Any]): Boolean = x match {
      case Some(y) => true
      case _       => false
   }
   
   def getResult(x: Option[Any]): Any = x match {
      case Some(y) => y
      case _       => false
   }
   
}
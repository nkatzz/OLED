/**
 * @author Nikos Katzouris
 *
 */




package logic

trait Expression {
  
   def tostring: String = ""
      
   def tostringQuote: String = ""
      
   def _type: String = ""
      
   def name: String = ""

   def isVariabe = this match {
      case v: Variable => true
      case _ => false
   }

   def isConstant = this match {
      case c: Constant => true
      case _ => false
   }

}
package logic

/** A variable is any term that starts with an upper-case letter */

case class Variable(override val name: String, inOrOutVar: String = "", override val _type: String = "") extends Expression {
   require(name.toCharArray()(0).isUpper) // else throws an IllegalArgumentException
   override def tostring = name
   override def tostringQuote = if (inOrOutVar == "-" || inOrOutVar == "#") "\"" + name + "\"" else name
   def asLiteral = Literal(functor = name)
}
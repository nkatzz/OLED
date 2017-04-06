package logic

/** A constant is any flat term that starts with a lower-case letter. */

case class Constant(override val name: String, plmrk: String = "", override val _type: String = "") extends Expression {
  require(!name.toCharArray()(0).isUpper) // else throws an IllegalArgumentException
  override def tostring = name
  override def tostringQuote = if (plmrk == "-" || plmrk == "#") "\"" + name + "\"" else name
  def asLiteral = Literal(functor = name)
}
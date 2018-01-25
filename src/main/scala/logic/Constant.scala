package logic

case class Constant(override val name: String, plmrk: String = "", override val _type: String = "") extends Expression {

  // No use. Allowing upper-case constants helps in MLN <--> ASL
  //require(!name.toCharArray()(0).isUpper) // else throws an IllegalArgumentException
  override def tostring = name

  override def tostringQuote = if (plmrk == "-" || plmrk == "#") "\"" + name + "\"" else name

  def asLiteral = Literal(functor = name)
}
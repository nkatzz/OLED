package all.structures

/**
    * A constant is any flat term that starts with a lower-case letter.
    *
    * @param name the constant symbol
    * @param _type the (optional) type of the constant
    * @param plmrk "+", "-", "#" indicating whether this constant corresponds to a input output or ground placemarker.
    * @overrides val _type from Expression
    */

   case class Constant(override val name: String, plmrk: String = "", override val _type: String = "") extends Expression {
      require(!name.toCharArray()(0).isUpper) // else throws an IllegalArgumentException
      override def tostring = name
      override def tostringQuote = if (plmrk == "-" || plmrk == "#") "\"" + name + "\"" else name
      def asLiteral = Literal(functor = name)
   }
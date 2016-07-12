package all.structures

/**
    * A variable is any term that starts with an upper-case letter
    *
    * @param name @tparam String the variable symbol
    * @param inOrOutVar @tparam String either '+' or '-' indicating input or output variable, as indicated by mode declarations.
    * The default parameter is an empty string to allow the constructor to work in cases where mode declarations are not present.
    * @param _type @tparam String the (optional) type of the variable
    * @overrides val _type from Expression
    */

   case class Variable(override val name: String, inOrOutVar: String = "", override val _type: String = "") extends Expression {
      require(name.toCharArray()(0).isUpper) // else throws an IllegalArgumentException
      override def tostring = name
      override def tostringQuote = if (inOrOutVar == "-" || inOrOutVar == "#") "\"" + name + "\"" else name
      def asLiteral = Literal(functor = name)
   }
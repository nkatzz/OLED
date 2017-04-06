package logic

import logic.Exceptions._


object Modes {
  


   case class PosPlmrk(override val _type: String) extends Expression {
      override val tostring = "+" + _type
      override def tostringQuote = this.tostring
   }
   case class NegPlmrk(override val _type: String) extends Expression {
      override val tostring = "-" + _type
      override def tostringQuote = this.tostring
   }
   case class ConstPlmrk(override val _type: String) extends Expression {
      override val tostring = "#" + _type
      override def tostringQuote = this.tostring
   }
   


   case class ModeAtom(functor: String, args: List[Expression], isNAF:Boolean = false) extends Expression {

      val arity = this.args.length

      val isEmpty = args match {
         case List() => true
         case _      => false
      }

      override val tostring: String = args match {
         case List() => if (isNAF) s"not $functor" else functor
         case _      => if (isNAF) s"not $functor" else functor + "(" + (for (a <- args) yield a.tostring).mkString(",") + ")"
      }

      lazy val negated = ModeAtom(functor = this.functor, args = this.args, isNAF = true)

      lazy val notNnegated = ModeAtom(functor = this.functor, args = this.args, isNAF = false)

      /**
       * @return a string representation of the mode declaration. This method is supposed to be called on a
       * variabilized version of the mode declaration, and it surrounds with double quotes
       * variables that correspond to output and ground placemarkers. For instance, assume the mode atom
       *
       * modeb(p(+type1,-type2,#type3))
       *
       * and its variabilized version
       *
       * p(X,Y,Z)
       *
       * The result of applying this method on the above is
       *
       * p(X,"Y","Z"). These atoms are passed to the ASP solver, in order to generate query atoms, for the
       * construction of the body of a Kernel Set clause. The quoted variables are treated as constants by the
       * solver, which generates instances by grounding only the variables that correspond to input terms.
       * The quotes are removed by post-processing each atom in an answer set, thus obtaining a query atom (i.e.
       * an atom of the form p(2,Y,Z) from the above). This is a query atom, which is subsequently used to
       * generate groundings of the atom that bind only Y,Z vars, keeping the input term intact.
       *
       */

      override def tostringQuote: String = args match {
         case List() => functor
         case _      => functor + "(" + (for (a <- args) yield a.tostringQuote).mkString(",") + ")"
      }

      /**
       * Variabilizes a mode declaration atom, i.e. it replaces all in-out-ground placemarkers with fresh variables.
       * The variabilized mode declarations are used in the construction of bottom clauses, in order to generate ground
       * instances of mode declarations atoms, by replacing variables by constants found in the data.
       *
       * returns a variabilized Literal. It's variables are annotated as +/-/# and it also carries a List[string] with the
       * typing predicates for it's variables.
       *
       */

      def varbed: Literal = {
         val lit = Literal(functor = this.functor)
         val (varbed, ttypes, _) = variabilize(List(Literal(functor = this.functor)), this.args, List(), 0)
         Literal(functor = varbed.head.functor, terms = varbed.head.terms, isNAF = this.isNAF, typePreds = ttypes, modeAtom = this)

      }

      /**
       *
       * This method does all the work of the variabilation.
       *
       * @param accum an accumulator that collects competed (variabilized) compound sub-terms.
       * @param remaining a list containing all remaining sub-terms that should be variabilized.
       * @param ttypes a list collecting typing predicates for the generated variables, e.g. person(X1), time(X100)
       * @param counter a counter that is incremented by 1 each time a new varaible is generated. The name a new variable is
       * simply "X"+currentCounterValue.
       */

      private def variabilize(accum: List[Literal], remaining: List[Expression],
                              ttypes: List[String], counter: Int): (List[Literal], List[String], Int) = {
         def f(x: Expression, sign: String, tail: List[Expression]) = {
            val cur = accum match {
               case Nil => Literal(functor = this.functor)
               case _   => accum.last
            }
            // We are variabilizing everything (it's modes variabilization) so replace all with a new Var.
            val update = Literal(functor = cur.functor,
               terms = cur.terms :+ Variable("X" + counter, sign, x._type), isNAF = false)
            this.variabilize(accum.tail :+ update, tail, ttypes :+ x._type + "(X" + counter + ")", counter + 1)
         }
         remaining match {
            case head :: tail => head match {
               case x: PosPlmrk   => f(x, "+", tail)
               case x: NegPlmrk   => f(x, "-", tail)
               case x: ConstPlmrk => f(x, "#", tail)
               case x: ModeAtom =>
                  val (varbed, newTypes, newCount) =
                     this.variabilize(List(Literal(functor = x.functor)), x.args, List(), counter)
                  val pop = accum.last
                  this.variabilize(List(Literal(functor = pop.functor, terms = pop.terms ::: varbed)),
                     tail, ttypes ::: newTypes, newCount)
               case _ =>
                  throw new LogicException("Variabilizing Mode Declaration " +
                    this.tostring + ": Found unexpected type")
            }
            case Nil =>
               val pop = accum.last
               (accum.tail :+ Literal(functor = pop.functor, terms = pop.terms), ttypes, counter)
         }
      }

      /**
       * Recieves a tuple of the form ('in','out','ground'), where each coordinate in the tuple is List[Expression]
       * of constants marked as input, output or ground placemarkerks. From this input, this method constructs
       * all istances of the current mode declaration atom 'm', generated as follows:
       * -- Each input placemarker in 'm' is replaced by a term in 'in'
       * -- Each ground placemarker in 'm' is replaced by a variable.
       * -- Each output placemarker in 'm' is replaced by a variable.
       *
       * These constitute the query atoms, used to generate Kernel Set body atoms.
       *
       * @example
       *
       * Assume that the current mode atom is modeb(p(+entity1,-emtity2,#entity3)) and the input is
       * (List("e1","e2"),List("e3"),List("e4","e5")). The result of this method is the list of atoms:
       *
       * List(p(e1,Y,Z),p(e2,Y,Z)).
       *
       * This method is to be called uppon a variabilized version of the mode atom, and replace the variables that
       * correspond to input terms, with terms from 'in'
       *
       *
       */

      def generateQueryAtoms(input: (List[Expression], List[Expression], List[Expression])) = {

      }

   }
   
   
}
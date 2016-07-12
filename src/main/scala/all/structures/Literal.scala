package all.structures

import scala.collection.mutable.ListBuffer
import scala.util.control.Breaks
import all.structures.Modes._
import all.structures.Exceptions._
import all.core.Core
import all.parsers.ModesParser

/**
 * Companion object
 */

object Literal extends ModesParser {

   val empty = Literal()

   /* Parse a string into a literal, An oprional mode atom may be provided. */

   def toLiteral(lit: String, mode: String = ""): Literal = {
      mode match {
         case "" => getParseResult(parse(literal, lit)).asInstanceOf[Literal]
         case _ =>
            val l = getParseResult(parse(literal, lit)).asInstanceOf[Literal]
            val m = getModeAtom(mode)
            Literal(functor = l.functor, terms = l.terms, isNAF = l.isNAF, modeAtom = m)
      }
   }

    def toLiteral1(lit: String, mode: ModeAtom = ModeAtom("", List())): Literal = {
       val l = getParseResult(parse(literal, lit)).asInstanceOf[Literal]
       val out = Literal(functor = l.functor, terms = l.terms, isNAF = l.isNAF, modeAtom = mode)
       out
    }
      
    def toLiteral2(lit: Literal, mode: ModeAtom = ModeAtom("", List())): Literal = {
       val out = mode match {
          case ModeAtom("", List(), false) => lit
          case _ => Literal(functor = lit.functor, terms = lit.terms, isNAF = lit.isNAF, modeAtom = mode)
        }
      out
    }

   def types(l: String, mode: ModeAtom) = {


     def terms(lit: Literal): List[Expression] = {
       val (in, out, grnd) = lit.getPlmrkTerms
       val v = in ++ out ++ grnd
       v match {
         case Nil =>
           val mode = lit.matchingMode
           if (!mode.isEmpty) {
             val l = Literal(functor = lit.functor, terms = lit.terms,
               isNAF = true, modeAtom = mode, typePreds = lit.typePreds)
             l.variables
           } else { Nil }
         case _ => v
       }
     }


     val lit = toLiteral1(l,mode)
     val termTypes = terms(lit) map {x => s"${x._type}(${x.tostring})"}
     termTypes.distinct.mkString(",")
   }

   def getModeHAtom(atom: String): ModeAtom = getParseResult(parseModes(modeh, atom))

   def getModeAtom(atom: String): ModeAtom = getParseResult(parseModes(mode, atom))

   def getModeBAtom(atom: String): ModeAtom = getParseResult(parseModes(modeb, atom))

}

/**
 * A literal is a compound term of the form p(x1,...xn), possibly preceded with 'not' ( 'not p(x1,...xn)' ),
 * in which case it is a negated literal. 'p' is the functor of the literal and xi's are its terms. Each xi
 *  is either a variable, a constant or a non-negated literal.
 *
 * @param functor the predicate/function symbol of the literal.
 * @param terms the inner terms of the literal. This is a var so that it can be updated, by populating the term objects
 * by indicators on whether they correspond to input-output vars or constants, a process that takes place during the
 * construction of the Literal object, by extracting relevant information from the accompanying modeAtom (if one is present
 * with the input). I don't know if this is the best way to do it (having vars), but its seems messy to create a companion object
 * for a case class (as this one).
 * @param isNAF true or false depending on whether the literal is negated or not.
 * @param an (optional) mode declaration pattern. This is pattern according to which the literal has been generated
 * (during bottom clause construction). The mode declaration is used  to annotate the variables and constants of the
 * literal with additional information (types/sorts of constants/variables, input or output variables), which is used in the
 * process of variabilizing the clause in which this literal belongs.
 * @param modeAtom an (optional) mode atom that matches this literal.
 * @param typePreds an (optional) list of typing predicates, extracted from a matching mode declaration,
 *  for the literal's variables and constants
 *
 */

case class Literal(functor: String = "", terms: List[Expression] = Nil, isNAF: Boolean = false,
                   modeAtom: ModeAtom = ModeAtom("", Nil), typePreds: List[String] = Nil) extends Expression {

   require(if (functor != "") !functor.toCharArray()(0).isUpper else true)


   lazy val arity = terms.length

   lazy val negated = Literal(functor = this.functor, terms = this.terms, isNAF = true, modeAtom = this.modeAtom, typePreds = this.typePreds)

   lazy val nonNegated = Literal(functor = this.functor, terms = this.terms, isNAF = false, modeAtom = this.modeAtom, typePreds = this.typePreds)

   def negateThis =
      if (this.isNAF) {
         Literal(functor = this.functor, terms = this.terms, isNAF = false, modeAtom = this.modeAtom, typePreds = this.typePreds)
      } else {
         Literal(functor = this.functor, terms = this.terms, isNAF = true, modeAtom = this.modeAtom, typePreds = this.typePreds)
      }

   def asPosLiteral = {
      if (!this.isNAF) PosLiteral(this.functor, this.terms, this.isNAF, this.modeAtom, this.typePreds)
      else throw new LogicException(s"Found negated literal casted as postive literal: ${this.tostring}}")
   }


   override val tostring: String = terms match {
      case List() => functor
      case _ =>
         val prefix = if (isNAF) s"not $functor" else functor;
         prefix + "(" + (for (
            a <- terms; x = a match {
               case x @ (_: Constant | _: Variable | _: Literal | _: PosLiteral) => x
               case _ => throw new LogicException("Unxpected type of inner term while parsing Literal.")
            }
         ) yield x.tostring).mkString(",") + ")"
   }

   override def tostringQuote: String = terms match {
      case List() => functor
      case _ =>
         val prefix = if (isNAF) s"not $functor" else functor;
         prefix + "(" + (for (
            a <- terms; x = a match {
               case x @ (_: Constant | _: Variable | _: Literal | _: PosLiteral) => x
               case _ => throw new LogicException("Unxpected type of inner term while parsing Literal.")
            }
         ) yield x.tostringQuote).mkString(",") + ")"
   }

   /**
    * @return a mode declarartion atom that matches this literal.
    * If none is found, returns the empty mode atom ( ModeAtom("",List() )
    */

   lazy val matchingMode: ModeAtom = {
      var out: ModeAtom = ModeAtom("", List())
      this.modeAtom match {
         case ModeAtom("", Nil, false) =>
            val loop = new Breaks;
            loop.breakable {
               for (x <- Core.modehs ::: Core.modebs) {
                  val test = if (this.functor != x.functor || this.arity != x.arity) false
                  else matchesMode(this.terms zip x.args)
                  if (test) {
                     out = x
                     loop.break()
                  }
               }
            }
         case _ => this.modeAtom

      }
      out
   }

   def matchesMode(remaining: List[(Expression, Expression)]): Boolean = {
      remaining match {
         case head :: tail => head match {
            case (n: Constant, m @ (_: PosPlmrk | _: NegPlmrk | _: ConstPlmrk)) => matchesMode(tail)
            case (n: Variable, m @ (_: PosPlmrk | _: NegPlmrk))                 => matchesMode(tail)
            case (n: Variable, m: ConstPlmrk) =>
               throw new LogicException("Found a variabilized term that corresponds to a grplmrk.")
            case (n: Literal, m: ModeAtom) =>
               if (n.functor != m.functor || n.arity != m.arity) false else matchesMode(n.terms zip m.args)
            case _ => throw new LogicException("Getting matching mode: Found unexpected term pairing.")
         }
         case Nil => true
      }
   }

   /**
    * Variabilizes a literal. If a matching mode declaration atom is passed with the input, then the literal is variabilzed according
    * to the directives provided by that atom. Else (if no mode atom is present), each constant of the literal is replaced by a new
    * variable (TODO: this is not implemented yet, see comments below). The variabilization of a literal is part of the process of
    * the variabilization of a clause. In this process, constants of the literal that are present in other literals of the clause,
    * which have already been variabilized, should be replaced by the same variable that has already been used for these constants.
    *
    * @param previousMap a map containing previous bindings of constants to variables.
    * @param accum  an accumulator that collects competed (variabilized) compound sub-terms.
    * @param remaining  a list containing all sub-terms remaining to be variabilized.
    * @param ttypes  a list collecting typing predicates for the generated variables,
    *  e.g. person(X1), time(X100) etc.
    * @param counter  a counter that is incremented by 1 each time a new variable is generated.
    * The name a new variable is simply "X"+currentCounterValue.
    * @param runningMode  a flag indicating a "mode" (purpose) for which this method is called. Default is
    * "", in which case the literal is simply variabilized. If mode = "extract-mode-terms", then this method
    * is called on a ground literal and it processes the corresponding mode declaration, extracting a tuple
    * (in,out,grnd) representing the terms of the ground atom that correspond to input, output or ground
    * placemarkers respectively
    */

   def varbed = {
      this.modeAtom match {
         /*
             * 
             * TODO: This should work in any case (exception needed here): Simply, if no mode atom is passed with the input, 
             * replace every constant in the literal. by a fresh variable. So, this should do exactly what modes' variabilizations does.
             * It's easy to do, but I have to think a way to do it without duplicating the code here. One way is to have ModeAtom inherit
             * from Literal and override the variabilization method, but then these classes cannot be case classes and I'll have
             * to write my own apply and unapply methods (I guess thats no big deal, but I don't have the time now).
             *
             */

         case ModeAtom("", Nil, false) =>
            throw new LogicException("Cannot variabilize a literal without a corresponing mode declaration")
         case _ =>
            val (varbed, ttypes, constVarMap, varCounter) =
               variabilize(
                  List(Literal(functor = this.functor,
                     terms = List(), isNAF = this.isNAF)),
                  this.terms zip this.modeAtom.args,
                  scala.collection.mutable.Map[Expression, Expression](), List(), 0
               )
            val l = Literal(functor = varbed(0).functor,
               terms = varbed(0).terms,
               isNAF = false, typePreds = ttypes,
               modeAtom = this.modeAtom)
            (l, ttypes, constVarMap, varCounter)
      }
   }

   def variabilize(accum: List[Literal], remaining: List[(Expression, Expression)],
                   previousMap: scala.collection.mutable.Map[Expression, Expression],
                   ttypes: List[String], counter: Int, runningMode: String = ""): (List[Literal], List[String], scala.collection.mutable.Map[Expression, Expression], Int) = {

      // x is a tuple (x1,x2), where x1 is a literal's constant and x2 is it's type as specified by the modeAtom 
      def f(x: (Expression, String), sign: String, tail: List[(Expression, Expression)],
            map: scala.collection.mutable.Map[Expression, Expression]) = {

         val cur = accum match {
            case Nil => Literal(functor = this.functor, terms = List(), isNAF = this.isNAF)
            case _   => accum.last
         }

         val (litUpdate, typesUpdate, varCountUpdate) = sign match {
            case "#" =>
               // a term corresponding to constant placemarker remains intact
               (Literal(functor = cur.functor, terms = cur.terms :+ x._1, isNAF = cur.isNAF), ttypes, counter)
            case _ =>
               // if the constant has been variabilized previousely, use the same var.
               if (map.keySet.contains(x._1)) {
                  (Literal(functor = cur.functor, terms = cur.terms :+ map(x._1), isNAF = cur.isNAF), ttypes, counter)
               } else {
                  // else, use a new one
                  val newVar = Variable("X" + counter, "+", x._2)
                  map += (x._1 -> newVar)
                  (Literal(functor = cur.functor, terms = cur.terms :+ newVar, isNAF = cur.isNAF),
                    ttypes :+ x._2 + "(X" + counter + ")", counter + 1)
               }
         }
         this.variabilize(accum.tail :+ litUpdate, tail, map, typesUpdate, varCountUpdate)
      }
      remaining match {
         case head :: tail => head match {
            case (x: Constant, y: PosPlmrk)   => f((x, y._type), "+", tail, previousMap)
            case (x: Constant, y: NegPlmrk)   => f((x, y._type), "-", tail, previousMap)
            case (x: Constant, y: ConstPlmrk) => f((x, y._type), "#", tail, previousMap)
            case (x: Literal, y: ModeAtom) =>
               val (varbed, newTypes, newMap, newCount) =
                  this.variabilize(List(Literal(x.functor)), x.terms zip y.args, previousMap, List(), counter)
               val pop = accum.last
               this.variabilize(List(Literal(functor = pop.functor, terms = pop.terms ::: varbed, isNAF = pop.isNAF)),
                  tail, newMap, ttypes ::: newTypes, newCount)
            case _ => throw new LogicException("Variabilizing Literal " + this.tostring + ": Found unexpected type")
         }
         case Nil =>
            val pop = accum.last
            (accum.tail :+
              Literal(functor = pop.functor, terms = pop.terms, isNAF = pop.isNAF), ttypes, previousMap, counter)
      }
   }

   /**
    * Extracts the terms of the literal marked as input-output or ground terms.
    *
    * @param in  an accumulator for input terms
    * @param out  an accumulator for output terms
    * @param grnd  an accumulator for ground terms
    * @param remaining  the (zipped) terms of the literal and the mode atom
    * that remain to be checked
    * @return a tuple (in,out,ground) carrying the marked terms
    */

   def getPlmrkTerms: (List[Expression], List[Expression], List[Expression]) =
      getPlmrkTerms(List(), List(), List(), this.terms zip this.modeAtom.args)

   def getPlmrkTerms(in: List[Expression], out: List[Expression], grnd: List[Expression],
                     remaining: List[(Expression, Expression)]): (List[Expression], List[Expression], List[Expression]) = {
      remaining match {
         case head :: tail => head match {
            case (x: Constant, y: PosPlmrk) =>
               getPlmrkTerms(in ::: List(Constant(x.name, "+", y._type)), out, grnd, tail)
            case (x: Constant, y: NegPlmrk) =>
               getPlmrkTerms(in, out ::: List(Constant(x.name, "-", y._type)), grnd, tail)
            case (x: Constant, y: ConstPlmrk) =>
               getPlmrkTerms(in, out, grnd ::: List(Constant(x.name, "#", y._type)), tail)
            case (x: Variable, y: PosPlmrk) =>
               getPlmrkTerms(in ::: List(Variable(x.name, "+", y._type)), out, grnd, tail)
            case (x: Variable, y: NegPlmrk) =>
               getPlmrkTerms(in, out ::: List(Variable(x.name, "-", y._type)), grnd, tail)
            case (x: Variable, y: ConstPlmrk) =>
               getPlmrkTerms(in, out, grnd ::: List(Variable(x.name, "#", y._type)), tail)
            case (x: Literal, y: ModeAtom) =>
               val (newin, newout, newconst) =
                  getPlmrkTerms(in, out, grnd, x.terms zip y.args)
               getPlmrkTerms(newin, newout, newconst, tail)
            case _ => throw new LogicException(this.tostring + ": Unexpected type.")
         }
         case Nil =>
            (in, out, grnd)
      }
   }

   /**
    * Skolemize this literal (replace all variables with skolem constants)
    */

   def skolemize(skolems: Map[String, String], accum: ListBuffer[Expression] = ListBuffer[Expression]()): ListBuffer[Expression] = {
      var temp = new ListBuffer[Expression]
      def keyExists = (x: Any) => if (skolems.keySet.exists(_ == x)) true else false
      def append = (x: Expression) => temp += x
      for (x <- this.terms) x match {
         case y: Variable =>
            val name = y.name
            if (keyExists(name)) append(Constant(skolems(name)))
            else
               throw new LogicException("Skolemise: Found a variable without corresponding skolem constant.")
         case y: Constant =>
            val name = y.name
            if (keyExists(name)) append(Constant(skolems(name)))
            else
               throw new LogicException("Skolemise: Found a constant without corresponding skolem constant.")
         case y: Literal =>
            val l = y
            val m = l.skolemize(skolems, temp)
            val toLit = Literal(functor = l.functor, terms = m.toList, isNAF = l.isNAF)
            temp += toLit
         case _ => throw new LogicException("Skolemise: Unexpected type.")
      }
      temp
   }

   def getSkolemConsts(skolems: ListBuffer[(String, String)], counter: Int): (ListBuffer[(String, String)], Int) = {
      var c = counter; var s = skolems
      def f = (x: String, y: String) => if (!s.contains(x)) s += x -> y else s
      def g = (x: Int) => c += x
      for (x <- this.terms) x match {
         case y: Variable =>
            f(y.name, "skolem" + c); g(1)
         case y: Constant => f(y.name, y.name) // use the constant as a skolem constant
         case y: Literal =>
            val m = y.getSkolemConsts(s, c)
            s = m._1; c = m._2
         case _ => throw new LogicException("Skolemize: Unexpected type of inner term.")
      }
      (s, c)
   }

   /**
    * Replace all occurrences of thisExpr in this literal with thatExpr
 *
    * @param thisExpr
    * @param thatExpr
    * @return a Literal with all occurrences of thisExpr replaced by thatExpr
    *
    */

   def replace(thisExpr: Expression, thatExpr: Expression): Literal = {
      var temp = new ListBuffer[Expression]
      def append = (x: Expression) => if (x == thisExpr) temp += thatExpr else temp += x
      for (x <- this.terms) x match {
         case y: Variable => append(y)
         case y: Constant => append(y)
         case y: Literal =>
            if (y == thisExpr) temp += thatExpr
            else {
               val l = y
               val m = l.replace(thisExpr, thatExpr)
               temp += m
            }
         case _ => throw new LogicException("Replace, don't know what to do.")
      }
      Literal(functor = this.functor, terms = temp.toList, isNAF = this.isNAF)
   }

   /**
    * @param map  a map of expressions
    * @return a Literal that results x with y in the current literal, for each x -> y found in map.
    */

   def replaceAll(map: Map[_ <: Expression, _ <: Expression]): Literal = map.foldLeft(this)((x, y) => x.replace(y._1, y._2))

   /**
    * Get all variables from this Literal
    */

   def getVars: ListBuffer[Variable] = {
      val vars = new ListBuffer[Variable]
      for (x <- this.terms) x match {
         case y: Variable => if (!vars.contains(y)) vars += y
         case y: Literal =>
            val z = y.getVars
            vars ++= z.toList.filter { v => !vars.contains(v) }
         case _ =>
      }
      vars
   }

   /**
    * Returns a list of typing predicates for the variables of the literal.
    * e.g. 'time(X)' if X is of type 'time'
    */

   def getTypePredicates: List[String] = {
      val f = (x: Expression) => x.asInstanceOf[Variable]
      val vars = this.variables
      val tpreds = for (x <- vars) yield f(x)._type + "(" + f(x).name + ")"
      tpreds
   }

   def getConstantsTypes: List[String] = {
      val f = (x: Expression) => x.asInstanceOf[Constant]
      val vars = this.constants
      val tpreds = for (x <- vars) yield f(x)._type + "(" + f(x).name + ")"
      tpreds
   }

   /**
    * Get all terms of this Literal that correspond to variables.
    */

   def getTermsThatCorrespondToVars: List[_ <: Expression] = {
      val mode = this.matchingMode
      getTermsThatCorrespondToVars(mode)
   }

   def variables: List[Expression] = {
      val (in, out, _) = this.getPlmrkTerms
      val v = in ++ out
      v match {
         case Nil =>
            val mode = this.matchingMode
            if (!mode.isEmpty) {
               val l = Literal(functor = this.functor, terms = this.terms,
                  isNAF = true, modeAtom = mode, typePreds = this.typePreds)
               l.variables
            } else { Nil }

         case _ => v filter (x => !x.isConstant)
      }
   }

   def constants: List[Expression] = {
      val (in, out, grnd) = this.getPlmrkTerms
      val v = in ++ out ++ grnd
      v match {
         case Nil =>
            val mode = this.matchingMode
            if (!mode.isEmpty) {
               val l = Literal(functor = this.functor, terms = this.terms,
                  isNAF = true, modeAtom = mode, typePreds = this.typePreds)
               l.constants
            } else { Nil }

         case _ => v filter (x => !x.isVariabe)
      }
   }


   def getTermsThatCorrespondToVars(mode: ModeAtom): List[_ <: Expression] = {
      val out = new ListBuffer[T forSome { type T <: Expression }]
      for (x <- this.terms zip mode.args) {
         x match {
            case (term, m @ (_: PosPlmrk | _: NegPlmrk)) => out += term
            case (x: Literal, y: ModeAtom) =>
               val inner = x.getTermsThatCorrespondToVars(y)
               out ++= inner
            case _ =>
         }
      }
      out.toList.distinct
   }

}

/**
 * This is a helper class for the representation of non-negated literals.
 */

object PosLiteral {
   val empty = PosLiteral()
}

case class PosLiteral(functor: String = "", terms: List[Expression] = Nil, isNAF: Boolean = false,
                      modeAtom: ModeAtom = ModeAtom("", Nil), typePreds: List[String] = Nil) extends Expression {
   def arity = terms.length
   def asLiteral = Literal(functor = functor, terms = terms,
      isNAF = false, modeAtom = modeAtom, typePreds = typePreds)
   override def tostring = this.asLiteral.tostring
   override def tostringQuote = this.asLiteral.tostringQuote

}

object AnswerSet {

   def UNSAT = new AnswerSet(List(Core.UNSAT))
   def empty = new AnswerSet(List[String]())
}

case class AnswerSet(val atoms: List[String]) {

   val isEmpty = atoms == List()

}
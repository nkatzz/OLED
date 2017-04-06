package utils.lookaheads

import logic.Exceptions.MyParsingException
import logic.Literal
import parsers.ClausalLogicParser
import scala.io.Source

/**
  * Created by nkatz on 11/30/16.
  */

object LookAheadUtils {

  /*
*
* A linking variable is a variable found in a linking predicate (e.g. in a comparison predicate) in a lookahead schema
* appearsInLiteral is the number of the literal in which it appears (first or second literal, where the first is the literal
* that is about to be added to the clause we're currently learning and the second one in the literal that already exists
* in the clause and we're linking the new one to it.). So appearsInLiteral is 1 or 2. Position in literal is the index of the
 * current linking variable in the variables list of the appearsInLiteral literal.
*
* */
  class LinkingVariable(val variable: logic.Variable, val appearsInLiteral: Int, val positionInLiteral: Int)
  class LinkingPredicate(val literal: Literal, val vs: List[LinkingVariable])
  class LinkingPredicatesGroup(val predicates: List[LinkingPredicate])

  class LookAheadSpecification(val lookAheadSpecification: String) extends LookAheadsParser{
    /*
     * An example pf a lookahead spcification is
     * lookahead( transaction(Whatever1,A1,Whatever2,T1) <-> transaction(Whatever3,A2,Whatever4,T2) : {before(T1,T2) | after(T1,T2)}, {greaterThan(A1,A2) | lessThan(A1,A2)} )
     */
    private val p = parse(lookAheadSpecification)
    private val linkedAtoms = p._1
    private val linkingPredGroups = p._2
    val litToBeAdded = linkedAtoms._1
    val litToLinkTo = linkedAtoms._2
    private def isVariableUncommon(v: logic.Variable) = {
      ! (litToBeAdded.getVars.contains(v) && litToLinkTo.getVars.contains(v))
    }
    private def variableExists(v: logic.Variable) = {
      litToBeAdded.getVars.contains(v) || litToLinkTo.getVars.contains(v)
    }
    /*
     * l is a literal from some LinkingPredicatesGroup. This method returns a tuple
     * with l in the first coordinate and a list of the variables that appear in l
     * in the form of LinkingVariable objects
     */
    private def getLinkingVars(l: Literal) = {
      val t = l.getVars.toList.map { v =>
        require(isVariableUncommon(v), s"Variable with name ${v.name} is common both in litToBeAdded and litToLinkTo")
        require(variableExists(v), s"Variable with name ${v.name} does not appear in neither one of litToBeAdded or litToLinkTo")
        val appearsInLiteral = if (litToBeAdded.getVars.contains(v)) 1 else 2
        val litItAppearsIn = if (litToBeAdded.getVars.contains(v)) litToBeAdded else litToLinkTo
        val positionInLiteral = litItAppearsIn.getVars.indexOf(v)
        new LinkingVariable(v, appearsInLiteral, positionInLiteral)
      }
      (l,t)
    }
    // That's the only thing that matters from this class. Everything that happens in this class aims at getting this list
    val actualLinkingGroups =
      linkingPredGroups.map(group => group.map( literal => getLinkingVars(literal))).
        map(z => z.map(p => new LinkingPredicate(p._1, p._2))).map(a => new LinkingPredicatesGroup(a))
  }


  class LookAheadsParser extends ClausalLogicParser {

    def linkPredsGroup: Parser[List[Literal]] = "{" ~> repsep(literal, "|") <~ "}"
    def linkPredsGroups: Parser[List[List[Literal]]] = repsep(linkPredsGroup, ",")
    def linkedLiterals: Parser[(Literal, Literal)] = literal ~ "<->" ~ literal ^^ {case x ~ "<->" ~ y => (x, y) }
    def specificationParser: Parser[((Literal, Literal),List[List[Literal]])] =
      "lookahead" ~ "(" ~ linkedLiterals ~ ":" ~ linkPredsGroups ~ ")" ^^ {case "lookahead" ~ "(" ~ x ~ ":" ~ y ~ ")" => (x,y) }

    private def _parse(expression: String): Option[((Literal, Literal),List[List[Literal]])] = {
      parseAll(specificationParser, expression) match {
        case Success(result, _) => Some(result)
        case f                  => None
      }
    }

    private def getParseResult(x: Option[((Literal, Literal),List[List[Literal]])]): ((Literal, Literal),List[List[Literal]]) = x match {
      case Some(y) => y
      case _       => throw new MyParsingException(x.toString)
    }

    def parse(expression: String): ((Literal, Literal),List[List[Literal]]) = {
      getParseResult(_parse(expression))
    }
  }

  object Run extends App {


    val LOOK_AHEADS_TEST = {
      val f = Source.fromFile("/home/nkatz/dev/ILED/datasets/Fraud/modes").getLines.toList.filter(line => line.startsWith("lookahead"))
      if (f.nonEmpty) f.map( x => new LookAheadSpecification(x) ) else Nil
    }

    val stop = "stop"


    val p = new LookAheadsParser
    p.parse("lookahead( transaction(Whatever,A1,Whatever,T1) <-> transaction(Whatever,A2,Whatever,T2) : {before(T1,T2) | after(T1,T2)}, {greaterThan(A1,A2) | lessThan(A1,A2)} )")

  }



}

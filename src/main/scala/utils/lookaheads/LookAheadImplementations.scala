/*
 * Copyright (C) 2016  Nikos Katzouris
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

package utils.lookaheads

import logic.{Clause, Literal}
import utils.lookaheads.LookAheadUtils._

/**
  * Created by nkatz on 11/30/16.
  */
object LookAheadImplementations {

  def lookAhead_<-(litToAdd: Literal, currentClause: Clause, bottomClause: Clause, spec: LookAheadSpecification) = {
    val lastLitThatMatches = getLastLiteralThatMatches(litToAdd, currentClause, bottomClause, spec)
    val atoms = spec.actualLinkingGroups.flatMap(group => getLinkingAtoms(litToAdd, lastLitThatMatches, group, bottomClause))
    atoms
  }

  private def getLastLiteralThatMatches(litToAdd: Literal, currentClause: Clause, bottomClause: Clause, spec: LookAheadSpecification) = {

    /*
    if (matches(spec.litToLinkTo,litToAdd)) {
    currentClause.head.asLiteral
  } else {
    currentClause.toLiteralList.reverse.find(p => matches(p, spec.litToLinkTo) ).getOrElse {
      throw new RuntimeException(s"Could not find any literal in\n ${currentClause.tostring}\nthat " +
        s"matches the schema of\n${spec.litToLinkTo.tostring}. The lookahead schema is:\n${spec.lookAheadSpecification}")
    }
  }
 */

    currentClause.toLiteralList.reverse.
      find(p => matches(p, spec.litToLinkTo) ).
      getOrElse {
      throw new RuntimeException(s"Could not find any literal in\n ${currentClause.tostring}\nthat " +
        s"matches the schema of\n${spec.litToLinkTo.tostring}. The lookahead schema is:\n${spec.lookAheadSpecification}")
      }
  }





  private def getLinkingAtoms(lit1: Literal, lit2: Literal, l: LinkingPredicatesGroup, bottomClause: Clause) = {

    val map = Map(1 -> lit1, 2 -> lit2)

    val linkingAtoms = l.predicates.map { x =>
      val linkingAtom = x.literal
      val actualVars = x.vs.map(z => map(z.appearsInLiteral).getVars(z.positionInLiteral) )
      val dummyVars = linkingAtom.getVars.toList
      val paired = if (dummyVars.length == actualVars.length) dummyVars zip actualVars else throw new RuntimeException(s"dummy and actual vars cannot" +
        s" be paired for literal $linkingAtom. Dummy vars are ${dummyVars.map(_.tostring).mkString(" ")} and actual vars are ${actualVars.map(_.tostring).mkString(" ")}")
      linkingAtom.replaceAll(paired.toMap)
    }

    val actualLinkingAtoms = linkingAtoms.filter(x => bottomClause.toStrList.contains(x.tostring))

    // For the fraud application exactly one of the actualLinkingAtoms must be found in the bottom clause
    // because the atoms in each linking group are contradicting. I think that this is the generic way this
    // should work. In any case, that's how it's currently implemented. There is an exception however, in the
    // case where we are adding a literal that refers to the same time as in the head (then we have no before/1
    // or after/2.) So I won't throw the exception below

    /*
    if (actualLinkingAtoms.length != 1) throw new RuntimeException("More than one atoms from the same linking group are found in the bottom clause." +
      s"The bottom clause is\n${bottomClause.tostring}. Linking atoms found: ${actualLinkingAtoms.map(_.tostring).mkString(" ")}")
    */

    actualLinkingAtoms
  }

  def hasLookAhead(lit: Literal, lookaheadSpecs: List[LookAheadSpecification]) = {
    lookaheadSpecs.exists(p => p.litToBeAdded.functor == lit.functor && p.litToBeAdded.arity == lit.arity)
  }

  def matches(x: Literal, y:Literal) = {
    x.functor == y.functor && x.arity == y.arity
  }

  def selectLookaheadSpec(litToAdd: Literal, c: Clause, bottomClause: Clause, lookaheadSpecs: List[LookAheadSpecification]): LookAheadSpecification = {
    val (headLookaheadSpec, bodyLookaheadSpecs) = lookaheadSpecs.foldLeft(List[LookAheadSpecification](),List[LookAheadSpecification]()) { (x,y) =>
      val headLink = x._1
      val bodyLink = x._2
      if (matches(bottomClause.head.asLiteral, y.litToLinkTo)) {
        (headLink :+ y, bodyLink)
      } else {
        if (bottomClause.body.exists(p => matches(p, y.litToLinkTo))) {
          (headLink, bodyLink :+ y)
        } else {
          throw new RuntimeException(s"Lookaheds Error: There is no literal in\n${c.tostring} matching the specified linking atom ${y.litToLinkTo.tostring}")
        }
      }
    }
    // just a sanity check
    if (headLookaheadSpec.isEmpty) {
      throw new RuntimeException("No lookahead spec linking to the head. Add one")
    }
    if (headLookaheadSpec.length > 1) throw new RuntimeException("Something's wrong, I'm getting multiple head-linking lookaheads")
    bodyLookaheadSpecs.find(p => matches(p.litToLinkTo, litToAdd) && c.body.exists(z => matches(z,p.litToLinkTo))).getOrElse(headLookaheadSpec.head)
  }

}

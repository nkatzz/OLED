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

package utils

import logic.{Clause, Literal}

/**
  * Created by nkatz on 11/7/16.
  */

object LookAheads {

  /*
  * Whenever a new lookahead policy is defined (and declared in a mode declarations file),
  * the policy should be implemented in the LookAheadImplementations object and a mapping
  * between the policy's name and its implementation should be defined here.
  * */
  val policyMap = Map("appearsEarlier" -> LookAheadImplementations.appearsEarlier_<-)


  class LookAheadSpecification(val lookAheadDefinition: String) {
    /*
     * A lookahead specification is a declaration of the form:
     *
     * lookahead( transaction(X,Y,T), before(T,T1), appearsEarlier(2) )
     *
     * In this definition:
     * - transaction(X,Y,T) is the current atom
     * - before(T,T1) is the lookahead atom
     * - appearsEarlier(2) is the policy atom
     *
     * The lookahead specification says that when a version of a current atom is to be
     * added to the clause, then it must be accompanied by a version of the lookahead atom.
     * Moreover these two atoms must share (have in common) their 3rd and 1rst variable respectively
     * (denoted by the same variable T in the respective positions in the atom signatures). Also, the
     * policy atom enforces additional constrains on the remaining variables of the lookahead atom.
     * For instance the policy atom above (appearsEarlier(2)) states that the second variable in a
     * lookahead atom must appear earlier in the clause in which the lookahead atom is about to be added.
     * Policies are implemented in the LookAheadImplementations object
     *
     */
    val parsed = Literal.parse(lookAheadDefinition)

    // the current atom
    val currentLiteralSignature = parsed.terms.head.asInstanceOf[Literal]

    // the lookahead
    val lookAheadLiteralSignature = parsed.terms(1).asInstanceOf[Literal]

    // the policy atom
    val policySignature = parsed.terms(2).asInstanceOf[Literal]

    // the linking (shared) variable between the current and the lookahead atom.
    // The variable itself is not important, we only need it to extract the positions
    // in the actual current and lookahead atoms that we'll encounter during learning,
    // which the linking variable should occupy.
    val targetLookAheadSharedVariable = {
      val vars = currentLiteralSignature.getVars
      vars.toSet.intersect(lookAheadLiteralSignature.getVars.toSet)
    }

    // fail if no shared variable is found
    require(targetLookAheadSharedVariable.nonEmpty, s"No shared variables between current and lookahead atoms inthe lookahead specification $lookAheadDefinition")

    // index of shared variable in a current atom
    val sharedVarIndex_in_CurrentAtom = currentLiteralSignature.getVars.indexOf(targetLookAheadSharedVariable.head)

    // index of shared variable in a lookahead atom
    val sharedVarIndex_in_LookAheadAtom = lookAheadLiteralSignature.getVars.indexOf(targetLookAheadSharedVariable.head)

    // index of linking variable in a lookahead atom
    val linkingVar_in_LookAheadAtom = policySignature.terms.head.name.toInt - 1

    def policy = policyMap(policySignature.functor)
  }

  object LookAheadImplementations {

    /*
     * All lookahead implementations should be declared here.
     */


    /*
     * This is an implementation of the "appearsEarlier_<-" lookahead policy. This policy is declared in the
     * mode declarations file as follows (predicate and variable names, arities etc are random, just for demonstration):
     *
     * lookahead( p(X,Y,T), q(T,T1), appearsEarlier(2) )
     *
     * The intended meaning of this declaration is: Whenever a p/3 literal is added to a clause r, a q/2 literal
     * should also be added. Both these literals are drawn from a bottom clause (bottomClause in the method's signature), while
     * policyLiteral in the method's signature is the literal appearsEarlier(2).
     * The relation between these two literals is that they should share a variable T. Also, the
     * remaining variable T1 of q/2 should appear in some literal r' that already appears in clause r.
     * This is specified by appearsEarlier(2), which means that the second variable of q/2 should "appearEarlier". The "<-" in the
     * name of the policy means that we search clause r for literal r' "from right to left" i.e. from the last body literal
     * to the head atom.
     *
     * This method returns the lookahead literal for which the linking variable appears "as closer" to the end of the clause as possible, i.e.
     * it appears in a literal closer to the end of the clause.
     *
     */
    val appearsEarlier_<- = (lit: Literal, specification: LookAheadSpecification, clause: Clause, bottomClause: Clause) => {

      val currentAtomSignature = specification.currentLiteralSignature
      if (lit.functor == currentAtomSignature.functor && lit.arity == currentAtomSignature.arity) {

        val sharedVarIndex_in_CurrentAtom = specification.sharedVarIndex_in_CurrentAtom
        val sharedVarIndex_in_LookAheadAtom = specification.sharedVarIndex_in_LookAheadAtom

        val sharedVar = lit.getVars(sharedVarIndex_in_CurrentAtom)

        // for the shared variable find in the bottom clause all literals that match the lookahead atom
        // signature and contain the shared variable in the proper position
        val candidateLookAheads =
          bottomClause.body.filter { p =>
            p.functor == specification.lookAheadLiteralSignature.functor &&
            p.arity == specification.lookAheadLiteralSignature.arity &&
            p.getVars(sharedVarIndex_in_LookAheadAtom).name == sharedVar.name &&
              clause.toLiteralList.filter(l => List("fraud","transaction").contains(l.functor)).exists(s => s.getVars.map(_.name).contains(p.getVars(specification.linkingVar_in_LookAheadAtom).name))
          }

        val f = (x: logic.Variable) => {
          // get max to get the literal closest to the end of the clause
          clause.toLiteralList.filter(l => List("fraud","transaction").contains(l.functor)).map(y => if (y.getVars.map(_.name).contains(x.name)) clause.toLiteralList.indexOf(y) + 1 else 0).max

        }
        if (candidateLookAheads.nonEmpty) {
          candidateLookAheads.map{q =>
            (q, f(q.getVars(specification.linkingVar_in_LookAheadAtom)))
          }.sortBy(z => z._2).last._1
        } else {
          Literal()
        }
      } else {
        Literal()
      }
    }


    def appearsEarlier_<-(lit: Literal, specification: String, clause: Clause, bottomClause: Clause) = {

      // This is a total hack, just to make it work. I'll see how to make it generic

      // A lookahead link looks like that:
      // --------------------------------------------------------------------------------------
      // transaction/4 -> { before/2, (4,1) }, { greaterThan/2, (2,1) }, { lessThan/2, (2,1) }
      // --------------------------------------------------------------------------------------
      // --------------------------------------------------------------------------------------

      /*
       * I'll use a specific example to see how it plays out. The atoms involved will be before/2, after/2, greaterThan/2, lessThan/2.
       * The way these atoms will be used in the example will be indicative of what I want to achieve. When that's done, I'll find a
       * way to make it generic, specify lookaheads in the mode declarations, parse them into objects for easy handling and search etc
       *
       * So, here comes the working example
       *
       * Assume that r is the clause that we are currently learning and
       * α = transaction(Card, A1, Cntry, T1) is the atom that we are about to add to r. Assume also that
       * β = transaction(Card, A2, Cntry, T2) is the last atom that appears in r.
       * if T1 is not the time variable that appears in head(r):
       *   STEP 1: Find either a before/2, or after/2 atom in the bottom clause that links T1 and T2.
       *   STEP 2: Find either a greaterThan/2, lessThan/2 or
       * else:
       *   simply add α to r
       */




      var foundLookAheadAtom = Literal()

    }





    /*
     * This policy is similar to "appearsEarlier_<-" but the search for a literal that contains a linking variable is done
     * "left to right", i.e. from the head to the last body literal
     */
    def appearsEarlier_->(lit: Literal, lookAheadLit: Literal, clause: Clause, searchSpace: Clause) = {

    }

    /*
     * This policy is similar to "appearsEarlier_<-" but the search for a literal that contains a linking variable is done randomly
     */
    def appearsEarlier_*(lit: Literal, lookAheadLit: Literal, clause: Clause, searchSpace: Clause) = {

    }



  }


}

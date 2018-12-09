package oled.winnow

import java.io.File

import app.runutils.{Globals, RunningOptions}
import logic.Examples.Example
import logic.{Literal, Theory}
import oled.winnow.AuxFuncs.marked
import utils.{ASP, Utils}

/**
  * Created by nkatz at 9/12/2018
  */
object PredictUpdateHandler {


  private val withec = Globals.glvalues("with-ec").toBoolean


  def getExpertsPredictions(inps: RunningOptions, batch: Example, markedProgram: String) = {

    if (withec) {

      val targetFluent = getTargetFluent(inps)

      val tpRule1 = s"tp(I, holdsAt($targetFluent,Te)) :- rule(I), example( holdsAt($targetFluent,Te) ), marked(I, initiatedAt($targetFluent,Ts) ), next(Ts,Te), time(Te), time(Ts)."
      val tpRule2 = s"tp(I, holdsAt($targetFluent,Te)) :- rule(I), example( holdsAt($targetFluent,Te) ), not marked(I, terminatedAt($targetFluent,Ts) ), next(Ts,Te), time(Te), time(Ts)."
      val tpRule3 = s"tp(inertia, holdsAt($targetFluent,Te)) :- example( holdsAt($targetFluent,Te) ), inertia( holdsAt($targetFluent,Ts) ), next(Ts,Te), time(Te), time(Ts)."

      val fpRule = s"fp(I, holdsAt($targetFluent,Te)) :- rule(I), not example( holdsAt($targetFluent,Te) ), marked(I, initiatedAt($targetFluent,Ts) ), next(Ts,Te), time(Te), time(Ts)."
      val fpRule2 = s"fp(inertia, holdsAt($targetFluent,Te)) :- not example( holdsAt($targetFluent,Te) ), inertia( holdsAt($targetFluent,Ts) ), next(Ts,Te), time(Te), time(Ts)."

      val fnRule = s"fn(I, holdsAt($targetFluent,Te)) :- rule(I), example( holdsAt($targetFluent,Te) ), marked(I, terminatedAt($targetFluent,Ts) ), next(Ts,Te), time(Te), time(Ts)."


      val e = (batch.annotationASP ++ batch.narrativeASP).mkString("\n")

      val directives = s"\n$tpRule1\n$tpRule2\n$fpRule\n$fnRule"

      val program = e + markedProgram + "\n#include \""+inps.entryPath+"/bk.lp\"." + directives + "\n#show.\n#show tp/2.\n#show fp/2.\n#show fn/2."
      val f2 = Utils.getTempFile(s"quick-and-dirty",".lp")
      Utils.writeToFile(f2, "append")(p => List(program) foreach p.println)
      val paaath = f2.getCanonicalPath
      val _result = ASP.solve(task = Globals.SCORE_RULES, aspInputFile = new File(paaath))

      val result = if (_result.nonEmpty) _result.head.atoms.toSet else Set[String]()

      val (tpAtoms, fpAtoms, fnAtoms) = result.foldLeft(Set[String](), Set[String](), Set[String]()) { (x, atom) =>
        val (a, b, c) = (x._1, x._2, x._3)
        if (atom.startsWith("tp")) (a + atom, b, c)
        else if (atom.startsWith("fp")) (a, b+atom, c)
        else if (atom.startsWith("fn")) (a,b,c+atom)
        else throw new RuntimeException("FUCK This shouldn't have happened")
      }

      val allInferredAtoms = tpAtoms ++ fpAtoms ++ fnAtoms

      allInferredAtoms

    } else {
      Set[String]()
    }

  }

  private def getTargetFluent(inps: RunningOptions) = {
    val targetFluent = {
      // We can take the first one of the head modes (the target fluent is the same
      // regardless of whether the mode atom is an initiation of termination one).
      // Then, to get the target fluent, simply retrieve the first one of the 'terms' arg.

      val t = inps.globals.MODEHS.head.varbed.terms.head
      // The 'if' is for cases where the target pred is of the form initiatedAt(#fluent, +time), as in
      // initiatedAt(#fluent, +time) where fluent(leisure) is in the BK.
      // The 'else' is for compound fluents.
      if (t.isVariabe) Literal(functor = t._type) else inps.globals.MODEHS.head.varbed.terms.head.asInstanceOf[Literal]
      //modehs.head.varbed.terms.head.asInstanceOf[Literal]
    }.tostring
    targetFluent
  }


}

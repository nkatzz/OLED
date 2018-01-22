package oled.weightlearn

import java.io.File

import app.runutils.Globals
import jep.Jep
import logic.{AnswerSet, Constant, Literal}
import logic.Examples.Example
import utils.{ASP, Utils}

object ASP2MLN {

  /*
  *
  * RESTRICTIONS:
  *
  * 1. Currently, all domain predicates (e.g. close/4, next/2 etc), i.e. all predicates
  * that are neither label predicates (initiatedAt/2, terminatedAt/2), nor event predicates
  * (happensAt/2) ARE ASSUMED TO BE FLAT. Therefore, I simply capitalize the predicate functor
  * and each predicate constant to convert them to MLN format. Supporting functions in domain
  * predicates requires some extra work in the conversion.
  *
  *
  * */


  def main(args: Array[String]): Unit = {
    val atoms = ASP.solve(task = Globals.INFERENCE,
      aspInputFile = new File("/home/nkatz/dev/iled/datasets/CaviarWeightLearn/ASP/get-predicates.lp"), jep= new Jep())
    //println(atoms.head.atoms)
    convertAtoms(atoms.head.atoms, "meeting", "initiatedAt")
  }



  def convertAtoms(atoms: List[String], targetHLE: String, targetClass: String) = {

    val fluents = atoms.filter(x => x.contains("fluent") && x.contains(targetHLE)).map { atom => atomToMLNConstant(atom) }.toMap
    val events = atoms.filter(x => x.contains("event")).map { atom => atomToMLNConstant(atom) }.toMap

    val (labelPreds_, eventPreds_, domainPreds_) = atoms.foldLeft(List[String](), List[String](), List[String]()) { (accum, atom) =>
      if (atom.contains(targetClass) && atom.contains(targetHLE)) {
        (accum._1 :+ atom, accum._2, accum._3)
      } else if (atom.contains("happensAt")){
        (accum._1, accum._2 :+ atom, accum._3)
      } else {
        if (!atom.contains("initiatedAt") && !atom.contains("terminatedAt")) {
          (accum._1, accum._2, accum._3 :+ atom)
        } else {
          (accum._1, accum._2, accum._3)
        }
      }
    }

    val labelPreds = labelPreds_.map ( x => ECPredToMLN(x, fluents) )
    val eventPreds = eventPreds_.map ( x => ECPredToMLN(x, events) )

    // These are various domain-knowledge predicates.
    // They are assumed to be flat, so I just capitalize constants.
    /*
    val domainPreds = domainPreds_.map { atom =>
      val lit = Literal.toLiteral1(atom)
      val terms = lit.terms.map(x => Constant(x.tostring.capitalize))
      Literal(functor = lit.functor.capitalize, terms = terms).tostring
    }
    */

    println(fluents)
    println(events)
    println(labelPreds)
    println(eventPreds)
    println(domainPreds_)
    //println(domainPreds)
  }

  def atomToMLNConstant(s: String) = {
    val atom = Literal.toLiteral1(s).terms.head.asInstanceOf[Literal]
    val constant = (List(s"${atom.functor.capitalize}") ++ atom.terms.map(x => x.tostring.capitalize)).mkString("_")
    (atom.tostring, constant)
  }

  def ECPredToMLN(s: String, atomConstantsMap: Map[String, String]) = {
    val (ss, isNegated) = if (s.contains("false_")) (s.split("false_")(1), true) else (s, false)
    val w = atomConstantsMap.keySet.find(x => s.contains(x)).getOrElse(throw new RuntimeException(s"Cannot fluent mapping for atom $s in fluents map: $atomConstantsMap"))
    val mlnAtom = ss.capitalize.replace(w, atomConstantsMap(w))
    if (isNegated) s"!$mlnAtom" else mlnAtom
  }



  def solve(ex: Example, target: String, jep: Jep): List[AnswerSet] = {
    val f = Utils.getTempFile(s"asp2mln",".lp")
    Utils.writeToFile(f, "append")(p => List(ex.annotationASP.filter(_.contains(target)) ++ ex.narrativeASP) foreach p.println)
    ASP.solve(task = Globals.INFERENCE, aspInputFile = new File(f.getCanonicalPath), jep=jep)
  }

}

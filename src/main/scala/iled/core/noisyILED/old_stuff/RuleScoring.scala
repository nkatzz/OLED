package iled.core.noisyILED.old_stuff

import java.io.File

import iled.core.Core
import iled.core.Implicits._
import iled.structures.Examples.Example
import iled.structures.{Clause, Literal, Theory}
import iled.utils.{ASP, Utils}
import jep.Jep

/**
  * Created by nkatz on 4/2/2016.
  */
object RuleScoring {


  def score(merged: Theory, e: Example, jep: Jep) = {
    scoreRules(merged,e,jep)
    merged.clauses foreach {
      x => {
        println("top rule:",x.tps,x.fps,x.fns,x.##)
        x.supportSet.clauses.foreach {
          y => println("ss rule:",y.tps,y.fps,y.fns,y.##)
        }
        println(" ")
      }
    }
  }

  def ruleHashCode(x: Clause) = x.## //Math.abs(x.##)

  def markAll(theory: Theory) = {
    def markRule(r: Clause) = Clause(head=Literal(functor = "marked", terms=List(r.##, r.head)),body=r.withTypePreds().body)

    val markInit = "\ninitiatedAt(F,T) :- marked(I,initiatedAt(F,T)), rule(I).\n"
    val markTerm = "\nterminatedAt(F,T) :- marked(I,terminatedAt(F,T)), rule(I).\n"
    val directives = Core.markRulesDirectives
    //--------------------------------------------------
    val programPart1 = markInit + markTerm + directives
    //--------------------------------------------------
    val allSupports = theory.clauses flatMap (_.supportSet.clauses)
    // No, refinements will be a rule's attribute
    //val allRefinements = allSupports flatMap(_.refinements)
    val allRefinements = theory.clauses flatMap(_.refinements)
    //val allRefinements = allSupports flatMap(x => x.refinements.flatMap(z => z.allRules))

    val allRules = theory.clauses ++ allSupports ++ allRefinements

    // generate marked versions for all:
    val markedTheory = theory.clauses map markRule
    val markedSupports = allSupports map markRule
    val markedRefinements = allRefinements map markRule
    val allRulesMarked = markedTheory ++ markedSupports ++ markedRefinements
    // create a hashmap of hashcodes and the corresponding clauses
    val hashCodesClausesMap = (allRules map (x => x.##.toString -> x)).toMap
    val rulePredicates = hashCodesClausesMap.keySet.map(x => s"rule($x). ").mkString("\n")
    val show = "\n#show tps/2.\n#show fps/2.\n#show fns/2.\n"
    //------------------------------------------------------------------------------------------------
    val programPart2 = allRulesMarked.map(x => x.tostring+"\n").mkString("\n") + rulePredicates + show
    //------------------------------------------------------------------------------------------------
    (programPart1+programPart2,hashCodesClausesMap)
  }

  def scoreRules(theory: Theory, example: Example, jep: Jep): Unit = {
    val e = (example.annotationASP ++ example.narrativeASP).mkString("\n")
    val markedProgram = markAll(theory)
    val all = e + s"\n#include " + "\""+Core.bkMarkedFile+"\".\n" + markedProgram._1
    val f = Utils.getTempFile("isConsistent",".lp",deleteOnExit = true)
    Utils.writeToFile(f, "append")(p => List(all) foreach p.println)
    val path = f.getCanonicalPath
    val answerSet = ASP.solve(task = Core.INFERENCE, aspInputFile = new File(path), jep=jep)
    val out = answerSet match {
      case Nil => ""
      case _ => //println(answerSet.head.atoms.mkString(" "))

        /* debug */
        if (answerSet.head.atoms.exists(x => x.startsWith("tps"))) {
          val stop = "stop"
        }
        /* debug */

        val f = (atom: String) => {
          val tolit = Literal.toLiteral(atom)
          val (hash, example) = (tolit.terms.head.tostring, tolit.terms.tail.head.tostring)
          (tolit,hash,example)
        }

        val g = (hashCode: String, what: String) =>{
          val clause = markedProgram._2(hashCode)
          what match {
            case "tps" => clause.tps += 1
            case "fps" => clause.fps += 1
            case "fns" => clause.fns += 1
          }
        }

        val groupped = answerSet.head.atoms.foldLeft(List[String](),List[String](),List[String]()){
          (x,y) =>
            val (tps,fps,fns) = (x._1,x._2,x._3)
            val z = f(y)
            z._1.functor match {
              case "tps" => (tps :+ z._2,fps,fns)
              case "fps" => (tps,fps :+ z._2,fns)
              case "fns" => (tps,fps,fns :+ z._2 )
            }
        }
        val (tps,fps,fns) = (groupped._1,groupped._2,groupped._3)
        // increment the counts for each rule
        tps foreach (x => g(x,"tps"))
        fps foreach (x => g(x,"fps"))
        //fns foreach (x => g(x,"fns")) // UNCOMMENT THIS TO GET FNS COUNTS (MAYBE THEY WILL BE NEEDED FOR terminatedAt RULES)
    }

    //out
  }


}

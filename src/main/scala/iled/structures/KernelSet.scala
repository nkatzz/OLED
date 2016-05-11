package iled.structures

import com.typesafe.scalalogging.LazyLogging
import iled.core.Core
import iled.core.Xhail._
import iled.structures.Modes.ModeAtom
import iled.utils.ASP

import scala.collection.mutable.ListBuffer
import scala.util.matching.Regex

/**
  * Created by nkatz on 11/6/15.
  */


class KernelSet(abdModel: AnswerSet, examples: Map[String, List[String]]) extends Theory with LazyLogging{

  /*

  def generateKernel(abdModel: List[String],
                     alternativePath: String = "",
                     examples: Map[String, List[String]],
                     aspInputFile: java.io.File): (List[Clause], List[Clause]) = {

    val filterout = (x: String, y: Regex, z: List[String]) =>
      z.filter(
        e => !y.findAllIn(x).toList.map(
          q => q.replaceAll("\"", "")
        ).exists(e.contains(_)))

    def generateQueries(interms: List[Expression]): List[(List[String], ModeAtom)] = {

      val p: List[String] = for ( x <- interms;
                                  pred = x.asInstanceOf[Constant]._type;
                                  arg = x.asInstanceOf[Constant].name
      ) yield s"$pred($arg)."
      var accum = new ListBuffer[(List[String], ModeAtom)]()
      for (x <- Core.modebs) {
        val (q,y) = groundModeAtom(x,p)
        accum += ((q, y))
      }
      accum.toList
    }

    def groundModeAtom(x: ModeAtom, p: List[String]) = {
      val varb = x.varbed
      val str = varb.tostringQuote + " :- " +
        filterout(varb.tostringQuote, "\"([A-Za-z0-9_])*\"".r, varb.typePreds).mkString(",") + "."
      ASP.toASPprogram(program =
        p ++ List(str) ++
          //List(s"\n#include "+"\""+Core.bkFile+"\".") ++
          List(s"\n\n#show ${varb.functor}/${varb.arity}."),
        writeToFile = aspInputFile.getCanonicalPath)
      val q = ASP.solve("getQueries", examples = examples, aspInputFile = aspInputFile)
      (q.head.atoms, x)
    }

    val abducedAtoms: List[Literal] = for (
      x <- abdModel;
      tolit = Literal.toLiteral(x);
      (atom, modeAtom) = try {
        (tolit.terms(1), Core.modehs(tolit.terms(0).asInstanceOf[Constant].name.toInt - 1))
      } catch {
        case e: java.lang.ClassCastException => (tolit, tolit.matchingMode)
      }
    ) yield Literal.toLiteral2(atom.asInstanceOf[Literal], modeAtom.asInstanceOf[ModeAtom])

    var kernelSet = new ListBuffer[Clause]()

    for (x <- abducedAtoms) {
      var body = new ListBuffer[Literal]()
      val (_interms, _, _) = x.getPlmrkTerms
      val interms = _interms.to[ListBuffer]
      for (i <- 0 to Core.glvalues("variableDepth").toInt) {
        val queries = generateQueries(interms.toList)


        val deduce =
          (for ((queryList, mAtom) <- queries ;
                show = queryList map (
                  x => x.replaceAll("\"", "")
                  ) map ( x =>
                  "\n#show " + "ifTrue("+Core.modebs.indexOf(mAtom)+","+x+")" + ":" + x + ".\n")
          ) yield show).flatten

        val program = abducedAtoms.map(x => x.tostring + ".")

        ASP.toASPprogram(program =
          examples("annotation") ++
            examples("narrative") ++
            program ++
            List(s"\n#include "+"\""+Core.bkFile+"\".") ++
            List("\n#show.\n") ++ deduce, writeToFile = aspInputFile.getCanonicalPath)
        val q = ASP.solve("deduction", examples = examples, aspInputFile = aspInputFile)

        val b = q.head.atoms.asInstanceOf[List[String]] map (
          x => Literal.toLiteral(x)
          ) map (
          x => (x.terms(0),x.terms(1))
          ) map (
          x => Literal.toLiteral2(x._2.asInstanceOf[Literal],
            Core.modebs(x._1.asInstanceOf[Constant].name.toInt))
          )

        for (k <- b) {
          if (!body.contains(k)) body ++= b
          val (_, outTerms, _) = k.getPlmrkTerms
          interms ++= outTerms
        }

      }
      val kernelClause = Clause(x.asPosLiteral, body.toList)
      kernelSet += kernelClause
    }
    val varKernel = kernelSet.map(x => x.varbed);
    val vlength = varKernel.length
    val compressed = compressTheory(varKernel.toList);
    val clength = compressed.length
    logger.info("Created Kernel set")
    logger.debug("\n------------------------------------------------------------------------------------\n" +
      s"Kernel Set (Ground---Variabilized($vlength clauses)---Compressed($clength clauses)):" +
      "\n------------------------------------------------------------------------------------\n" +
      showTheory(kernelSet.toList) + "\n\n" + showTheory(varKernel.toList) + "\n\n" + showTheory(compressed.toList))

    (kernelSet.toList, compressed)
  }


}


object KernelSet {

*/
}


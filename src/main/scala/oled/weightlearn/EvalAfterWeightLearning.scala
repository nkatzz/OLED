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

package oled.weightlearn

import logic.Examples.Example
import logic.{Constant, Literal}
import utils.Utils

import scala.io.Source
import scala.sys.process._

class EvalAfterWeightLearning(val mlnClauses: List[String], val testData: Iterator[Example]) {

  private val cwd = System.getProperty("user.dir")

  private val scriptsPath = if (cwd.contains("scripts")) cwd+"/weight-learn-eval-meet" else cwd+"/scripts/weight-learn-eval-meet"

  //private val scriptsPath = if (cwd.contains("scripts")) cwd+"/weight-learn-eval-move" else cwd+"/scripts/weight-learn-eval-move"


  private val bkFile = scriptsPath+"/bk.mln"
  private val resultsFile = scriptsPath+"/results"
  private val inferScript = scriptsPath+"/infer.sh"
  private val compileScript = scriptsPath+"/compile.sh"
  private val compiled = scriptsPath+"/compiled.mln"
  private val evidenceFile = scriptsPath+"/evidence.db"
  private val domainFile = scriptsPath+"/domain.lp"

  private val bk = Source.fromFile(bkFile).getLines.
    toList.takeWhile(line => line != "// LEARNT RULES FROM HERE!") ++
    List("// LEARNT RULES FROM HERE!") ++ mlnClauses

  Utils.writeLine(bk.mkString("\n"), bkFile, "overwrite")

  /* Compile the MLN. This must take place just once for all testing data. */
  private val command1 = Seq(compileScript, bkFile, compiled).mkString(" ")
  private val res1 = command1 !!

  def getCounts() = {
    testData.foldLeft(0,0,0) { (accum, testBatch) =>
      val (_tps, _fps, _fns) = (accum._1, accum._2, accum._3)

      /* Get the domain signatures for events and fluents */
      val (sigs, nexts) = getEventFluentSignatures(testBatch).
        partition(p => p.startsWith("event") || p.startsWith("fluent"))

      val signatures = sigs.map { atom =>
        val parsed = Literal.parseWPB2(atom)

        val compound = {
          val innerSignatureAtom = parsed.terms.head.asInstanceOf[Literal]
          // All inner terms of the signature atom should be constants,
          // otherwise something's wrong...
          Literal(predSymbol = innerSignatureAtom.predSymbol,
            terms = innerSignatureAtom.terms.map(x => Constant(x.name.capitalize))).tostring
        }

        val flattened = Literal.toMLNFlat(parsed).terms.head.tostring
        s"$flattened = $compound"
      }

      val nextAtoms = nexts.toVector.map { x => toMLNFormat(x) }

      val _narrativeToMLN = testBatch.narrative.toVector.map { x => toMLNFormat(x) }

      val (__narrativeToMLN, startTimePred) = _narrativeToMLN.partition(x => !x.startsWith("Starttime"))

      val narrativeToMLN = __narrativeToMLN :+ startTimePred.head.replace("Starttime", "StartTime")

      val evidence = (signatures.toVector ++ Vector("\n\n") ++ narrativeToMLN ++ nextAtoms).mkString("\n")

      Utils.writeLine(evidence, evidenceFile, "overwrite")

      /* Run MAP inference */
      val command2 = Seq(inferScript, compiled, evidenceFile, resultsFile).mkString(" ")
      val res2 = command2 !!

      /* Read off the inference results */
      val inferredAtoms = Source.fromFile(resultsFile).getLines.
        filter(p => p.startsWith("HoldsAt") && p.split(" ")(1) == "1").map(x => x.split(" ")(0)).toSet

      val annotationAtoms = testBatch.annotation.map { x => toMLNFormat(x)}.toSet

      /* Calculate tps, fps, fns */

      val tps = inferredAtoms.intersect(annotationAtoms).size
      val fps = inferredAtoms.diff(annotationAtoms).size
      val fns = annotationAtoms.diff(inferredAtoms).size

      println(tps, fps, fns)

      (_tps + tps, _fps + fps, _fns + fns)
    }
  }





  /* Utility functions */

  def toMLNFormat(x: String) = {
    val parsed = Literal.parseWPB2(x)
    Literal.toMLNFlat(parsed).tostringMLN
  }

  def getEventFluentSignatures(e: Example) = {
    val all = (e.narrativeASP ++ List(s"""#include "$domainFile".""")) .mkString("\n")
    val f = Utils.getTempFile("ground", ".lp")
    Utils.writeLine(all, f.getCanonicalPath, "overwrite")
    val cores = Runtime.getRuntime.availableProcessors
    val command = Seq("clingo", f.getCanonicalPath , "-Wno-atom-undefined", s"-t$cores", "--verbose=0").mkString(" ")
    val result = command.lineStream_!
    val results = result.toVector
    val atoms = results(0)
    val status = results(1)
    if (status == "UNSATISFIABLE") throw new RuntimeException("UNSATISFIABLE program!")
    atoms.split(" ")
  }




}

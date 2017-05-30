package logic

import app.Globals
import jep.Jep
import logic.Examples.Example
import utils.{ASP, Utils}
import xhail.Xhail

import scala.collection.mutable.ListBuffer


/**
  * Created by nkatz on 9/13/16.
  */
object LogicUtils {


  def compressTheory(kernel: List[Clause]): List[Clause] = {
    val compressed = new ListBuffer[Clause]
    val included = (c: Clause) => compressed.toList.exists(x => x.thetaSubsumes(c) && c.thetaSubsumes(x))
    for (c <- kernel) {
      //val others = kernel.filter(x => x != c)
      if (!included(c)) compressed += c
    }
    compressed.toList
  }

  def generateKernel(examples: Map[String,List[String]], fromWeakExmpl: Boolean = false,
                     jep: Jep, learningTerminatedOnly: Boolean=false,
                     oledLearningInitWithInertia: Boolean=false, bkFile: String, globals: Globals) = {

    val infile = Utils.getTempFile("example", ".lp", deleteOnExit = true)

    //val f = (x: String) => if(x.endsWith(".")) x.split("\\.")(0) else x
    //val interpretation = examples("annotation").map(x => s"${f(x)}.") ++ examples("narrative").map(x => s"${f(x)}.")

    val f = (x: String) => if (x.endsWith(".")) x else s"$x."
    val interpretation = examples("annotation").map(x => s"${f(x)}") ++ examples("narrative").map(x => s"${f(x)}")

    Utils.writeToFile(infile, "overwrite") { p => interpretation.foreach(p.println) }
    var (kernel, varKernel) =
      Xhail.runXhail(fromFile = infile.getAbsolutePath, kernelSetOnly = true,
        fromWeakExmpl = fromWeakExmpl, jep=jep, learningTerminatedAtOnly=learningTerminatedOnly,
        oledLearningInitWithInertia=oledLearningInitWithInertia, bkFile=bkFile, globals=globals)
    if (fromWeakExmpl) {
      varKernel = varKernel.map (x => Clause.updateField(x, fromWeakExample = true))
    }
    (kernel,varKernel)
  }

  /*The only difference is that the examples are provided with a file. I have to
  * fix this, it's stupid to duplicate code like that.*/
  def generateKernel2(examplesFile: java.io.File, jep: Jep, bkFile: String, globals: Globals) = {
    val (kernel, varKernel) =
      Xhail.runXhail(fromFile = examplesFile.getAbsolutePath, kernelSetOnly = true,
        fromWeakExmpl = false, jep=jep, learningTerminatedAtOnly=false,
        oledLearningInitWithInertia=false, bkFile=bkFile, globals=globals)
    (kernel,varKernel)
  }


  def isSAT(theory: Theory, example: Example, globals: Globals, F: (Theory, Example, Globals) => String, jep: Jep): Boolean = {
    val f = F(theory, example, globals)
    val out = ASP.solve(Globals.CHECKSAT, Map(), new java.io.File(f), example.toMapASP, jep = jep)
    if (out != Nil && out.head == AnswerSet.UNSAT) false else true
  }

  def updateSupport(theory: Theory, kernelSet: Theory, fromWeakExample: Boolean = false) = {

    // This is used to avoid adding redundant rules in the
    // support set. A rule is redundant if it subsumes
    // by a rule already present in the support set
    val isRedundant = (ss: Clause, c: Clause) =>
      c.supportSet.clauses exists (x => ss.thetaSubsumes(x))

    for (c <- theory.clauses;
         ks <- kernelSet.clauses
         if !isRedundant(ks, c) && c.thetaSubsumes(ks)) {
      val markedKS = Clause.updateField(ks, fromWeakExample = fromWeakExample)
      c.supportSet = Theory(c.supportSet.clauses :+ markedKS)
    }

    // This is used in order to avoid maintaining redundant
    // rules in the support set. In this context, a support set
    // rule is redundant if it subsumes some other rule
    // in the support set. This can happen in cases where e.g.
    // p :- q,r was added to the support set early on and
    // later on p :- q,r,s was added. In this case the first
    // rule is redundant and should be removed. This redundancy
    // checking should be done whenever the support set
    // changes with the addition of a rule.

    theory.clauses foreach (x => x.compressSupport)
  }


}

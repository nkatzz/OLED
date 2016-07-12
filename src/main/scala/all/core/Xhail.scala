package all.core

import java.io.File

import com.typesafe.scalalogging._
import all.globalValues.GlobalValues
import all.parsers.ASPResultsParser
import all.structures.Exceptions._
import all.structures.Modes._
import all.structures.Rules._
import all.structures._
import all.utils.{MongoUtils, ASP, Utils}
import jep.Jep

import scala.collection.mutable.ListBuffer
import scala.io.Source
import scala.util.matching.Regex


object Xhail extends ASPResultsParser with LazyLogging {

  /**
    * @param fromFile either a path to a file from where the training examples are read, or "", in which case the examples are
    *                 fetched from a db.
    * @param the name of a database from which the examples are fetched.
    * @param inputDirectory this is not used. The input directory with the bk and modes file and the ASP solver is given as
    *                       a static field in Core
    * @kernelSetOnly if true only the Kernel Set is generated and it is not searched for a hypothesis.
    *
    */

  val showTheory = (x: List[Clause]) => x.map { x => x.tostring }.mkString("\n")
  var outTheory: Theory = Theory()



  def main(args: Array[String]) {

    // These arguments are to be read from command line
    // and set to Core.glvalues via Core.setGlobalParams
    Core.glvalues("perfect-fit") = "false"
    //Core.inputPath = s"${Core.cwd}/Caviar"
    //Core.fromDB = "CAVIAR-01-Walk1"
    Core.setGlobalParams(args)
    val jep = new Jep()

    runXhail(fromDB = Core.fromDB, jep=jep)
    jep.close()
  }

  def runXhail(fromFile: String = "",
               fromDB: String = "",
               inputDirectory: String = "",
               kernelSetOnly: Boolean = false,
               learningTerminatedAtOnly: Boolean = false,
               fromWeakExmpl:Boolean = false,
               jep: Jep,
               bkFile: String = Core.bkFile,
               oledLearningInitWithInertia: Boolean=false): (List[Clause],List[Clause]) = {

    val matches = (p: Regex, str: String) => p.pattern.matcher(str).matches
    val examples: Map[String, List[String]] = fromFile match {
      case "" =>
        fromDB match {
          case "" => throw new TrainingExamplesException("Provide a file or a mongo DB with the training examples.")
          case _ => Utils.getAllExamples(fromDB, "examples")
        }
      case _ =>
        val all =
          Source.fromFile(fromFile).getLines.toList.map(x => x.replaceAll("\\s", "")).filter(line => !matches( """""".r, line))
        val annotation = all.filter { x => x.contains("example(") }
        val narrative = all.filter { x => !x.contains("example(") }
        Map("annotation" -> annotation, "narrative" -> narrative)
    }

    val aspFile: File = Utils.getTempFile("aspinput", ".lp", "", deleteOnExit = true)
    val abdModels: List[AnswerSet] =
      abduce("modehs",
        examples = examples,
        learningTerminatedAtOnly = learningTerminatedAtOnly, fromWeakExmpl = fromWeakExmpl, jep=jep, bkFile=bkFile)
    //if (abdModel != Nil) logger.info("Created Delta set")
    val (kernel, varKernel) = abdModels match {
      case Nil => (List[Clause](),List[Clause]())
      case _ =>
        if (!Core.glvalues("iter-deepening").toBoolean){
          //val abduced = if(learningTerminatedAtOnly) abdModels.head.atoms.filter(_.contains("terminatedAt")) else abdModels.head.atoms
          val abduced =
            /*
            if(!oledLearningInitWithInertia) {
              if(learningTerminatedAtOnly) abdModels.head.atoms.filter(_.contains("terminatedAt")) else abdModels.head.atoms
            } else {
              //if(learningTerminatedAtOnly) abdModels.head.atoms.filter(_.contains("terminatedAt")) else abdModels.head.atoms
              if(learningTerminatedAtOnly) abdModels.head.atoms.filter(_.contains("terminatedAt")) else abdModels.head.atoms.filter(_.contains("initiatedAt"))
            }
            */
            if(learningTerminatedAtOnly) abdModels.head.atoms.filter(_.contains("terminatedAt")) else abdModels.head.atoms
            //if(learningTerminatedAtOnly) abdModels.head.atoms.filter(_.contains("terminatedAt")) else abdModels.head.atoms.filter(_.contains("initiatedAt"))
          generateKernel(abduced, examples = examples, aspInputFile = aspFile, jep=jep, bkFile=bkFile)
        } else {
          return iterativeSearch(abdModels,examples,jep,kernelSetOnly) // this is used from ILED to find a kernel with iterative search
        }
    }
    if (!kernelSetOnly) findHypothesis(varKernel, examples = examples, jep=jep)
    (kernel, varKernel)
  }


  def iterativeSearch(models: List[AnswerSet], e: Map[String, List[String]], jep: Jep, kernelSetOnly: Boolean) = {

    val findHypothesisIterativeSearch = (varKernel: List[Clause], examples: Map[String, List[String]], jep: Jep) => {
      val aspFile: File = Utils.getTempFile("aspInduction", ".lp", "", deleteOnExit = true)
      val (_, use2AtomsMap,_,_,_,_) = ASP.inductionASPProgram(kernelSet=Theory(varKernel),examples=examples,aspInputFile=aspFile)
      ASP.solve(Core.SEARCH_MODELS,use2AtomsMap,examples=examples,aspInputFile=aspFile,jep=jep)
    }

    var foundHypothesis = false
    var modelCounter = 0
    var kernel = (List[Clause](),List[Clause]())
    while (!foundHypothesis) {
      if (modelCounter == models.length){ // We tried all models and failed to find a hypothesis
        throw new RuntimeException("Failed to find a hypothesis with iterative search")
      }
      val model = models(modelCounter)
      logger.info("Trying and alternative adbuctive explanation:")
      kernel = generateKernel(model.atoms,examples=e,aspInputFile=Utils.getTempFile("iterSearch","lp"),jep=jep)
      val tryNew = findHypothesisIterativeSearch(kernel._2,e,jep)
      if (tryNew.nonEmpty && tryNew.head!=AnswerSet.UNSAT) {
        foundHypothesis = true
      } else {
        logger.info("Failed to generalize the Kernel set.")
      }
      modelCounter = modelCounter+1
    }
    if (!kernelSetOnly) findHypothesis(kernel._2,examples=e,jep=jep)
    kernel
  }

  /**
    * Prepares the ASP input for abduction and calls the ASP solver to get the results.
    *
    * @param abducibles a flag to indicate where to get the abducible predicates from.
    *                   Currently the only acceptable flag is "modehs", meaning that abducible predicates
    *                   are the head mode declaration atoms.
    * @param numberOfModels an upper bound to the number of models. Currently this is not
    *                       used anywhere.
    * @param useMatchModesProgram @tparam Boolean is true, then this methods creates an additional program that allows
    *                             to pair an abduced atom with its matching mode atom, on ASP side.
    * @throws AbductionException in case of mistaken or missing abducibles flag.
    * @todo implement the case where abducible predicates are explicitly provided.
    *       (see comments in code).
    * @todo implement iterative deepening.
    */



  def abduce(abducibles: Any,
             numberOfModels: Int = 1000,
             useMatchModesProgram: Boolean = true,
             examples: Map[String, List[String]],
             learningTerminatedAtOnly: Boolean = false,
             fromWeakExmpl:Boolean = false,
             jep: Jep, bkFile: String = Core.bkFile): List[AnswerSet] = {

    val aspFile: File = Utils.getTempFile("aspinput", ".lp", "", deleteOnExit = true)
    val varbedExmplPatterns = Core.examplePatterns map (x => x.varbed)
    def getASPinput(jep: Jep) = {
      Core.modehs match {
        case Nil => throw new RuntimeException("No Mode Declarations found.")
        case _ =>
          val varbedMHAtoms = Core.modehs map (x => x.varbed)
          val generate: List[String] = varbedMHAtoms.map(
            x => s"{${x.tostring}} :- " + x.typePreds.mkString(",") + "."
          )
          // Generate minimize statement
          val minimize = Core.glvalues("iter-deepening") match {
            case "false" =>
              if (GlobalValues.glvalues("perfect-fit").toBoolean){
                "\n#minimize{\n" + (varbedMHAtoms map (
                  x => "1," + (x.variables map (y => y.tostring)).mkString(",") + s":${x.tostring}")
                  ).mkString(";\n") + "\n}."
              } else {
                val f = (x: Literal) => "1," + (x.variables map (y => y.tostring)).mkString(",")
                val ff = varbedExmplPatterns.map(x =>
                  s"${f(x)},posNotCovered(${x.tostring}):example(${x.tostring})," +
                    s" not ${x.tostring};\n${f(x)},negsCovered(${x.tostring}):${x.tostring}," +
                    s" not example(${x.tostring})").mkString(";")
                "\n#minimize{\n" + (varbedMHAtoms map (
                  x => "1," + (x.variables map (y => y.tostring)).mkString(",") + s":${x.tostring}")
                  ).mkString(";\n") + s";\n$ff\n}."
              }

            // If we want iterative deepening then we drop the minimize statements
            // Also to use iterative deepening we'll have to pass the required generateAtLeast
            // and generateAtMost parameters.
            case _ => ""
          }

          val coverageConstr: List[String] =
            if(GlobalValues.glvalues("perfect-fit").toBoolean) {
              ASP.getCoverageDirectives(learningTerminatedAtOnly)
            } else {
              val z = varbedExmplPatterns.map { x =>
                s"\nposNotCovered(${x.tostring}) :- example(${x.tostring}), not ${x.tostring}." +
                  s"\nnegsCovered(${x.tostring}) :- ${x.tostring}, not example(${x.tostring})."
              }.mkString("\n")
              List(z)
            }


          val modeMatchingProgram =
            if (useMatchModesProgram)
              ASP.matchModesProgram(Core.modehs.map(x => x.varbed))
            else List()

          ASP.toASPprogram(
            program = List(s"#include "+
              "\""+bkFile+"\"" +".\n\n") ++
              examples("annotation") ++ List("\n\n") ++
              examples("narrative") ++ List("\n\n") ++
              coverageConstr ++ generate ++ List(minimize),
            extra = modeMatchingProgram,
            writeToFile = aspFile.getCanonicalPath)
      }
    }
    abducibles match {
      case "modehs" => getASPinput(jep)
      /* This is for the case where abducibles are explicitly given.
       *
       * @todo: Implement this logic
       *
       * */
      case _: List[Any] => throw new AbductionException("This logic has not been implemented yet.")
      case _ => throw new AbductionException("You need to specify the abducible predicates.")
    }
    ASP.solve(Core.ABDUCTION,examples=examples,aspInputFile=aspFile,fromWeakExmpl=fromWeakExmpl,jep=jep)
  }

  /**
    * Generates a Kernel Set.
    *
    * @param abdModel @tparam List[Literal] the list of atoms previousely abduced.
    * @return the ground and variabilized Kernel Set in a tuple
    * @todo Need to fix the body generation loop: Each constant that corresponds to an output placemarker
    *       must be added to the initial (but growing) set of input terms, used to generate instances of body atoms.
    */

  def generateKernel(abdModel: List[String],
                     alternativePath: String = "",
                     examples: Map[String, List[String]],
                     aspInputFile: java.io.File, jep: Jep, bkFile: String = Core.bkFile): (List[Clause], List[Clause]) = {



    def groundBodyModesWithInTerms(interms: List[Expression]): List[(List[String], ModeAtom)] = {

      val filterout =
        (x: String, y: Regex, z: List[String]) => z.filter(e =>
          !y.findAllIn(x).toList.map(q =>
            q.replaceAll("\"", "")).exists(e.contains(_)))

      val p: List[String] =
        for ( x <- interms;
              pred = x.asInstanceOf[Constant]._type;
              arg = x.asInstanceOf[Constant].name
        ) yield s"$pred($arg)."

      val mapping = (Core.modebs map (x => (Core.modebs.indexOf(x),x))).toMap
      val groundModes =
        for (x <- Core.modebs;
             varb = x.varbed;
             quoted = x.varbed.tostringQuote;
             quoatedNoNeg = x.varbed.nonNegated.tostringQuote;
             test = "Stop") yield
          // surround with triple quotes to allow double quotes in the string
          s"""ground(${Core.modebs.indexOf(x)},$quoatedNoNeg) :- ${filterout(quoted, "\"([A-Za-z0-9_])*\"".r, varb.typePreds).mkString(",")}."""

      ASP.toASPprogram(
        program = p ++ groundModes ++ List("\n\n#show ground/2."),
        writeToFile = aspInputFile.getCanonicalPath)

      val q = ASP.solve("getQueries", examples = examples, aspInputFile = aspInputFile, jep=jep)
      /*
      val result =
        (for (x <- q.head.atoms;
              tolit = Literal.toLiteral(x);
              mode = mapping(tolit.terms.head.name.toInt);
              groundTerm = tolit.terms(1))
          yield (mode,groundTerm)).groupBy(_._1).mapValues(p => p map (q => q._2)).map(z => (z._2.map(k=>k.tostring),z._1)).toList
      */

      val result =
        (for (x <- q.head.atoms;
              tolit = Literal.toLiteral(x);
              mode = mapping(tolit.terms.head.name.toInt);
              groundTerm = tolit.terms(1))
          yield (mode,groundTerm)).groupBy(_._1).toList map {case (k,v) => (for ((_,m) <- v) yield m.tostring, k) }

      result
    }


    //println(abdModel)


// Map[Modes.ModeAtom, List[(Modes.ModeAtom, Expression)]]
    val abducedAtoms: List[Literal] = for (
      x <- abdModel;
      tolit = Literal.toLiteral(x);
      (atom, modeAtom) = try {
        (tolit.terms(1), Core.modehs(tolit.terms.head.asInstanceOf[Constant].name.toInt - 1))
      } catch {
        case e: java.lang.ClassCastException => (tolit, tolit.matchingMode)
      }
    ) yield Literal.toLiteral2(atom.asInstanceOf[Literal], modeAtom.asInstanceOf[ModeAtom])

    var kernelSet = new ListBuffer[Clause]()
    if (Core.glvalues("iter-deepening").toBoolean) logger.info(abducedAtoms.map(_.tostring).mkString(" "))
    for (x <- abducedAtoms) {
      var body = new ListBuffer[Literal]()
      val (_interms, _, _) = x.getPlmrkTerms
      val interms = _interms.to[ListBuffer]
      var solution = List[AnswerSet]()
      for (i <- 0 to Core.glvalues("variableDepth").toInt) {
        val queries = groundBodyModesWithInTerms(interms.toList)

        val deduce =
          (for ((queryList, mAtom) <- queries ;
                show = queryList map (x => x.replaceAll("\"", "")) map ( x =>
                  //"\n#show " + "ifTrue("+Core.modebs.indexOf(mAtom)+","+x+")" + ":" + (if (!mAtom.isNAF) x else "not "+x) + ".\n")
                  s"\n#show ifTrue(${Core.modebs.indexOf(mAtom)},$x) : ${if (!mAtom.isNAF) x else "not "+x}, ${Literal.types(x,mAtom)}."
          )) yield show).flatten

        val program = abdModel.map(x => x + ".")

        ASP.toASPprogram(program =
          examples("annotation") ++
            examples("narrative") ++
            program ++
            List(s"\n#include "+"\""+bkFile+"\".") ++
            List("\n#show.\n") ++ deduce, writeToFile = aspInputFile.getCanonicalPath)
        solution = ASP.solve("deduction", examples = examples, aspInputFile = aspInputFile, jep=jep)
        if (solution.nonEmpty) {
          val f = (x: (Expression, Expression)) => {
            val mode = Core.modebs(x._1.asInstanceOf[Constant].name.toInt)
            val lit =
              if (mode.isNAF) Literal.toLiteral2(x._2.asInstanceOf[Literal]).negated
              else Literal.toLiteral2(x._2.asInstanceOf[Literal]).nonNegated
            Literal.toLiteral2(lit, mode)
          }

          val b = solution.head.atoms.asInstanceOf[List[String]] map (
            x => Literal.toLiteral(x)
            ) map (
            x => (x.terms.head, x.terms(1))
            ) map (
            //x => Literal.toLiteral2(x._2.asInstanceOf[Literal], Core.modebs(x._1.asInstanceOf[Constant].name.toInt))
            x => f(x)
            )

          for (k <- b) {
            if (!body.contains(k)) body ++= b
            val (_, outTerms, _) = k.getPlmrkTerms
            interms ++= outTerms
          }

        }
      }
      if (solution.nonEmpty) {
        val kernelClause = Clause(x.asPosLiteral, body.toList.distinct)
        kernelSet += kernelClause
      }

    }
    val varKernel = kernelSet.map(x => x.varbed)
    val vlength = varKernel.length
    val compressed = if (Core.glvalues("compressKernels").toBoolean) compressTheory(varKernel.toList) else varKernel.toList
    val clength = compressed.length
    logger.info("Created Kernel set")
    logger.debug("\n------------------------------------------------------------------------------------\n" +
      s"Kernel Set (Ground---Variabilized($vlength clauses)---Compressed($clength clauses)):" +
      "\n------------------------------------------------------------------------------------\n" +
      showTheory(kernelSet.toList) + "\n\n" + showTheory(varKernel.toList) + "\n\n" + showTheory(compressed.toList))
    //println(Theory(kernelSet.toList).tostring)
    (kernelSet.toList, compressed)
  }

  def compressTheory(kernel: List[Clause]): List[Clause] = {
    val compressed = new ListBuffer[Clause]
    val included = (c: Clause) => compressed.toList.exists(x => x.thetaSubsumes(c) && c.thetaSubsumes(x))
    for (c <- kernel) {
      val others = kernel.filter(x => x != c)
      if (!included(c)) compressed += c
    }
    compressed.toList
  }

  def findHypothesis(varKernel: List[Clause],
                     examples: Map[String, List[String]], jep: Jep) = {


    GlobalValues.glvalues("perfect-fit") = "false"


    val aspFile: File = Utils.getTempFile("aspInduction", ".lp", "", deleteOnExit = true)
    val (_, use2AtomsMap, _, _, _, _) =
      ASP.inductionASPProgram(kernelSet = Theory(varKernel),
        examples = examples, aspInputFile = aspFile)
    logger.info("Searching the Kernel Set for a hypothesis")

    val models = ASP.solve("xhail", use2AtomsMap, examples = examples, aspInputFile = aspFile, jep=jep)
    // I only keep the final, most compressive hypothesis.
    // To see and evaluate all discovered hypothese simply iterate over the models
    models foreach println
    val finalModel = models.head.atoms
    println("final:",finalModel)
    getNewRules(finalModel, use2AtomsMap)
  }






}
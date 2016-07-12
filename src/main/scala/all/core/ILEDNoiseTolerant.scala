package all.core

import com.mongodb.casbah.Imports._
import com.mongodb.casbah.commons.MongoDBObject
import com.typesafe.scalalogging.LazyLogging
import all.core.Iled._
import all.parsers.ClausalLogicParser
import java.io.File
import all.structures.Rules._
import all.structures._
import all.structures.Examples.{ExampleBatch, Example}
import all.utils.{ASP, Utils, Database}
import jep.Jep

import scala.collection.immutable.SortedMap

/**
  * Created by nkatz on 1/12/16.
  */

object ILEDNoiseTolerant {

  val startWithEmpty = false
  //val topRuleIndicator = 1000000 // a marker used at ASP-side to discriminate top rules from support set rules

  def main(args: Array[String]): Unit = {
    /*
    val pairs = IledStreaming.getAllPairs(step=40,debug=false)
    println(pairs.size)
    for (p <- pairs) {
      if (p.isStronglyInitiated || p.isStronglyTerminated) {
        println(p.time, if (p.isStronglyInitiated) "initiated" else "terminated")

      }
    }*/
    val db = new Database(Core.fromDB, "examples")
    val jep = new Jep()
    test(db, 2, withWeaks=true, jep=jep)
  }



  def test(DB: Database, batchSize: Int, withWeaks: Boolean, jep: Jep): Unit = {
    Core.glvalues("perfect-fit") = "false"

    val initTheory = if (startWithEmpty) new PriorTheory() else getInitialHypothesis()
    println(initTheory.tostring)
    DB.collection.find().sort(MongoDBObject("time" -> 1)).foldLeft(0, initTheory){
      (x, _) =>
        val startTime = x._1
        val theory = x._2
        val _batch = DB.getBatch(startTime, batchSize, usingWeakExmpls=withWeaks)
        val (batch, endTime) = (_batch._1, _batch._2)
        println(startTime, endTime)
        val e = if (withWeaks){
          batch.exs.size match {
            case 2 => mergePair(batch)
            case _ => throw new RuntimeException("WORKING WITH BATHCES LARGER THAN PAIRS (>2 EXAMPLES) IS NOT SUPPORTED IN THIS VERSION")
          }
        } else {
          batch.asSingleExmpl
        }
        val newTheory = processExample(e, theory, withWeaks=false, jep=jep)
        val merged = newTheory.merge
        score(merged,e,jep)
        (endTime,newTheory)
    }
  }




  def mergePair(batch: ExampleBatch) = {
    // The input to the keepAtom function is an annotation 'atom'
    // originating form the first example in the pair and the
    // 'annotation' of the second example in the pair.
    // 'atom' is parsed into a Literal instance and it's fluent
    // is extracted. If the fluent appears in an atom from 'annotation'
    // then we are dealing with a weak example (i.e. the fluent's truth)
    // value is preserved in the second example by inertia) and since we
    // want to learn from weaks here, 'atom' is discarded in order to
    // defuse inertia. If, on the other hand 'atom's fluent  does not
    // appear in any atom from 'annotation', then the fluent is terminated,
    // so we need to preserve 'atom' in order to learn a termination rule.
    val keepAtom = (atom: String, annotation: List[String]) => {
      val fluent = Literal.toLiteral(atom).terms.head.tostring // the fluent is the first term
      annotation forall (x => !x.contains(fluent))
    }
    val firstExample = batch.exs.head
    val secondExample = batch.exs.tail.head
    val time = Example.f_time(firstExample)
    val narrative = Example.f_narrative(firstExample) ++ Example.f_narrative(secondExample)
    val annotation = Example.f_annot(firstExample).filter(x => keepAtom(x,Example.f_annot(secondExample))) ++ Example.f_annot(secondExample)
    new Example(annot = annotation, nar = narrative, _time = time)
  }

  def processExample(e: Example, priorTheory: PriorTheory, withWeaks: Boolean, jep: Jep): PriorTheory = {
    //val kernel = IledStreaming.generateKernel(e.toMapASP, jep=jep)
    val kernel =
      if (Core.glvalues("specializeOnly").toBoolean && !priorTheory.isEmpty) List[Clause]()
      else IledStreaming.generateKernel(e.toMapASP, jep = jep)

    /*
    if (kernel.nonEmpty) {
      val x = IledStreaming.getAllRefinements(kernel.head).toList
    }
    */
    var theory = new PriorTheory(retainedRules = priorTheory.merge.compress)
    if (!withWeaks) {
      // I am currently expanding this one only (!withWeaks). When the time comes to
      // do experiments with strong and weak examples, the second branch
      // of the if has to be modified. Currently I only pass pairs of examples
      // without prior supervision, to create a base for noise-tolerant learning

      // The idea is to start the learning with a most-general theory consisting
      // of empty-bodied rules, whose heads are head mode declaration atoms.
      // This theory does not have a support set yet, so the first thing to do from
      // the first available example is to populate the support of this seed theory

      val rulesWithPopulatedSupports = Theory(theory.merge.clauses filter (! _.supportSet.isEmpty)).toPriorTheory

      rulesWithPopulatedSupports.isEmpty match {
        case false =>
          if (!isSAT(rulesWithPopulatedSupports.merge,e,ASP.check_SAT_Program,jep = jep)) {
            theory = revise(kernelSet = Theory(kernel), priorTheory = rulesWithPopulatedSupports, example = e, jep = jep)
          } else {
            updateSupport(theory.merge, Theory(kernel), withWeaks)
          }
        case _ => updateSupport(theory.merge, Theory(kernel), withWeaks)
      }
    } else { // currently never getting here
      theory = reviseWithWeaks(Theory(kernel), theory, e, jep = jep)
    }
    show(theory.retainedRules, theory.newRules, theory.refinedRules, e, "forward")
    //Utils.checkIfCompressed(theory)
    theory
  }


  def refine(incRules: List[InconstistentRule]): List[Theory] = {
    // For each inconsistent rule R:
    // First, check each one of the stored refinements, to see if it
    // rejects the negative examples that the rule currently
    // covers. If one does, we are done (we simply update the
    // negative counts for R to use in the future and also the counts
    // for each existing refinement and each support clause).
    //-----------------------------------------------------
    // This is done by the function checkExistingRefs().
    // Also, maybe its better for the existing refinements
    // to be stored as fields of the support set rule they
    // were generated from.
    //-----------------------------------------------------
    // If None of the existing refinements rejects the negative
    // examples, we try to generate new refinements. How will
    // we do that? Further refine each one of the existing refs?
    // I guess so.
    // This is done by the function generateNewRefs().
    //----------------------------------------------------------
    // Perhaps its best to proceed as follows:
    // When a new example arrives we first score everything
    // (theory rules, their support rules and their refs). Via
    // scoring, we identify the inconsistent theory rules.
    // Next we check if an existing refinement rejects the negatives
    // (for the inconsistent rules)
    for (incRule <- incRules) {
     // val (_, use2AtomsMap, defeasiblePrior, use3AtomsMap, _, _) =
     //   ASP.inductionASPProgram(kernelSet=kernelSet,priorTheory=priorTheory.merge,examples=example.toMapASP,aspInputFile=aspFile,withSupport=withSupport,retained=keepIntact.merge)
    }
    List[Theory]()
  }




  def ssUpdate(pt: PriorTheory, e: Example, withWeaks: Boolean, jep: Jep): Unit = {
    val (_, kernel) = Utils.generateKernel(e.toMapASP, jep = jep)
    if (kernel.nonEmpty) {
      updateSupport(pt.merge, Theory(kernel), withWeaks && e.isWeak)
    }
  }

  def score(merged: Theory, e: Example, jep: Jep) = {
    scoreRules(merged,e,jep)
    merged.clauses foreach {
      x => {
        println("top rule:",x.tps,x.fps,x.fns)
        x.supportSet.clauses.foreach {
          y => println("ss rule:",y.tps,y.fps,y.fns)
        }
        println(" ")
      }
    }
  }

  def getInitialHypothesis() = {
    val mheads = Core.modehs.map(x => x.varbed)
    val z = mheads map (x => Clause(head=x,body=List[Literal]()))
    new PriorTheory(retainedRules = Theory(z))
  }

  def getTopKRefs(rule: Clause, e: Example, jep: Jep) = {

  }








  def scoreRules(theory: Theory, example: Example, jep: Jep): Unit = {
    // If annotation is given here, negatives may be covered by inertia.
    // On the other hand, if the annotation is omitted then during inference
    // a correct rule will (correctly) entail positives that will be treated
    // as negatives (due to the lack of annotation). To overcome the issue,
    // the annotation is provided but inertia is defused during inference.
    // This method marks each rule in order to track the derivation of
    // negative examples from the particular rule.
    val e = (example.annotationASP ++ example.narrativeASP).mkString("\n")
    //val exConstr = getCoverageDirectives(checkConsistencyOnly = true).mkString("\n")
    val markInit = "\ninitiatedAt(F,T) :- marked(I,J,initiatedAt(F,T)),rule(I),supportRule(J).\n"
    val markTerm = "\nterminatedAt(F,T) :- marked(I,J,terminatedAt(F,T)),rule(I),supportRule(J).\n"
    val show = "\n#show tps/3.\n#show fps/3.\n#show fns/3."

    val markSSRules =
      (for( (c,i) <- theory.clauses zip List.range(0,theory.clauses.length);
            (cs,j) <- c.supportSet.clauses zip List.range(0,c.supportSet.clauses.length);
            y = cs.withTypePreds();
            markedSSRule = Clause(Literal(functor="marked", terms=List(Constant(i.toString), Constant(j.toString),y.head)),body = y.body))
        yield List(markedSSRule.tostring,s"isRule($i,$j).").mkString("\n")).mkString("\n")

    val markedTheoryRules =
      (for( (c,i) <- theory.clauses zip List.range(0,theory.clauses.length);
            y = c.withTypePreds();
            markedRule = Clause(Literal(functor="marked", terms=List(Constant(i.toString), Constant(1000000.toString), y.head)), body = y.body))
        yield List(markedRule.tostring,s"isRule($i,1000000).").mkString("\n")).mkString("\n")

    val ruleGen = s"rule(0..${theory.clauses.length}).\n"

    val varbedExmplPatterns = for (x <- Core.examplePatterns) yield x.varbed.tostring
    val tps = varbedExmplPatterns.map(x => s"\ntps(I,J,$x):- marked(I,J,$x), example($x),rule(I),supportRule(J),isRule(I,J).\n").mkString("\n")
    val fps = varbedExmplPatterns.map(x => s"\nfps(I,J,$x):- marked(I,J,$x), not example($x),rule(I),supportRule(J),isRule(I,J).\n").mkString("\n")
    val fns = varbedExmplPatterns.map(x => s"\nfns(I,J,$x):- not marked(I,J,$x), example($x),rule(I),supportRule(J),isRule(I,J).\n").mkString("\n")
    // fns are not added
    val directives = List(tps,fps,fns).mkString("\n")

    val ssRuleGne = s"supportRule(0..${theory.clauses.foldLeft(0){
      (x,y) =>
        val newMax = y.supportSet.clauses.length
        if (newMax > x) newMax else x
    }}).\n"

    val markTheoryRulesGen = s"supportRule(1000000).\n" // if J = topRuleIndicator, then its a theory rule (not a support set rule)

    val f = Utils.getTempFile("isConsistent",".lp",deleteOnExit = true)
    Utils.writeToFile(f, "append")(
      p => List(e,directives,markedTheoryRules,markSSRules,markInit,markTerm,s"\n#include "+"\""+Core.bkMarkedFile+"\".\n",ruleGen,ssRuleGne,markTheoryRulesGen,show) foreach p.println
    )
    val path = f.getCanonicalPath
    val answerSet = ASP.solve(task = Core.INFERENCE, aspInputFile = new File(path), jep=jep)
    val out = answerSet match {
      case Nil => ""
      case _ =>
        println(answerSet.head.atoms.mkString(" "))
        val f = (a: String) => {
          val tolit = Literal.toLiteral(a)
          val (i, j) = (tolit.terms.head.tostring.toInt, tolit.terms.tail.head.tostring.toInt)
          (tolit,i,j)
        }
        val g = (x: (Int,Int), what: String) =>{
          val clause = x._2 match {
            case 1000000 => theory.clauses(x._1)
            case _ => theory.clauses(x._1).supportSet.clauses(x._2)
          }
          what match {
            case "tps" => clause.tps += 1
            case "fps" => clause.fps += 1
            case "fns" => clause.fns += 1
          }
        }
        val groupped = answerSet.head.atoms.foldLeft(List[(Int,Int)](),List[(Int,Int)](),List[(Int,Int)]()){
          (x,y) =>
            val (tps,fps,fns) = (x._1,x._2,x._3)
            val z = f(y)
            z._1.functor match {
              case "tps" => (tps :+ (z._2,z._3),fps,fns)
              case "fps" => (tps,fps :+ (z._2,z._3),fns)
              case "fns" => (tps,fps,fns :+ (z._2,z._3) )
            }
        }
        val (tps,fps,fns) = (groupped._1,groupped._2,groupped._3)
        if (fps.nonEmpty) {
          val stop = "stop"
        }
        tps foreach (x => g(x,"tps"))
        fps foreach (x => g(x,"fps"))
        //fns foreach (x => g(x,"fns")) // UNCOMMENT THIS TO GET FNS COUNTS (MAYBE THEY WILL BE NEEDED FOR terminatedAt RULES)
    }

    //out
  }






  /* Only for generating-storing CAVIAR data, not used anywhere else. Run it as follows (e.g.)
   *  ParseCAVIAR.run(ParseCAVIAR.correctedCaviarPath)
   *  assuming that the folders with the data are in place
   *
   *  */

  object ParseCAVIAR extends ClausalLogicParser {

    // We'll maintain two versions of CAVIAR: The first will be the corrected one (where examples at "borderlines"
    // where a fluent changes its value, have been corrected -- pushed a frame forward for initiation, add extra
    // annotated frame for termination). This has happened everywhere (consulting the narrative at the same time),
    // except in cases of more than two persons participating in an HLE (e.g. happensAt( moving( grp_ID0, [ id0, id2, id3 ]), 13440).)
    // The corrected version of CAVIAR is under /dev/CAVIAR-abrupt-corrected-borderlines.
    // The second version is the original one, nothing has been tweaked. It is located under /dev/CAVIAR-abrupt-original

    def run(path: String) = {

      val mongoClient = MongoClient()
      mongoClient.dropDatabase("CAVIAR_Real_FixedBorders")
      val collection = mongoClient("CAVIAR_Real_FixedBorders")("examples")

      val d = new File(path)
      val innerFolders = d.listFiles.sortBy(_.getName.split("-")(0).toInt)
      var lastTime = 0
      for (f <- innerFolders) {

        val files = f.listFiles.filter(x => dataFileNames.exists(p => x.getName.contains(p)))
        val contents =
          (for (f <- files)
            yield scala.io.Source.fromFile(f).getLines().filter(p => !p.startsWith("%"))).toList.flatten.mkString.replaceAll("\\s","").split("\\.").toList

        val parsed = contents.flatMap(x => parseAll(caviarParser(lastTime),x).getOrElse(List(""))).filter(_!="").asInstanceOf[List[Atom]]
        val atoms = SortedMap[Int, List[Atom]]() ++ parsed.groupBy(_.time.toInt)
        for ( (k,v) <- atoms ) {
          val narrative = v.filter(x => !x.annotationAtom).flatMap(z => z.atoms)
          val annotation = v.filter(x => x.annotationAtom).flatMap(z => z.atoms)
          val entry = MongoDBObject("time" -> k) ++ ("annotation" -> annotation) ++ ("narrative" -> narrative)
          collection.insert(entry)
          println(s"inserted $entry")
        }
        lastTime = parsed.last.time.toInt+40
      }
    }

    val hleMapping = Map("moving" -> "moving", "fighting" -> "fighting", "leaving_object" -> "leavingObject", "interacting" -> "meeting")
    val correctedCaviarPath = "/home/nkatz/dev/CAVIAR-abrupt-corrected-borderlines"
    val originalCaviarPath = "/home/nkatz/dev/CAVIAR-abrupt-original"
    val dataFileNames = List("AppearenceIndv","MovementIndv","SituationGrp")

    def word: Parser[String] = """[A-Za-z0-9_]*""".r ^^ { x => x }
    def person: Parser[Person] = "id" ~ number ^^ { case x ~ y => new Person(x + y) }
    def persons: Parser[List[Person]] = "[" ~> repsep(person, ",") <~ "]"
    def time: Parser[String] = number
    def orientationValue: Parser[String] = number
    def appearanceValue: Parser[String] = word
    def coordinates: Parser[(String, String)] = "(" ~ number ~ "," ~ number ~ ")" ^^ { case "(" ~ x ~ "," ~ y ~ ")" => (x, y) }
    def meeting: Parser[String] = "interacting"
    def moving: Parser[String] = "moving"
    def fighting: Parser[String] = "fighting"
    def leavingObject: Parser[String] = "leaving_object"
    def walking: Parser[String] = "walking"
    def active: Parser[String] = "active"
    def inactive: Parser[String] = "inactive"
    def running: Parser[String] = "running"
    def abrupt: Parser[String] = "abrupt"
    def happens: Parser[String] = "happensAt"
    def holds: Parser[String] = "holdsAt"
    def orientation: Parser[String] = "orientation"
    def appearance: Parser[String] = "appearance"
    def coords: Parser[String] = "coord"




    def annotationParser(pastTime: Int): Parser[AnnotationAtom] =
      happens ~ "(" ~ (meeting | moving | fighting | leavingObject) ~ "(" ~ word ~ "," ~ persons ~ ")" ~ "," ~ time ~ ")" ^^ {
      case _~"("~x~"("~_~","~y~")"~","~z~")" => new AnnotationAtom(x, y, (z.toInt+pastTime).toString)
    }
    def lleParser(pastTime: Int): Parser[NarrativeAtom] = happens ~ "(" ~ (walking | active | inactive | running | abrupt) ~ "(" ~ person ~ ")" ~ "," ~ time ~ ")" ^^ {
      case _~"("~lle~"("~p~")"~","~t~")" => new NarrativeAtom(what = lle, id = p.id, time = (t.toInt+pastTime).toString)
    }
    def orientationParser(pastTime: Int): Parser[NarrativeAtom] = holds ~ "(" ~ orientation ~ "(" ~ person ~ ")" ~ "=" ~ number ~ "," ~ time ~ ")" ^^ {
      case _~"("~_~"("~p~")"~"="~v~","~t~")" => new NarrativeAtom(what = "orientation", id = p.id, orientation = v, time = (t.toInt+pastTime).toString)
    }
    def appearanceParser(pastTime: Int): Parser[NarrativeAtom] = holds ~ "(" ~ appearance ~ "(" ~ person ~ ")" ~ "=" ~ word ~ "," ~ time ~ ")" ^^ {
      case _~"("~_~"("~p~")"~"="~v~","~t~")" => new NarrativeAtom(what = "appearance", id = p.id, appearance = v, time = (t.toInt+pastTime).toString)
    }
    def coordsParser(pastTime: Int): Parser[NarrativeAtom] = holds ~ "(" ~ coords ~ "(" ~ person ~ ")" ~ "=" ~ coordinates ~ "," ~ time ~ ")" ^^ {
      case _~"("~_~"("~p~")"~"="~c~","~t~")" => new NarrativeAtom(what = "coord", id = p.id, xcoord = c._1, ycoord = c._2, time = (t.toInt+pastTime).toString)
    }

    //def groupCoordParser: Parser[String] =

    def caviarAtomParser(pastTime: Int): Parser[Atom] = annotationParser(pastTime)|lleParser(pastTime)|orientationParser(pastTime)|appearanceParser(pastTime)|coordsParser(pastTime)
    def caviarParser(pastTime: Int): Parser[List[Atom]] = rep(caviarAtomParser(pastTime))
    class Person(val id: String)

    trait Atom {
      val annotationAtom: Boolean
      val atoms: List[String]
      val time: String
    }

    class AnnotationAtom(val HLE: String, val persons: List[Person], val time: String) extends Atom{
      val annotationAtom = true
      val atoms =
        if (HLE == "leaving_object") {
          List(s"holdsAt(${hleMapping(HLE)}(${persons.head.id},${persons(1).id}),$time)")
        } else {
          persons.toSet.subsets(2).flatMap(y => for (z <- y.toList.permutations) yield s"holdsAt(${hleMapping(HLE)}(${z.head.id},${z(1).id}),$time)").toList
        }
    }

    // what is either an LLE, or orientation, appearance, coord
    class NarrativeAtom(val what: String = "none", val id: String, val xcoord: String = "none",
                        val ycoord: String = "none", val orientation: String = "none",
                        val appearance: String = "none", val time: String) extends Atom{

      val annotationAtom = false
      val atoms = what match {
        case ("walking" | "active" | "inactive" | "running" | "abrupt") => List(s"happensAt($what($id),$time)")
        case "coord" => List(s"coords($id,$xcoord,$ycoord,$time)")
        case "appearance" => appearance match {
          case "appear" | "disappear" => List(s"happensAt($appearance($id),$time)")
          case _ => List(s"holdsAt($appearance($id),$time)")
        }
        case "orientation" => List(s"orientation($id,$orientation,$time)")
      }
    }
  }

}

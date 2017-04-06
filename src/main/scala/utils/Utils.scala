package utils

import java.io.{File, FileWriter, BufferedWriter}
import com.typesafe.scalalogging.LazyLogging
import logic.Examples.{Example, ExamplePair}
import logic.Exceptions.MyParsingException
import parsers.ASPResultsParser
import scala.annotation.tailrec
import scala.math._
import scala.util.Random
import com.mongodb.casbah.Imports._
import scala.collection.mutable.ListBuffer
import scala.io.Source
import logic._


object Utils extends ASPResultsParser with LazyLogging{

   

   /**
    * Transforms input to an ASP program. The program is written in an output file that is passed to the ASP solver.
    * the writeTo file is the only non-optional parameter of the method.
    *
    * @param writeToFile @tparam String path to file where the ASP program is written.
    * @param program @tparam List[String] an (optional) set of ground or non-ground rules and/or ground facts.
    * @param generateDirectives @tparam List[String] an (optional) list containing declarations for atoms to be be generated during the computation
    * of answer sets.
    * @example of such input:
    *
    * List("father(X,Y):person(X):person(Y)","grandfather(X,Y):person(X):person(Y)")
    *
    * Such a list is transformed into the "generate" part of the program:
    *
    * {father(X,Y):person(X):person(Y), grandfather(X,Y):person(X):person(Y)}.
     * @param generateAtLeast @tparam Int an (optional) lower bound for the number of generated atoms to be included in an answer set.
    * @param generateAtMost @tparam Int an (optional) upper bound for the number of generated atoms to be included in an answer set.
    * @param minimizeStatements @tparam List[String] an (optional) list of atoms whose instances in an anser set should be minimized.
    * @example of such input:
    *
    * List("father(X,Y)","grandfather(X,Y)"))
    *
    * Such a list is transformed into a minimize statement:
    *
    * #minimize{father(X,Y),grandfather(X,Y)}.
     * @param maximizeStatements @tparam List[String] similar as above for maximize directives.
    * @param constraints @tparam List[List[String]] a set of integrity constraints. Example:
    *
    * List(List("father(X,Y)","mother(X,Y)"), List("father(X,Y)","not male(X)"))
    *
    * Such input is transformed to integrity constraints in the ASP program:
    *
    * :- father(X,Y), mother(X,Y).
    * :- father(X,Y), not male(X).
     * @param show @tparam List[String] an (optional) list of atoms that are to be displayed. All other atoms in an answer set are hidden.
    * A #hide directive is generated is this list is not empty.
     * @example of such input:
    *
    * List("father(X,Y)","mother(X,Y)") or
    *
    * List("father/2","mother2")
    *
    * Such input is transformed into
    *
    *
    * #hide.
    * #show father(X,Y).
    * #show mother(X,Y)
    * @param extra @tparam List[String] any extra knowledge, that is simply printed in the ASP input file
    */ 

    def toASPprogram(program: List[String] = Nil,
      generateDirectives: List[String] = Nil,
      generateAtLeast: Int = 1000000000,
      generateAtMost: Int = 1000000000,
      minimizeStatements: List[String] = Nil,
      maximizeStatements: List[String] = Nil,
      constraints: List[List[String]] = Nil,
      show: List[String] = Nil,
      extra: List[String] = Nil,
      writeToFile: String) = {

      Utils.clearFile(writeToFile) // clear here, append everywhere else.
      Utils.writeToFile(new java.io.File(writeToFile), "append")(p => program foreach (p.println))
      val genStatems = (generateDirectives, generateAtLeast, generateAtMost) match {
         case x @ (Nil, _, _) => List()
         //case x @ (head :: tail, 1000000000,1000000000) => println(x); x._1.map( y => "{" + y + "}.\n")
         case x @ (head :: tail, 1000000000, 1000000000) => for (e <- x._1) yield "{" + e + "}."
         case x @ (head :: tail, lower, 1000000000) => (head :: tail).map(y => "$lower {" + y + "}.\n")
         case x @ (head :: tail, 1000000000, upper) => (head :: tail).map(y => "0 {" + y + "} $upper.\n")
         case x @ (head :: tail, lower, upper) => (head :: tail).map(y => "$lower {" + y + "} $upper.\n")
      }
      Utils.writeToFile(new java.io.File(writeToFile), "append")(p => genStatems foreach (p.println))
      val minStatement = minimizeStatements match { // This is a single string
         case Nil => ""
         case _ => "#minimize{ " + minimizeStatements.mkString(",") + "}.\n"
      }
      val maxStatement = maximizeStatements match { // This is a single string
         case Nil => ""
         case _ => "#maximize{ " + maximizeStatements.mkString(",") + "}.\n"
      }
      val constrs = constraints match { // This is a list of strings
         case Nil => List("")
         case _ => for (x <- constraints) yield ":- " + x.mkString(",") + ".\n"
      }
      Utils.writeLine(minStatement, writeToFile, "append")
      Utils.writeLine(maxStatement, writeToFile, "append")
      Utils.writeToFile(new java.io.File(writeToFile), "append")(p => constrs foreach (p.println))
      val (hideDir, showDirs) = show match {
         case Nil => ("", List(""))
         case _ => ("", (for (x <- show) yield "#show " + x + "."))
      }
      Utils.writeLine(hideDir, writeToFile, "append")
      Utils.writeToFile(new java.io.File(writeToFile), "append")(p => extra foreach (p.println))
      Utils.writeToFile(new java.io.File(writeToFile), "append")(p => showDirs foreach (p.println))
   }
   
   

   


  def isSubset(x: Set[Any], y: Set[Any]): Boolean = x subsetOf y


  /**
   *
   ****************
   * File IO Utils
   ****************
   *
  */

   def clearFile(file: String): Unit = {
      val writer = new java.io.PrintWriter(new FileWriter(new java.io.File(file), false))
      writer.write("")
      writer.close()
   }

   def createOrClearFile(path: String): String = {
      val myFile = new java.io.File(path);
      if (!myFile.exists()) {
         myFile.createNewFile();
      } else {
         clearFile(path)
      }
      path
   }

   def writeToFile(f: java.io.File, howTowrite: String)(op: java.io.PrintWriter => Unit) {
      // Write an iterable to file. Usage:
      //writeToFile(new File("example.txt")) { p => data.foreach(p.println) }
      val p = howTowrite match {
         case "append" => new java.io.PrintWriter(new FileWriter(f, true))
         case "overwrite" => new java.io.PrintWriter(new FileWriter(f, false))
         case _ => new java.io.PrintWriter(new FileWriter(f, false)) // default is overwrite
      }
      try { op(p) } finally { p.close() }
   }

   def readFiletoList(file: String): List[String] = {
      Source.fromFile(file).getLines.toList
   }

   def readFileToString(file: String) = {
      Source.fromFile(file).mkString.replaceAll("\\s", "")
   }

  def getTempFile(prefix: String, suffix: String,
                  directory: String = "",
                  deleteOnExit: Boolean = true): File = {

    var file: java.io.File = new java.io.File("")
    directory match {
      case "" => file = java.io.File.createTempFile(prefix,suffix)
      case _ => file = java.io.File.createTempFile(prefix,suffix,new java.io.File(directory))
    }
    if (deleteOnExit) file.deleteOnExit()
    file
  }
   
   def writeLine(in: String, file: String, append: String): Unit = {
      val w = append match {
         case "append" => new BufferedWriter(new FileWriter(file, true))
         case "overwrite" => new BufferedWriter(new FileWriter(file, false))
         case _ => throw new RuntimeException("Specify append or overwrite")
      }
      w.write(in)
      w.close()
   }


  def getInnerDirs(dir: String):List[File] = {
    val d = new File(dir)
    if (d.exists && d.isDirectory) {
      d.listFiles.filter(_.isDirectory).toList
    } else {
      List[File]()
    }
  }

  def getInnerFiles(dir: String):List[File] = {
    val d = new File(dir)
    if (d.exists && d.isDirectory) {
      d.listFiles.filter(_.isFile).toList
    } else {
      List[File]()
    }
  }












   /*
	 * -------------------------------------------------------------------------------------------
	 * Utilities for connecting to mongo and getting examples. Fixes for locking below.
	 * -------------------------------------------------------------------------------------------
	 * 
	 * Fix mongo connectivity issues: 
	 *                                                              
	 * -- Manually remove the lockfile: sudo rm /var/lib/mongodb/mongod.lock  
	 *                      
	 * -- Run the repair script: sudo -u mongodb mongod -f /etc/mongodb.conf --repair 
	 *              
	 * -- Start your MongoDB server with sudo start mongodb and verify it is running with sudo 
	 *     
	 *    status mongodb and by trying to connect to it with mongo test.
	 * 
	 * --------------------------------------------------------------------------------------------
	 */


   
   
   



   def getAllDBs(identifier: String = ""): List[String] = {
      //Returns the names of all existing DBs. Identifier (if passed) is used to filter the names.
      val mongoClient = MongoClient()
      val all = mongoClient.databaseNames.filter(x => x.contains("CAVIAR-")).toList.sortBy { x => x.split("-")(1).toInt }
      all.foreach(x => mongoClient(x)("examples"))
      mongoClient.close()
      all
   }


   def getAllExamples(db: String, collection: String, 
                      alternativeAspFile: String = ""): Map[String,List[String]] = {
      
      // Get all examples from a DB and write them to ASP input files.
      // Returns true if the current DB has supervision, else false
      val mongoClient = MongoClient()
      val col = mongoClient(db)(collection)
      //val col = MongoClient()(db)(collection)
      val out = examplestoASP("all", "all", col, alternativeAspFile)
      mongoClient.close()
      out
   }
   /*
	 * Get one example
	 */
   def getOneExample(field: String, fieldValue: Any, collection: MongoCollection): Any = {
      examplestoASP(field, fieldValue, collection)
   }

   /**
    * Returns a Structures.ExamplePair object from a pair of two consecutive examples.
    * Here field is time, fieldValueStart and fieldValueEnd are integers.
    */
   
   def getExamplePairs(field: String, fieldValueStart: Int,
    fieldValueEnd: Int, collection: MongoCollection): Option[ExamplePair] = {
      
      for {
         e1 <- collection.findOne(MongoDBObject(field -> fieldValueStart))
         e2 <- collection.findOne(MongoDBObject(field -> fieldValueEnd)) 
      } yield ExamplePair(Example(e1), Example(e2))
      

      
      /*
       * The above does the same at this:
       * 
       * collection.findOne(MongoDBObject(field -> fieldValueStart)).flatMap { 
       * e1 =>  collection.findOne(MongoDBObject(field -> fieldValueEnd)) .map { e2 => ExamplePair(Example(e1), Example(e2)) }}
       * 
       * The first part:
       * 
       * collection.findOne(MongoDBObject(field -> fieldValueStart))
       * 
       * executes the query field -> fieldValueStart and returns an Option[DBObject]
       * 
       * To that, we apply the faltMap function (to the )
       */
      
   }
     
   
   def examplestoASP(field: String, 
                     fieldValue: Any, 
                     collection: MongoCollection, 
                     alternativeAspFile: String = ""): Map[String,List[String]] = {
      
      var annotation = new ListBuffer[String]()
      var narrative = new ListBuffer[String]()
      field match {
         case "all" =>
            for (x <- collection.find().sort(MongoDBObject("time" -> 1))) {
               annotation = annotation ++ x.asInstanceOf[BasicDBObject].get("annotation").asInstanceOf[BasicDBList].toList.map(x => s"example($x).")
               narrative = narrative ++ x.asInstanceOf[BasicDBObject].get("narrative").asInstanceOf[BasicDBList].toList.map(x => s"$x.")
            }

         case "\\s" => throw new RuntimeException("Which example do you want?")
         case _ => fieldValue match {
            case "\\s" => throw new RuntimeException("Which example do you want?")
            case "all" => throw new RuntimeException("Excecution should not have reached this code")
            case _ =>
               val query = MongoDBObject(field -> fieldValue)
               try {
                  val target = collection.findOne(query).get
                  annotation = annotation ++ target.asInstanceOf[BasicDBObject].get("annotation").asInstanceOf[BasicDBList].toList.map(x => s"example($x).");
                  narrative = narrative ++ target.asInstanceOf[BasicDBObject].get("narrative").asInstanceOf[BasicDBList].toList.map(x => s"$x.");
                  //write((annotation, narrative))
               } catch {
                  case e: NoSuchElementException =>
                     println(s"The example with \'field -> value\' : \'$field -> $fieldValue\' does not exist")

                     //System.exit(-1)
               }

         }
      }
      //return if (annotation.length > 0) true else false
      return Map("annotation" -> annotation.toList, "narrative" -> narrative.toList)
   }

   def computeDistancesMany(supervision: List[List[String]], collection: MongoCollection): Unit = {
      collection.find().sort(MongoDBObject("time" -> 1)).foreach(row =>
         try {
            for (x <- supervision) print(Hausdorff.exmplHausdrfDist(x, f(row)) + " ") 
            println("\n")
         } catch {
            case e: MyParsingException => f(row).foreach(println); println(e); throw new RuntimeException
         })
   }

   def computeDistances(realExmpl: List[String], collection: MongoCollection): Unit = {
      collection.find().sort(MongoDBObject("time" -> 1)).foreach(row =>
         try {
            println(Hausdorff.exmplHausdrfDist(realExmpl, f(row))) 
         } catch {
            case e: MyParsingException => f(row).foreach(println); println(e); throw new RuntimeException
         })
   }

   /**
    * Helper method
    */
   def f(row: Any): List[String] = {
      val x = row.asInstanceOf[BasicDBObject].get("narrative").asInstanceOf[BasicDBList].toList
      val y = row.asInstanceOf[BasicDBObject].get("annotation").asInstanceOf[BasicDBList].toList
      val example = x ++ y map { x => x.asInstanceOf[String] }
      example
   }
   /**
    * Get an example as an interpretation in a list
    */
   def getExample(field: String, fieldValue: Any, collection: MongoCollection): List[String] = {
      val query = MongoDBObject(field -> fieldValue)
      val target = collection.findOne(query) match {
         case Some(x) => x
         case _ => List()
      }
      val narrative = target.asInstanceOf[BasicDBObject].get("narrative").asInstanceOf[BasicDBList].toList
      val annotation = target.asInstanceOf[BasicDBObject].get("annotation").asInstanceOf[BasicDBList].toList
      val oneExample = narrative ++ annotation map { x => x.asInstanceOf[String] }
      oneExample
   }

   /* Get a simple string as result, from the field of interest */
   def getStringByField(field: String, fieldValue: Any, collection: MongoCollection): Any = {
      val query = MongoDBObject(field -> fieldValue)
      val target = collection.findOne(query)
      val result = target match {
         case Some(x) => target.get(field)
         case _ => None
      }
      result
   }

   /* Returns a query whose result is an array */
   def getArrayByField(field: String, fieldValue: Any, collection: MongoCollection): Any = {
      val query = MongoDBObject(field -> fieldValue)
      val target = collection.findOne(query).get
      //val result = target match {
      //  case Some(x) => target.getAs[MongoDBList](field).get
      //}
      //result
   }

   /* Returns the first entry. Works for data stored as arrays*/
   def getOneArray(collection: MongoCollection): List[String] = {
      val target = collection.findOne().asInstanceOf[BasicDBList]

      val result = Some(List[String]() ++ target map { x => x.asInstanceOf[String] }) match {
         case Some(z) => z
         case _ => List[String]()
      }
      result

   }

   
   /**
    * Time a function. Usage: simply wrap around a block of code. E.g:
    * 
    * val hd = time { exmplHausdrfDist(labled.asVarbedInterpretation, z._2.toStrList) }
    * 
    */
   
   def _time[R](block: => R): R = {
      val t0 = System.nanoTime()
      val result = block // call-by-name
      val t1 = System.nanoTime()
      println("Time: " + ((t1 - t0)/1000000000.0) + " sec")
      result
   }

  def time[R](block: => R): (R,Double) = {
    val t0 = System.nanoTime()
    val result = block // call-by-name
    val t1 = System.nanoTime()
    val totalTime = (t1 - t0)/1000000000.0
    (result,totalTime)
  }

  def mywhile(cond : => Boolean, block : => Unit) : Unit =
    if(cond) {
      block
      mywhile(cond, block)
    }

  def lined(msg: String) = s"\n$msg\n${"-"*msg.length}"

  def mean(s: List[Double]) = s.foldLeft(0.0)(_ + _) / s.size

  def deviation(s: List[Double], mean: Double) = {
    val diffs = s map (x => math.abs(x - mean))
    this.mean(diffs)
  }

  def combinations(n: Int, k: Int) = {
    if (n >= k) factorial(n)/(factorial(k)*factorial(n-k)) else BigInt(0)
  }

  def factorial(x: BigInt): BigInt = {
    @tailrec
    def f(x: BigInt, acc: BigInt): BigInt = {
      if (x == 0) acc else f(x - 1, x * acc)
    }
    f(x, 1)
  }

  def sampleN(N: Int, sampleFrom: List[Any]) = {
    @tailrec
    def sampleN(N: Int, sampleFrom: List[Any], sample: List[Any]): List[Any] = {
      sample.length match {
        case N => sample
        case _ =>
          val newValue = Random.shuffle(sampleFrom).head
          val newSample = if (!sample.contains(newValue)) sample :+ newValue else sample
          sampleN(N, sampleFrom, newSample)
      }
    }
    sampleN(N,sampleFrom,List())
  }

  def checkIfCompressed(theory: PriorTheory) = {
    val compressed = LogicUtils.compressTheory(theory.merge.clauses)
    if(compressed.length != theory.merge.clauses.length) {
      logger.error("You're packing the same rules")
      val sames = for (x <- theory.merge.clauses) yield (x,theory.merge.clauses.find(y => y!=x && x.thetaSubsumes(y) && y.thetaSubsumes(x)))
      val p = sames map {
        x => x._2 match {
          case Some(y) => s"Same pair: ${x._1.tostring} from weak: ${x._1.fromWeakExample}\n${y.tostring} from weak: ${y.fromWeakExample}"
          case None => None
        }
      }
      logger.error(s"same rules: $p")


    }
  }








  /* Returns the maximum Hausdorff distance of this clause from a list of clauses */
  def similarity(c: Clause, x: List[Clause]) = {
    val dists = x.filter(_.body.nonEmpty).foldLeft(List[Double]()){ (accum,newClause) =>
      //val sim = Hausdorff.litlistFromlitlist(c.literals,newClause.literals)
      val sim = Hausdorff.litlistFromlitlist(c.body,newClause.body)
      accum :+ sim
    }
    //println(dists)
    if (dists.nonEmpty) dists.min else -100.0
  }


  def hoeffding(delta: Double, n: Int) = {
    sqrt(scala.math.log(1.0 / delta) / (2 * n))
    // For the following, check p.3 of
    // Rutkowski, Leszek, et al. "Decision trees for mining data streams based on the McDiarmid's bound."
    // IEEE Transactions on Knowledge and Data Engineering 25.6 (2013): 1272-1279.
    // (this is McDiarmid’s inequality)
    //----------------------------------------------------------
    // 6*(2*Math.log(Math.E*2) + Math.log(2*2)) + 2*Math.log(2)
    //----------------------------------------------------------
  }



}
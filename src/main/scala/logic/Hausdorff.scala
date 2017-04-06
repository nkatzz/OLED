package logic

import parsers.ClausalLogicParser

/**
 * Compute the Haussdorff distance between Herbrand Interpretations.
 */

object Hausdorff extends ClausalLogicParser {

  /*
   def main(args: Array[String]) {
      //val x1 = "initiatedAt(fighting(id1,id2),X)"
      //val x2 = "initiatedAt(fighting(id1,id3),X)"
      //println(hausdrfDist(x1, x2))
      /*
    val atom = "a(1,2,4)"
    val example1 = List("p(a(2,6,4),Y)","a(1,2,4)")
    val example2 = List("a(1,2,3)","p(a(2,3,4),X)","q(z,Term)")
    println(elemFromExampleDist(atom,example2))
    println(exmplFromExmplDist(example1,example2))
    * 
    */
      val e1 = List("happensAt(walking(id0),680)", "coords(id0,262,285,680)", "happensAt(enters(id0),680)", "orientation(id0,0,680)")
      val e2 = List()
   }
*/
  def litsHausdrfDist(l1: List[Literal], l2: List[Literal]): Double = {
    math.max(litlistFromlitlist(l1, l2), litlistFromlitlist(l2, l1))
  }

  def litlistFromlitlist(l1: List[Literal], l2: List[Literal]): Double = {
    val d = for (x <- l1; d = literalFromLiteralList(x, l2)) yield d
    d.reduceLeft(_ max _)
  }

  def literalFromLiteralList(lit: Literal, list: List[Literal]): Double = {
    val distances = for (z <- list; d = litHausdrfDist(lit, z)) yield d
    distances.reduceLeft(_ min _)
  }

  def exmplHausdrfDist(ex1: List[String], ex2: List[String]): Double = {
    math.max(exmplFromExmplDist(ex1, ex2), exmplFromExmplDist(ex2, ex1))
  }

  def litHausdrfDist(x: Literal, y: Literal): Double = {
    hausdrfDist1(x.asInstanceOf[Expression], y.asInstanceOf[Expression])
  }

  def exmplFromExmplDist(exmpl1: List[String], exmpl2: List[String]): Double = {
    val d = for (x <- exmpl1; d = elemFromExampleDist(x, exmpl2)) yield d
    d.reduceLeft(_ max _)
  }

  def elemFromExampleDist(atom: String, example: List[String]): Double = {
    val distances = for (z <- example; d = hausdrfDist(atom, z)) yield d
    distances.reduceLeft(_ min _)
  }

  def hausdrfDist(x: String, y: String): Double = {
    val x1 = getParseResult(parse(literal, x))
    val x2 = getParseResult(parse(literal, y))
    hausdrfDist1(x1, x2)
  }



/*
  def hausdrfDist(x: Literal, y: Literal): Double = {
    hausdrfDist1(x, y)
  }

  def hausdrfDist(x: Clause, y: Clause): Double = {
    hausdrfDist1(x.literals, y.literals)
  }
*/
  def hausdrfDist1[T <: Expression](x: T, y: T): Double = (x, y) match {
    case (x: Literal, y: Literal) =>
      val d = haussdorffOuter(x, y)
      if (x.functor == y.functor && x.arity == y.arity) {
        d / (2 * x.arity)
      } else {
        d
      }
  }

  def haussdorff(x: Expression, y: Expression, accum: Double, rest: List[Tuple2[Expression, Expression]]): Double = (x, y, rest) match {
    case (x: Variable, y: Variable, hd :: tail) =>
      if (x.name == y.name) haussdorff(hd._1, hd._2, accum + 0.0, tail)
      else haussdorff(hd._1, hd._2, accum + 1.0, tail)
    case (x: Variable, y: Variable, List()) =>
      if (x.name == y.name) accum + 0.0
      else accum + 1.0
    case (x: Constant, y: Constant, hd :: tail) =>
      if (x.name == y.name) haussdorff(hd._1, hd._2, accum + 0.0, tail)
      else haussdorff(hd._1, hd._2, accum + 1.0, tail)
    case (x: Constant, y: Constant, List()) =>
      if (x.name == y.name) accum + 0.0 else accum + 1.0
    case (x: Literal, y: Literal, hd :: tail) =>
      if (x.functor != y.functor | x.arity != y.arity) {
        haussdorff(hd._1, hd._2, accum + 1.0, tail)
      } else {
        val currentDist = haussdorffOuter(x, y)
        haussdorff(hd._1, hd._2, accum + currentDist, tail)
      }
    case (x, y, List()) =>
      val currentDist = haussdorffOuter(x, y)
      accum + currentDist
    case (_, _, hd :: tail) =>
      haussdorff(hd._1, hd._2, accum + 1.0, tail)
    case (_, _, List()) =>
      1.0
  }

  def haussdorffOuter(x: Expression, y: Expression): Double = (x, y) match {
    case (x: Literal, y: Literal) =>
      if (x.functor != y.functor | x.arity != y.arity) {
        1.0
      } else {
        val hd :: tail = x.terms zip y.terms
        haussdorff(hd._1, hd._2, 0.0, tail)
      }
    case (x: Variable, y: Variable) =>
      if (x.name == y.name) 0.0 else 1.0
    case (x: Constant, y: Constant) =>
      if (x.name == y.name) 0.0 else 1.0
    case (_, _) => 1.0
  }

}
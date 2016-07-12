package all.structures

/**
  * Created by nkatz on 2/5/16.
  */

/*
  A refinement instance represents a set of specializations that may be
  generated from a bottom clause BC. It contains a set of rules (the specializations),
  which are bounded below by BC and above by a most-general specialization (MGS) of the BC.

  This class is used by the noise-tolerant version of ILED. The information stored in a
  Refinement object is indicative of how the noise-tolerant ILED works: Initially, the MGS of any BC
  (i.e. any kernel set clause that may be generated from an example) is a body-less rule
  with a modeh atom in the head. Once MGS covers a negative example, we try to specialize MGS,
  provided of course that its support set is non-empty. To this end, all minimal specializations
  of MGS that reject the negative example are generated and stored in the Specializations field
  of the the Refinement object.
*/

class Refinement(val MGS: Clause, val bottomClause: Clause) {
  var Specializations = List[Clause]()
  val allRules = List(MGS) ++ Specializations // just a helper

}

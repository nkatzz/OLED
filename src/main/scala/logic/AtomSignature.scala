package logic

/**
 * Created by nkatz at 6/10/19
 */

class AtomSignature(val predSymbol: String, arity: Int) {

  def tostring = s"$predSymbol/$arity"

}

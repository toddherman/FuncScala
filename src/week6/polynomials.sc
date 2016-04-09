package week6

object polynomials {

  // a class to represent polynomials as maps
  // map   exponent (Int), coefficient(Double)
  // instead of "val" terms field,
  class Poly(terms0: Map[Int, Double]) {
    // accomodate multiple arguments using an auxillary constructor that takes a sequence * of bindings
    def this(bindings: (Int, Double)*) = this(bindings.toMap)
    //	create field here with default value
    val terms = terms0 withDefaultValue 0.0

    // using foldLeft to compare efficiency
    def +(other: Poly) = new Poly((other.terms foldLeft terms)(addTerm))
    def addTerm(terms: Map[Int, Double], term: (Int, Double)): Map[Int, Double] = {
      // pull out the key
      val (exp, coeff) = term
      // foldleft more effiecient because
      //each of these bindings will be immediately added terms map
      // so we built up the result directly
      // whereas, before we would create an another list of terms that contain the adjusted terms,
      // and then we would concatenate this list to the original one
      terms + (exp -> (coeff + terms(exp)))
    }

    // merge coefficients that have the same exponent
    override def toString =
      // join all these bits together as a more friendly string
      // take co and ex of each term, yield a fancy string
      // add sorting
      (for ((exp, coeff) <- terms.toList.sorted.reverse) yield coeff + "x^" + exp) mkString " + "
  }

  // can remove map because that is achieved in new bindings above
  val p1 = new Poly(1 -> 2.0, 3 -> 4.0, 5 -> 6.2) //> p1  : week6.polynomials.Poly = 6.2x^5 + 4.0x^3 + 2.0x^1
  val p2 = new Poly(0 -> 3.0, 3 -> 7.0)           //> p2  : week6.polynomials.Poly = 7.0x^3 + 3.0x^0
  p1 + p2                                         //> res0: week6.polynomials.Poly = 6.2x^5 + 11.0x^3 + 2.0x^1 + 3.0x^0
  p1.terms(3)                                     //> res1: Double = 4.0
}
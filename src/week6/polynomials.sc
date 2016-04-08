package week6

object polynomials {

  // a class to represent polynomials as maps
  // map   exponent (Int), coefficient(Double)
  class Poly(val terms: Map[Int, Double]) {
    
    
    // implement a Plus operation
    def +(other: Poly) = new Poly(terms ++ other.terms)
    // merge coefficients that have the same exponent
    override def toString =
      // join all these bits together as a more friendly string
      // take co and ex of each term, yield a fancy string
      // add sorting
      (for ((exp, coeff) <- terms) yield coeff + "x^" + exp) mkString " + "
  }

  val p1 = new Poly(Map(1 -> 2.0, 3 -> 4.0, 5 -> 6.2))
                                                  //> p1  : week6.polynomials.Poly = 2.0x^1 + 4.0x^3 + 6.2x^5
  val p2 = new Poly(Map(0 -> 3.0, 3 -> 7.0))      //> p2  : week6.polynomials.Poly = 3.0x^0 + 7.0x^3
  p1 + p2                                         //> res0: week6.polynomials.Poly = 2.0x^1 + 7.0x^3 + 6.2x^5 + 3.0x^0
}
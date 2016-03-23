package week2_1

object rationals {

  val x = new Rational(1, 3)                      //> x  : week2_1.Rational = 1/3
  val y = new Rational(5, 7)                      //> y  : week2_1.Rational = 5/7
  val z = new Rational(3, 2)                      //> z  : week2_1.Rational = 3/2

  x.numer                                         //> res0: Int = 1
  x.denom                                         //> res1: Int = 3
  x + y                                           //> res2: week2_1.Rational = 22/21
  -x                                              //> res3: week2_1.Rational = 1/-3
  x - y - z                                       //> res4: week2_1.Rational = -79/42
  y + y                                           //> res5: week2_1.Rational = 10/7
  x < y                                           //> res6: Boolean = true
  x max y                                         //> res7: week2_1.Rational = 5/7
}

// using infix notation and relaxed identifiers to allow rational numbers to be used like Int or Double
// re: relaxed identifiers:  of significance are symbolic, operator symbols.  See notes on implied precedence.
class Rational(x: Int, y: Int) {
  require(y != 0, "denominator must be non-zero")

  private def gcd(a: Int, b: Int): Int = if (b == 0) a else gcd(b, a % b)

  // do not calculate gcd immediately
  private val g = gcd(x, y)
  def numer = x
  def denom = y

  // is this rational number less than that?
  def <(that: Rational) = numer * that.denom < that.numer * denom

  // is "this" rational number less than that?
  // Note self reference.  The object on which the current method is executed.
  def max(that: Rational) = if (this < that) that else this
  def +(that: Rational) =
    new Rational(
      numer * that.denom + that.numer * denom,
      denom * that.denom)
  // Because the prefix operator minus is really different from the infix operator minus,
  // there is a special convention in Scala. We have to call it unary minus
  def unary_- : Rational = new Rational(-numer, denom)

  def - (that: Rational) = this + -that

  // modification to apply simplification when numbers are converted to strings
  override def toString = {
    val g = gcd(numer, denom)
    numer / g + "/" + denom / g
  }

}
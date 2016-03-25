package week3

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
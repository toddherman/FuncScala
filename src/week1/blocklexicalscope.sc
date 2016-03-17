package week1

object blocklexicalscope {

  def sqrt(x: Double) = {
    def sqrtIter(guess: Double, x: Double): Double =
      if (isGoodEnough(guess, x)) guess
      else sqrtIter(improve(guess, x), x)

    def isGoodEnough(guess: Double, x: Double) =
      math.abs(guess * guess - x) / x < 0.001

    def improve(guess: Double, x: Double): Double =
      (guess + x / guess) / 2

    sqrtIter(1.0, x)
  }                                               //> sqrt: (x: Double)Double
  sqrt(2)                                         //> res0: Double = 1.4142156862745097
  sqrt(4)                                         //> res1: Double = 2.000609756097561
  sqrt(1e-6)                                      //> res2: Double = 0.0010000001533016628
  sqrt(1e60)                                      //> res3: Double = 1.0000788456669446E30

  // Since x is actually visible inside all these nested functions,
  // avoid the redundancy of passing it as a parameter.
  // Much cleaner, same results.
  def sqrtClean(x: Double) = {
    def sqrtIter(guess: Double): Double =
      if (isGoodEnough(guess)) guess
      else sqrtIter(improve(guess))

    def isGoodEnough(guess: Double) =
      math.abs(guess * guess - x) / x < 0.001

    def improve(guess: Double): Double =
      (guess + x / guess) / 2

    sqrtIter(1.0)
  }                                               //> sqrtClean: (x: Double)Double
  sqrt(2)                                         //> res4: Double = 1.4142156862745097
  sqrt(4)                                         //> res5: Double = 2.000609756097561
  sqrt(1e-6)                                      //> res6: Double = 0.0010000001533016628
  sqrt(1e60)                                      //> res7: Double = 1.0000788456669446E30


14%21                                             //> res8: Int(14) = 14
}
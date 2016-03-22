package week2

object rationals {
  val x = new Rational(1, 2)                      //> x  : week2.Rational = 1/2
  x.numer                                         //> res0: Int = 1
  x.denom                                         //> res1: Int = 2
  
  val y = new Rational(2, 3)                      //> y  : week2.Rational = 2/3
  x.add(y)                                        //> res2: week2.Rational = 7/6
}

class Rational(x: Int, y: Int) {
  def numer = x
  def denom = y
  
  def add(that: Rational) =
  new Rational(
  	numer * that.denom + that.numer + denom,
  	denom * that.denom)
  	
  override def toString = numer + "/" + denom
  
}
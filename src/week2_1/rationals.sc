package week2_1

object rationals {
  
  val x = new Rational(1, 3)                      //> x  : week2_1.Rational = 1/3
  val y = new Rational(5, 7)                      //> y  : week2_1.Rational = 5/7
  val z = new Rational(3, 2)                      //> z  : week2_1.Rational = 3/2
  
  x.numer                                         //> res0: Int = 1
  x.denom                                         //> res1: Int = 3
  x.add(y)                                        //> res2: week2_1.Rational = 22/21
  x.neg                                           //> res3: week2_1.Rational = 1/-3
  x.sub(y).sub(z)                                 //> res4: week2_1.Rational = -79/42
  y.add(y)                                        //> res5: week2_1.Rational = 10/7
  x.less(y)                                       //> res6: Boolean = true
  x.max(y)                                        //> res7: week2_1.Rational = 5/7
}

// added simplification of rational numbers using DRY
// gcd =  greatest common devisor
class Rational(x: Int, y: Int) {
  private def gcd(a: Int, b: Int): Int = if(b == 0) a else gcd(b, a % b)
  // calculate gcd immediately so it's value can be reused
  private val g = gcd(x, y)
  def numer = x / g
  def denom = y / g
  
  // alternate examples to illustrate data abstraction
  
  // advantageous if numer and denom are called infrequently
  //def numer = x / gcd(x, y)
  //def denom = y / gcd(x, y)
  
// turn them into vals so they are called only once
// adventageous if they are called VERY often
  //val numer = x / gcd(x, y)
  //val denom = y / gcd(x, y)
  
  // is this rational number less than that?
  def less(that: Rational) = numer * that.denom < that.numer * denom
  
  // is "this" rational number less than that?
  // Note self reference.  The object on which the current method is executed.
  def max(that: Rational) = if (this.less(that)) that else this
  def add(that: Rational) =
  	new Rational(
  		numer * that.denom + that.numer * denom,
  		denom * that.denom)
  	
  def neg() = {
   	new Rational(-numer, denom)
   }
   
  
  def sub(that: Rational) = add(that.neg)
  
  override def toString = numer + "/" + denom
  
}
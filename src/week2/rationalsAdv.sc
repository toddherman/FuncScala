package week2

// a more advanced approach.

// add neg class
// add subtract class
// find x - y - z
object rationalsAdv {
  val x = new RationalAdv(1, 3)                   //> x  : week2.RationalAdv = 1/3
  val y = new RationalAdv(5, 7)                   //> y  : week2.RationalAdv = 5/7
  val z = new RationalAdv(3, 2)                   //> z  : week2.RationalAdv = 3/2
  x.numer                                         //> res0: Int = 1
  x.denom                                         //> res1: Int = 3
  x.add(y)                                        //> res2: week2.RationalAdv = 15/21
  x.sub(y).sub(z)                                 //> res3: week2.RationalAdv = 28/42
}

class RationalAdv(x: Int, y: Int) {
  def numer = x
  def denom = y
  
  def add(that: RationalAdv) =
  	new RationalAdv(
  		numer * that.denom + that.numer + denom,
  		denom * that.denom)
  	
  def neg: RationalAdv = new RationalAdv(-numer, denom)
  
  def sub(that: RationalAdv) = add(that.neg)
  
  override def toString = numer + "/" + denom
}
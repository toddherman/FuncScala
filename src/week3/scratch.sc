// using an import class instead.  Makes following reference mere succicnt.
import week3.Rational
// Underscore is wildcard, means import everything.
// import week3._

// Braces mean import named.
// import week3.{Rational, Hello}

object scratch {
  //refer to external class by includeing package name
  new Rational(1, 2)                              //> res0: week3.Rational = 1/2

  def error(msg: String) = throw new Error(msg)   //> error: (msg: String)Nothing

  // will show an eception trace
  // error("test")

  val x = null                                    //> x  : Null = null
  val y: String = x                               //> y  : String = null
  
  // can't do this...
  // val z: Int = null
  // type mismatch
  
  if (true) 1 else false                          //> res1: AnyVal = 1
  // Why does this return "AnyVal"?
  // returns Int or Boolean, but those two types don't match
  // See Class Hierarchy graphic.
}
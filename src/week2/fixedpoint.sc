package week2


// for some functions f, we cal locate the fixed points by startting with
// and intial extimate and then iteratively applying f
// this is a close derivative of what was previously done for sqrt
object fixedpoint {
  val tolerance = 0.0001                          //> tolerance  : Double = 1.0E-4
  def isCloseEnough(x: Double, y: Double) =
  math.abs((x - y)/x) / x < tolerance             //> isCloseEnough: (x: Double, y: Double)Boolean
  def fixedPoint(f: Double => Double)(firstGuess: Double) = {
  def iterate(guess: Double): Double = {
  println(guess)
  val next = f(guess)
  if (isCloseEnough(guess, next)) next
  else iterate(next)
  }
  iterate(firstGuess)
  }                                               //> fixedPoint: (f: Double => Double)(firstGuess: Double)Double
  
  // try it for the given function with intial value 1
  fixedPoint(x => 1 + x/2)(1)                     //> 1.0
                                                  //| 1.5
                                                  //| 1.75
                                                  //| 1.875
                                                  //| 1.9375
                                                  //| 1.96875
                                                  //| 1.984375
                                                  //| 1.9921875
                                                  //| 1.99609375
                                                  //| 1.998046875
                                                  //| 1.9990234375
                                                  //| 1.99951171875
                                                  //| res0: Double = 1.999755859375
  // you get something close to 2 as expected.
  
  // attempt to use this for sqrt
  // fails in loop because of result oscillation
  // def sqrt(x: Double) = fixedPoint(y => x / y)(1)
  // sqrt(2) // FAIL in loop of 1,2,1,2,1,2,1,2
  
  // dampen using average.
  def sqrt(x: Double) = fixedPoint(y => (y + x / y)/2)(1)
                                                  //> sqrt: (x: Double)Double
  sqrt(2)                                         //> 1.0
                                                  //| 1.5
                                                  //| 1.4166666666666665
                                                  //| 1.4142156862745097
                                                  //| res1: Double = 1.4142135623746899
  
  // abstract the average function
  def averageDamp(f: Double => Double)(x: Double) = (x + f(x))/2
                                                  //> averageDamp: (f: Double => Double)(x: Double)Double
  
  
  // exercise
  // given fixedPoint and averageDamp, write a square root function
  // a function that takes a function to another function
  
  def sqrtFuncReturnsFunc(x: Double) =
  fixedPoint(averageDamp(y => x /y))(1)           //> sqrtFuncReturnsFunc: (x: Double)Double
  
  sqrtFuncReturnsFunc(2)                          //> 1.0
                                                  //| 1.5
                                                  //| 1.4166666666666665
                                                  //| 1.4142156862745097
                                                  //| res2: Double = 1.4142135623746899
  
}
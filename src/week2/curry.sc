package week2

// A curried function is a function that returns (nested) functions.
// All functions have 1 arg.
// wiki:
// technique of translating the evaluation of a function that takes multiple arguments (or a tuple of arguments)
// into evaluating a sequence of functions, each with a single argument.
object curry {

  // calculate the product of the values of a function for a given interval
  def product(f: Int => Int)(a: Int, b: Int): Int =
    if (a > b) 1
    else f(a) * product(f)(a + 1, b)              //> product: (f: Int => Int)(a: Int, b: Int)Int

  product(x => x * x)(3, 4)                       //> res0: Int = 144

  // writing factorioal in terms of above
  def fact(n: Int) = product(x => x)(1, n)        //> fact: (n: Int)Int
  fact(5)                                         //> res1: Int = 120

  // Generalizing for both product and sum
  // Parameterizied with mapping, and the two bounds...
  // and the unit value (0 or 1), and the combining function (+, *)

  def mapReduce(f: Int => Int, combine: (Int, Int) => Int, zero: Int)(a: Int, b: Int): Int =
    if (a > b) zero
    else combine(f(a), mapReduce(f, combine, zero)(a + 1, b))
                                                  //> mapReduce: (f: Int => Int, combine: (Int, Int) => Int, zero: Int)(a: Int, b:
                                                  //|  Int)Int

  def product_mr(f: Int => Int)(a: Int, b: Int): Int = mapReduce(f, (x, y) => x * y, 1)(a, b)
                                                  //> product_mr: (f: Int => Int)(a: Int, b: Int)Int
  product_mr(x => x * x)(3, 4)                    //> res2: Int = 144

  def fact_mr(n: Int) = product_mr(x => x)(1, n)  //> fact_mr: (n: Int)Int
  fact_mr(5)                                      //> res3: Int = 120

}
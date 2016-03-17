package week2

object anonfunctailrecurs {

  // rewrite sum function using anonymous function and tail recursion.
  def sum(f: Int => Int, a: Int, b: Int) = {
    def loop(a: Int, accumulator: Int): Int = {
      if (a > b) accumulator
      else loop(a + 1, f(a) + accumulator)
    }
    loop(a, 0)
  }                                               //> sum: (f: Int => Int, a: Int, b: Int)Int

  sum(x => x * x, 3, 5)                           //> res0: Int = 50
  sum(x => x * x * x, 3, 5)                       //> res1: Int = 216
}
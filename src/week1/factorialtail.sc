package week1

object factorialtail {

  //The factorial presented earlier was not tail recursive.
  //This is a tail recursive version.
  def factorial(n: Int): Int = {
    def loop(accmulator: Int, n: Int): Int =
      if (n == 0) accmulator
      else loop(accmulator * n, n - 1)
    loop(1, n)
  }                                               //> factorial: (n: Int)Int

  factorial(4)                                    //> res0: Int = 24
  factorial(6)                                    //> res1: Int = 720

}
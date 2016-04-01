package week5

object ExtInit {

  //Implement init as an external function analogous to last
  def init[T](xs: List[T]): List[T] = xs match {
    case List() => throw new Error("init of empty list")
    case List(x) => List()
    case y :: ys => y :: init(ys) // list of two or more
  }                                               //> init: [T](xs: List[T])List[T]

  //Implement concatenation as a stand-alone function
  def concat[T](xs: List[T], ys: List[T]): List[T] = xs match {
    case List() => ys // if the first list is empty, just return the second
    case z :: zs => z :: concat(zs, ys)
  }                                               //> concat: [T](xs: List[T], ys: List[T])List[T]
  // complexity is equal to the length of the list
  
  
    //Implement reverse as a stand-alone function
  def reverse[T](xs: List[T]): List[T] = xs match {
    case List() => xs // if the first list is empty, just return the second
    case y :: ys => reverse(ys) ++ List(y) // concat
  }                                               //> reverse: [T](xs: List[T])List[T]
  
  // complexity n * n, quadratic complexity.  Not good.  More later.
  

//Remove the n'th element of a list xs. If n is out of bounds, return xs itself.
//Usage example:
//removeAt(1, List('a', 'b', 'c', 'd'))  // List(a, c, d)
def removeAt(n: Int, xs: List[Int]) = (xs take n) ::: (xs drop n + 1)
                                                  //> removeAt: (n: Int, xs: List[Int])List[Int]
removeAt(1, List('a', 'b', 'c', 'd'))             //> res0: List[Int] = List(97, 99, 100)



}
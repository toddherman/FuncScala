package week5

import math.Ordering
object MergeSort {
  
  // attempt to parameterize
  // make the function sort polymorphic
  // pass the comparison operation as an additional parameter
  
  // if you have several parameter lists and one of them is a function value, to put the function value last.
  // Cuz then you have a better chance that the type's already inferred by the time the compiler
  // type checks the function value and
  // that means you don't have to write them explicitly
  // Using the "Ordering" class instead,
  // Made it implicit.  So code is concise, but still fully parametric.
   
  def msort[T](xs: List[T])(implicit ord: Ordering[T]): List[T] = {
    // split the list
    val n = xs.length / 2
    // if zero or 1 (division truncates), sorted already, just return the list
    if (n == 0) xs
    else {
      // form a pair, then match on the pair
      def merge(xs: List[T], ys: List[T]): List[T] = (xs, ys) match {
        // if the first is Nil, return the second
        case (Nil, ys) => ys
        case (xs, Nil) => xs
        // if both are NON Nil
        case (x :: xs1, y :: ys1) =>
          // compare x/y.  if less than, then x is the first element
          if (ord.lt(x, y)) x :: merge(xs1, ys)
          else y :: merge(xs, ys1)
      }
      // return first and second halves of the list
      val (fst, snd) = xs splitAt n
      // recursive call to sort, then merge
      merge(msort(fst), msort(snd))
    }
  }                                               //> msort: [T](xs: List[T])(implicit ord: scala.math.Ordering[T])List[T]

  val nums = List(2, -4, 5, 7, 1, 99)             //> nums  : List[Int] = List(2, -4, 5, 7, 1, 99)
  // the types of the two function values are not necessary, removed.
  // scala figures out type by analyzing the call of msort of nums, because nums is a list of int
  // big reduction using Ordering
  msort(nums)                                     //> res0: List[Int] = List(-4, 1, 2, 5, 7, 99)
  
// ussing a list of strings
  val fruits = List("apple", "pineapple", "orange", "banana", "kiwi")
                                                  //> fruits  : List[String] = List(apple, pineapple, orange, banana, kiwi)
  // pass java.string comparison operator.  Does it return -1?
  // scala figures out type by analyzing the call of msort of fruits, because fruits is a list of Strings
  // another big reduction using Ordering
  msort(fruits)                                   //> res1: List[String] = List(apple, banana, kiwi, orange, pineapple)

  // rewrite merge using a pattern matching over pairs
  // it doesn't really matter what is left-hand and what is right-hand
  def merge(xs: List[Int], ys: List[Int]): List[Int] =
    xs match {
      case Nil =>
        ys
      case x :: xs1 =>
        ys match {
          case Nil =>
            xs
          // if not Nil, we have two head elements, x and y
          // and two tail lists: xs1 and ys1
          //
          case y :: ys1 =>

            // compare the head elements with each other
            // if x is smaller, then take x followed by all remaining elements of
            // of xs, and all elements of ys
            if (x < y) x :: merge(xs1, ys)

            // y is not less, just merge xs with ys1
            else y :: merge(xs, ys1)
        }
    }                                             //> merge: (xs: List[Int], ys: List[Int])List[Int]

}
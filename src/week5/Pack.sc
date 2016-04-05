package week5

object Pack {

  val data = List("a", "a", "a", "b", "c", "c", "a")
                                                  //> data  : List[String] = List(a, a, a, b, c, c, a)
  val nums = List(2, -4, 5, 7, 1, 99)             //> nums  : List[Int] = List(2, -4, 5, 7, 1, 99)

  // pack consecutive duplicates into sublists
  def pack[T](xs: List[T]): List[List[T]] = xs match {
    case Nil => Nil
    case x :: xs1 =>
      // pattern match first/rest
      // using higher order function "span", take element as long as they are equal to x
      // span sytax: (List[Int], List[Int]) = (List(2),List(-4, 5, 7, 1, 99))
      // longest prefix, remainder in one traversal
      val (first, rest) = xs span (y => y == x)
      first :: pack(rest)
  }                                               //> pack: [T](xs: List[T])List[List[T]]

  pack(data)                                      //> res0: List[List[String]] = List(List(a, a, a), List(b), List(c, c), List(a))
                                                  //| 

  // deduce run length encoding of a list
  // return a list of pairs of the element and an integer
  def encode[T](xs: List[T]): List[(T, Int)] =
    // most of the work is done using pack (above) which yields a list of lists
    // With a packed list, apply a simple transformation to get length for each element
    // return a pair => first element in the sub list, length of that sub list
    pack(xs) map (ys => (ys.head, ys.length))     //> encode: [T](xs: List[T])List[(T, Int)]

  encode(data)                                    //> res1: List[(String, Int)] = List((a,3), (b,1), (c,2), (a,1))

  def concat[T](xs: List[T], ys: List[T]): List[T] =
    // when changed to foldLeft, error: "value :: is not a member of type parameter T"
   	// cons is not applicable to arbitrary elements, only to lists.
    (xs foldRight ys)(_ :: _)                     //> concat: [T](xs: List[T], ys: List[T])List[T]
    
    concat(data, nums)                            //> res2: List[Any] = List(a, a, a, b, c, c, a, 2, -4, 5, 7, 1, 99)
}
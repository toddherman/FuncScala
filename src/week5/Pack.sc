package week5

object Pack {
  
  val data = List("a", "a", "a", "b", "c", "c", "a")
                                                  //> data  : List[String] = List(a, a, a, b, c, c, a)
  
  // pack consecutive duplicates into sublists
  def pack[T](xs: List[T]): List[List[T]] = xs match {
    case Nil => Nil
    case x :: xs1 =>
    // pattern match first/rest
    // using higher order function "span", take element as long as they are equal to x
    // span sytax: (List[Int], List[Int]) = (List(2),List(-4, 5, 7, 1, 99))
    // longest prefix, remainder in one traversal
    val (first, rest) = xs span(y => y == x)
    first :: pack(rest)
  }                                               //> pack: [T](xs: List[T])List[List[T]]

  pack(data)                                      //> res0: List[List[String]] = List(List(a, a, a), List(b), List(c, c), List(a))
                                                  //| 
}
package week5

object Mapping { 

  // using pattern matching
  def squareList(xs: List[Int]): List[Int] = xs match {
    case Nil => Nil
    case y :: ys => y * y :: squareList(ys)
  }                                               //> squareList: (xs: List[Int])List[Int]
  
  // shorter and clearer using higher order function "map"
  def squareListRedux(xs: List[Int]): List[Int] =
    xs map (x => x * x)                           //> squareListRedux: (xs: List[Int])List[Int]

  val nums = List(2, -4, 5, 7, 1, 99)             //> nums  : List[Int] = List(2, -4, 5, 7, 1, 99)

  squareList(nums)                                //> res0: List[Int] = List(4, 16, 25, 49, 1, 9801)
  squareListRedux(nums)                           //> res1: List[Int] = List(4, 16, 25, 49, 1, 9801)
}
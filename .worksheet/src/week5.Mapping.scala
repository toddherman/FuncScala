package week5

object Mapping {;import org.scalaide.worksheet.runtime.library.WorksheetSupport._; def main(args: Array[String])=$execute{;$skip(186); 

  // using pattern matching
  def squareList(xs: List[Int]): List[Int] = xs match {
    case Nil => Nil
    case y :: ys => y * y :: squareList(ys)
  };System.out.println("""squareList: (xs: List[Int])List[Int]""");$skip(136); 
  
  // shorter and clearer using higher order function "map"
  def squareListRedux(xs: List[Int]): List[Int] =
    xs map (x => x * x);System.out.println("""squareListRedux: (xs: List[Int])List[Int]""");$skip(40); 

  val nums = List(2, -4, 5, 7, 1, 99);System.out.println("""nums  : List[Int] = """ + $show(nums ));$skip(21); val res$0 = 

  squareList(nums);System.out.println("""res0: List[Int] = """ + $show(res$0));$skip(24); val res$1 = 
  squareListRedux(nums);System.out.println("""res1: List[Int] = """ + $show(res$1))}
}

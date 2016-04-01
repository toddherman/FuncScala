package week5

object MergeSort {
  def msort(xs: List[Int]): List[Int] = {
   // split the list
    val n = xs.length / 2
    // if zero or 1 (division truncates), sorted already, just return the list
    if (n == 0) xs
    else {
      def merge(xs: List[Int], ys: List[Int]) = ???
      
      // return first and second halves of the list
      val (fst, snd) = xs splitAt n
      
      // recursive call to sort, then merge
      merge(msort(fst), msort(snd))
    }
  }                                               //> msort: (xs: List[Int])List[Int]


    val pair = ("answer", 42)                     //> pair  : (String, Int) = (answer,42)
    
    // decompose pairs using pattern matching.  like labels or indexes?
    val(label, value) = pair                      //> label  : String = answer
                                                  //| value  : Int = 42
    
    
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
package week5

object HighOrdListFunc {;import org.scalaide.worksheet.runtime.library.WorksheetSupport._; def main(args: Array[String])=$execute{;$skip(79); 
    val nums = List(2, -4, 5, 7, 1, 99);System.out.println("""nums  : List[Int] = """ + $show(nums ));$skip(72); 
    val fruits = List("apple", "pineapple", "orange", "banana", "kiwi");System.out.println("""fruits  : List[String] = """ + $show(fruits ));$skip(34); val res$0 = 
    
    nums filter (x => x > 0);System.out.println("""res0: List[Int] = """ + $show(res$0));$skip(32); val res$1 = 
    nums filterNot (x => x > 0);System.out.println("""res1: List[Int] = """ + $show(res$1));$skip(100); val res$2 = 
    
    // partition is like filter and filterNot in one traversal
    nums partition (x => x > 0);System.out.println("""res2: (List[Int], List[Int]) = """ + $show(res$2));$skip(168); val res$3 = 
    
    // next two look at a prefix and a suffix
    // will only take the longest prefix of the list, nothing after condition is met
    nums takeWhile (x => x > 0);System.out.println("""res3: List[Int] = """ + $show(res$3));$skip(53); val res$4 = 
    
    // opposite
    nums dropWhile (x => x > 0);System.out.println("""res4: List[Int] = """ + $show(res$4))}
}

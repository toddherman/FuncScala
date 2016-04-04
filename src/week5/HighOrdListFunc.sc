package week5

object HighOrdListFunc {
    val nums = List(2, -4, 5, 7, 1, 99)           //> nums  : List[Int] = List(2, -4, 5, 7, 1, 99)
    val fruits = List("apple", "pineapple", "orange", "banana", "kiwi")
                                                  //> fruits  : List[String] = List(apple, pineapple, orange, banana, kiwi)
    
    nums filter (x => x > 0)                      //> res0: List[Int] = List(2, 5, 7, 1, 99)
    nums filterNot (x => x > 0)                   //> res1: List[Int] = List(-4)
    
    // partition is like filter and filterNot in one traversal
    nums partition (x => x > 0)                   //> res2: (List[Int], List[Int]) = (List(2, 5, 7, 1, 99),List(-4))
    
    // next two look at a prefix and a suffix
    // will only take the longest prefix of the list, nothing after condition is met
    nums takeWhile (x => x > 0)                   //> res3: List[Int] = List(2)
    
    // opposite
    nums dropWhile (x => x > 0)                   //> res4: List[Int] = List(-4, 5, 7, 1, 99)
    
    nums span (x => x > 0)                        //> res5: (List[Int], List[Int]) = (List(2),List(-4, 5, 7, 1, 99))
}
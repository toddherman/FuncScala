package week6

object Sets {

  val fruit = Set("apple", "pear", "banana")      //> fruit  : scala.collection.immutable.Set[String] = Set(apple, pear, banana)

  1 to 6                                          //> res0: scala.collection.immutable.Range.Inclusive = Range(1, 2, 3, 4, 5, 6)
  // illustrates non-ordered
  val s = (1 to 6).toSet                          //> s  : scala.collection.immutable.Set[Int] = Set(5, 1, 6, 2, 3, 4)

  s map (_ + 2)                                   //> res1: scala.collection.immutable.Set[Int] = Set(5, 6, 7, 3, 8, 4)
  //fruit filter (_1.startsWith == "app")
  s.nonEmpty                                      //> res2: Boolean = true
}
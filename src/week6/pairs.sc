package week6

object pairs {
  //given a positive integer n, find all pairs of positive integers, i and j, with 1 <= j < i n
  // such that i + j is a prime number
  // Overview:  Generate, filter

  val n = 7                                       //> n  : Int = 7

  // yields a vector of vectors
  // each element is an i less than j
  // map: apply a function to each element.
  (1 until n) map (i =>
    (1 until i) map (j => (i, j)))                //> res0: scala.collection.immutable.IndexedSeq[scala.collection.immutable.Index
                                                  //| edSeq[(Int, Int)]] = Vector(Vector(), Vector((2,1)), Vector((3,1), (3,2)), V
                                                  //| ector((4,1), (4,2), (4,3)), Vector((5,1), (5,2), (5,3), (5,4)), Vector((6,1)
                                                  //| , (6,2), (6,3), (6,4), (6,5)))

  // We want to generate just a single collection of pairs, not a collection of vectors.
  // So, what we need to do is we need to concatenate all the element vectors into one single list of pairs.
  // Could use foldRight with concatenation.  Or more simply, "flatten"
  ((1 until n) map (i =>
    (1 until i) map (j => (i, j)))).flatten       //> res1: scala.collection.immutable.IndexedSeq[(Int, Int)] = Vector((2,1), (3,1
                                                  //| ), (3,2), (4,1), (4,2), (4,3), (5,1), (5,2), (5,3), (5,4), (6,1), (6,2), (6,
                                                  //| 3), (6,4), (6,5))

  // Useful law:  xs flatMap f = (xs map f).flatten
  // Therefor...

  (1 until n) flatMap (i =>
    (1 until i) map (j => (i, j)))                //> res2: scala.collection.immutable.IndexedSeq[(Int, Int)] = Vector((2,1), (3,1
                                                  //| ), (3,2), (4,1), (4,2), (4,3), (5,1), (5,2), (5,3), (5,4), (6,1), (6,2), (6,
                                                  //| 3), (6,4), (6,5))
  // now filter
  def isPrime(n: Int): Boolean = (2 until n) forall (d => n % d != 0)
                                                  //> isPrime: (n: Int)Boolean

  (1 until n) flatMap (i =>
    (1 until i) map (j => (i, j))) filter (pair =>
    isPrime(pair._1 + pair._2))                   //> res3: scala.collection.immutable.IndexedSeq[(Int, Int)] = Vector((2,1), (3,
                                                  //| 2), (4,1), (4,3), (5,2), (6,1), (6,5))

  // works, but makes your head hurt.  Looking for greater simplicity.
  // same using for/yeild
  // much more readable

  for {
    i <- 1 until n
    j <- 1 until i
    if isPrime(i + j)
  } yield (i, j)                                  //> res4: scala.collection.immutable.IndexedSeq[(Int, Int)] = Vector((2,1), (3,
                                                  //| 2), (4,1), (4,3), (5,2), (6,1), (6,5))

  // exercise: rewrite scalar product from last session
  val ts = List(1.2, 2.1, 3.4, 44.7)              //> ts  : List[Double] = List(1.2, 2.1, 3.4, 44.7)
  val us = List(7.2, 6.1, 5.4, 4.7)               //> us  : List[Double] = List(7.2, 6.1, 5.4, 4.7)
  
  def scalarProduct(xs: List[Double], ys: List[Double]): Double =
    //  let x and y range over the result of zipping the two vectors
    //  return(yield), in each case, the multiplication between x and y
    //  finally, sum them up.
    (for ((x, y) <- xs zip ys) yield x * y).sum   //> scalarProduct: (xs: List[Double], ys: List[Double])Double

  scalarProduct(ts, us)                           //> res5: Double = 249.90000000000003

}
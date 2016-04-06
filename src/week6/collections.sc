package week6

object collections {
  val xs = Array(1, 2, 3, 44)                     //> xs  : Array[Int] = Array(1, 2, 3, 44)
  xs map (x => x * 2)                             //> res0: Array[Int] = Array(2, 4, 6, 88)
  //or
  xs map (_ * 2)                                  //> res1: Array[Int] = Array(2, 4, 6, 88)

  // another sequence type: string
  val s = "Hello Milky Way"                       //> s  : String = Hello Milky Way
  s filter (c => c.isUpper)                       //> res2: String = HMW
  // or
  s filter (_.isUpper)                            //> res3: String = HMW

  val t: Range = 1 until 5                        //> t  : Range = Range(1, 2, 3, 4)
  val u: Range = 1 to 5                           //> u  : Range = Range(1, 2, 3, 4, 5)
  1 to 10 by 3                                    //> res4: scala.collection.immutable.Range = Range(1, 4, 7, 10)
  6 to 1 by -2                                    //> res5: scala.collection.immutable.Range = Range(6, 4, 2)

  s exists (_.isUpper)                            //> res6: Boolean = true
  s forall (_.isUpper)                            //> res7: Boolean = false

  val pairs = List(1, 2, 3, 4) zip s              //> pairs  : List[(Int, Char)] = List((1,H), (2,e), (3,l), (4,l))
  pairs.unzip                                     //> res8: (List[Int], List[Char]) = (List(1, 2, 3, 4),List(H, e, l, l))

  s flatMap (c => List('.', c))                   //> res9: String = .H.e.l.l.o. .M.i.l.k.y. .W.a.y

  xs.sum                                          //> res10: Int = 50
  xs.product                                      //> res11: Int = 264
  xs.min                                          //> res12: Int = 1
  xs.max                                          //> res13: Int = 44

  val M = 4                                       //> M  : Int = 4
  val N = 5                                       //> N  : Int = 5
  // List all combinations of numbers x and y
  (1 to M) flatMap (x => (1 to N) map (y => (x, y)))
                                                  //> res14: scala.collection.immutable.IndexedSeq[(Int, Int)] = Vector((1,1), (1,
                                                  //| 2), (1,3), (1,4), (1,5), (2,1), (2,2), (2,3), (2,4), (2,5), (3,1), (3,2), (3
                                                  //| ,3), (3,4), (3,5), (4,1), (4,2), (4,3), (4,4), (4,5))
val ts = Vector(1.2, 2.1, 3.4, 44.7)              //> ts  : scala.collection.immutable.Vector[Double] = Vector(1.2, 2.1, 3.4, 44.7
                                                  //| )
val us = Vector(7.2, 6.1, 5.4, 4.7)               //> us  : scala.collection.immutable.Vector[Double] = Vector(7.2, 6.1, 5.4, 4.7)
                                                  //| 

// first zip up the x s into y s into pairs
ts zip us                                         //> res15: scala.collection.immutable.Vector[(Double, Double)] = Vector((1.2,7.2
                                                  //| ), (2.1,6.1), (3.4,5.4), (44.7,4.7))
// map that performs the multiplication on each pair
(ts zip us).map(xy => xy._1 * xy._2)              //> res16: scala.collection.immutable.Vector[Double] = Vector(8.64, 12.81, 18.36
                                                  //| , 210.09000000000003)
// sum of the results of the multiplications
(ts zip us).map(xy => xy._1 * xy._2).sum          //> res17: Double = 249.90000000000003

  def scalarProduct(xs: Vector[Double], ys: Vector[Double]): Double =
    (xs zip ys).map(xy => xy._1 * xy._2).sum      //> scalarProduct: (xs: Vector[Double], ys: Vector[Double])Double

  // alternate using pattern matching
  // instead of pulling out the elements of the pair with the selectors
  def scalarProductPM(xs: Vector[Double], ys: Vector[Double]): Double =
    (xs zip ys).map { case (x, y) => x * y }.sum  //> scalarProductPM: (xs: Vector[Double], ys: Vector[Double])Double

	scalarProduct(ts, us)                     //> res18: Double = 249.90000000000003
	
	// find primes
	// prime if the only divisors are 1 and n
	def isPrime(n: Int): Boolean = (2 until n) forall (d => n % d != 0)
                                                  //> isPrime: (n: Int)Boolean
 isPrime(16)                                      //> res19: Boolean = false
}
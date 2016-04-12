package week7

object streams {

  def isPrime(n: Int): Boolean = (2 until n) forall (d => n % d != 0)
                                                  //> isPrime: (n: Int)Boolean

  val xs = Stream.cons(1, Stream.cons(2, Stream.empty))
                                                  //> xs  : Stream.Cons[Int] = Stream(1, ?)
  Stream(1, 2, 3)                                 //> res0: scala.collection.immutable.Stream[Int] = Stream(1, ?)

  (1 to 1000).toStream                            //> res1: scala.collection.immutable.Stream[Int] = Stream(1, ?)

  ((1000 to 10000).toStream filter isPrime)(1)    //> res2: Int = 1013
  ((1000 to 10000).toStream filter isPrime)(6)    //> res3: Int = 1039

  //Stream methods:
  //Same methods as List: isEmpty, head, tail, filter, etc...
  //but except cons "::" which always produces a list "#::" produces a stream

  def streamRange(lo: Int, hi: Int): Stream[Int] = {
    print(lo + " ")
    if (lo >= hi) Stream.empty
    else Stream.cons(lo, streamRange(lo + 1, hi))
  }                                               //> streamRange: (lo: Int, hi: Int)Stream[Int]
  streamRange(1, 10).take(3)                      //> 1 res4: scala.collection.immutable.Stream[Int] = Stream(1, ?)
  streamRange(1, 10).take(3).toList               //> 1 2 3 res5: List[Int] = List(1, 2, 3)

  def expr = {
    val x = { print("x"); 1 }
    lazy val y = { print("y"); 2 }
    def z = { print("z"); 3 }
    z + y + x + z + y + x
  }                                               //> expr: => Int
  
 // note lazy evaluation also implies not evaluating a second time
  expr                                            //> xzyzres6: Int = 12
  
  (streamRange(1000, 10000) filter isPrime) apply 1
                                                  //> 1000 1001 1002 1003 1004 1005 1006 1007 1008 1009 1010 1011 1012 1013 res7: 
                                                  //| Int = 1013

  
  
  
}
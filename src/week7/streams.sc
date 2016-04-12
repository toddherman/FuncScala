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

  // infinite
  def from(n: Int): Stream[Int] = n #:: from(n + 1)
                                                  //> from: (n: Int)Stream[Int]

  val nats = from(0)                              //> nats  : Stream[Int] = Stream(0, ?)
  val m4s = nats map (_ * 4)                      //> m4s  : scala.collection.immutable.Stream[Int] = Stream(0, ?)

  //does not evaluate
  m4s take 100                                    //> res8: scala.collection.immutable.Stream[Int] = Stream(0, ?)
  // make it into a list, and it evaluates
  (m4s take 100).toList                           //> res9: List[Int] = List(0, 4, 8, 12, 16, 20, 24, 28, 32, 36, 40, 44, 48, 52,
                                                  //|  56, 60, 64, 68, 72, 76, 80, 84, 88, 92, 96, 100, 104, 108, 112, 116, 120, 
                                                  //| 124, 128, 132, 136, 140, 144, 148, 152, 156, 160, 164, 168, 172, 176, 180, 
                                                  //| 184, 188, 192, 196, 200, 204, 208, 212, 216, 220, 224, 228, 232, 236, 240, 
                                                  //| 244, 248, 252, 256, 260, 264, 268, 272, 276, 280, 284, 288, 292, 296, 300, 
                                                  //| 304, 308, 312, 316, 320, 324, 328, 332, 336, 340, 344, 348, 352, 356, 360, 
                                                  //| 364, 368, 372, 376, 380, 384, 388, 392, 396)

  // The Sieve of Eratosthenes
  def sieve(s: Stream[Int]): Stream[Int] =
    // take the first number and follow it with all that are NOT multiples
    // filter predicate:  the number, modulo the first number must not be zero
    s.head #:: sieve(s.tail filter (_ % s.head != 0))
                                                  //> sieve: (s: Stream[Int])Stream[Int]

  // to see it
  val primes = sieve(from(2))                     //> primes  : Stream[Int] = Stream(2, ?)
  primes.take(100).toList                         //> res10: List[Int] = List(2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43,
                                                  //|  47, 53, 59, 61, 67, 71, 73, 79, 83, 89, 97, 101, 103, 107, 109, 113, 127, 
                                                  //| 131, 137, 139, 149, 151, 157, 163, 167, 173, 179, 181, 191, 193, 197, 199, 
                                                  //| 211, 223, 227, 229, 233, 239, 241, 251, 257, 263, 269, 271, 277, 281, 283, 
                                                  //| 293, 307, 311, 313, 317, 331, 337, 347, 349, 353, 359, 367, 373, 379, 383, 
                                                  //| 389, 397, 401, 409, 419, 421, 431, 433, 439, 443, 449, 457, 461, 463, 467, 
                                                  //| 479, 487, 491, 499, 503, 509, 521, 523, 541)

  // revisiting sq rt.  Doesn't blow up (infinitely recurse) because of lazy cons.
  def sqrtStream(x: Double): Stream[Double] = {
    def improve(guess: Double) = (guess + x / guess) / 2
    lazy val guesses: Stream[Double] = 1 #:: (guesses map improve)
    guesses
  }                                               //> sqrtStream: (x: Double)Stream[Double]

  // we have decoupled the idea of a converging sequence from determination criteria
  sqrtStream(4).take(10).toList                   //> res11: List[Double] = List(1.0, 2.5, 2.05, 2.000609756097561, 2.00000009292
                                                  //| 22947, 2.000000000000002, 2.0, 2.0, 2.0, 2.0)

  //we can add determination criterion later
  def isGoodEnough(guess: Double, x: Double) =
    math.abs((guess * guess - x) / x) < 0.0001    //> isGoodEnough: (guess: Double, x: Double)Boolean

  // subject our square root stream to a filter method of all the elements that are good enough approximations
  sqrtStream(4).filter(isGoodEnough(_, 4)).take(10).toList
                                                  //> res12: List[Double] = List(2.0000000929222947, 2.000000000000002, 2.0, 2.0,
                                                  //|  2.0, 2.0, 2.0, 2.0, 2.0, 2.0)
}
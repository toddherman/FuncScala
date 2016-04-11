package week7

object streams {;import org.scalaide.worksheet.runtime.library.WorksheetSupport._; def main(args: Array[String])=$execute{;$skip(104); 

  def isPrime(n: Int): Boolean = (2 until n) forall (d => n % d != 0);System.out.println("""isPrime: (n: Int)Boolean""");$skip(58); 

  val xs = Stream.cons(1, Stream.cons(2, Stream.empty));System.out.println("""xs  : Stream.Cons[Int] = """ + $show(xs ));$skip(18); val res$0 = 
  Stream(1, 2, 3);System.out.println("""res0: scala.collection.immutable.Stream[Int] = """ + $show(res$0));$skip(25); val res$1 = 

  (1 to 1000).toStream;System.out.println("""res1: scala.collection.immutable.Stream[Int] = """ + $show(res$1));$skip(49); val res$2 = 

  ((1000 to 10000).toStream filter isPrime)(1);System.out.println("""res2: Int = """ + $show(res$2));$skip(47); val res$3 = 
  ((1000 to 10000).toStream filter isPrime)(6);System.out.println("""res3: Int = """ + $show(res$3));$skip(316); 


//Stream methods:
//Same methods as List: isEmpty, head, tail, filter, etc...
//but except cons "::" which always produces a list "#::" produces a stream

  def streamRange(lo: Int, hi: Int): Stream[Int] = {
    print(lo + " ")
    if (lo >= hi) Stream.empty
    else Stream.cons(lo, streamRange(lo + 1, hi))
  };System.out.println("""streamRange: (lo: Int, hi: Int)Stream[Int]""");$skip(29); val res$4 = 
  streamRange(1, 10).take(3);System.out.println("""res4: scala.collection.immutable.Stream[Int] = """ + $show(res$4));$skip(36); val res$5 = 
  streamRange(1, 10).take(3).toList;System.out.println("""res5: List[Int] = """ + $show(res$5))}
  
}

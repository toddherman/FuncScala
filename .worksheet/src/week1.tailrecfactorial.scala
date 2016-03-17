package week1

object tailrecfactorial {;import org.scalaide.worksheet.runtime.library.WorksheetSupport._; def main(args: Array[String])=$execute{;$skip(306); 

  //The factorial presented earlier was not tail recursive.
  //This is a tail recursive version.
  def factorial(n: Int): Int = {
    def loop(accmulator: Int, n: Int): Int =
      if (n == 0) accmulator
      else loop(accmulator * n, n - 1)
    loop(1, n)
  };System.out.println("""factorial: (n: Int)Int""");$skip(17); val res$0 = 

  factorial(4);System.out.println("""res0: Int = """ + $show(res$0));$skip(15); val res$1 = 
  factorial(6);System.out.println("""res1: Int = """ + $show(res$1))}
}

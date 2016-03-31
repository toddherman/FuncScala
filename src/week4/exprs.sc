package week4

object exprs {
  // pattern matching is good fit for resolving our decomposition problem
  def show(e: Expr): String = e match {
    case Number(x) => x.toString
    case Sum(l, r) => show(l) + " + " + show(r)
  }
  
  show(Sum(Number(1), Number(44)))
}
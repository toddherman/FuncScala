package week6

object nqueens {

  // input number of row of chessboard
  // output a set of solutions, each a list of Ints
  def queens(n: Int): Set[List[Int]] = {
    // places a number of queens on the board and produces a set of solutions
    def placeQueens(k: Int): Set[List[Int]] =

      if (k == 0) Set(List()) // we don't need to place any queen, just return the empty list
      else
        for {
          //first solve the sub problem of placing K minus one queens
          // "<-" read as "range over"
          queens <- placeQueens(k - 1) // decrement through the rows
          // put the k queen in a column
          col <- 0 until n
          // can't place it any column, check
          if isSafe(col, queens) // is the column safe with respect to previous queens
        } yield col :: queens // coliumn followed by queens

    placeQueens(n)
  }                                               //> queens: (n: Int)Set[List[Int]]

  // is a given column secure for placement amongst the other placed queens?
  def isSafe(col: Int, queens: List[Int]): Boolean = {
    // add rows to all queens
    val row = queens.length // length of list
    // transform list of column numbers to pairs of rows and columns
    // i.e. list(0,3,1) -> list((2,0), (1,3), (0,1))
    // walk backwards through the rows, zip with given queens
    val queensWithRow = (row - 1 to 0 by -1) zip queens
    // check to see if the new queen is NOT in check
    queensWithRow forall {
      // take the row r and the column c out of the pair
      // first:  is the current column NOT the same as any previous queens
      // and &&:  confirm not in check on any of the diagonals
      //           the absolute difference between the 2 columns and 2 rows must not be the same
      case (r, c) => col != c && math.abs(col - c) != row - r
    }
  }                                               //> isSafe: (col: Int, queens: List[Int])Boolean

  queens(4) //yields 2 solutions                  //> res0: Set[List[Int]] = Set(List(1, 3, 0, 2), List(2, 0, 3, 1))

  def show(queens: List[Int]) = {
    val lines =
      for (col <- queens.reverse)
        yield Vector.fill(queens.length)("* ").updated(col, "X ").mkString
    "\n" + (lines mkString "\n")
  }                                               //> show: (queens: List[Int])String
  
  queens(4) map show                              //> res1: scala.collection.immutable.Set[String] = Set("
                                                  //| * * X * 
                                                  //| X * * * 
                                                  //| * * * X 
                                                  //| * X * * ", "
                                                  //| * X * * 
                                                  //| * * * X 
                                                  //| X * * * 
                                                  //| * * X * ")
}
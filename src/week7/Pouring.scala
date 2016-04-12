package week7

// vector of all the glasses and their capacity
class Pouring(capacity: Vector[Int] {
  
  //States
  // is a vector of integers.  Create type alias
  type State = Vector[Int]
  
  //initial state would consist of all empty glasses. 
  //a vector of the length of capacity consisting of all zeroes.
  // map, function that take a glass and returns zero
  val initialState = capacity map (x => 0)
  
  //Moves
  trait Move{
   def change(state: State): State 
  }
  
  case class Empty(glass: Int) extends Move{
    //updated doesn't destroy the state, it will still be available
   def change(state: State) = state updated (glass, 0)
  }
  case class Fill(glass: Int) extends Move{
    //fill means brought to capacity
   def change(state: State) = state updated (glass, capacity(glass))
  }
  case class Pour(from: Int, to: Int) extends Move{
    //fill means brought to capacity
   def change(state: State) = {
     val amount = state(from) min (capacity(to) - state(to))
     // now that we know the amount to pour...
     state updated (from, state(from) - amount) updated (to, state(to) + amount)
   }
  }
  
  val glasses = 0 until capcity.length
  
  val moves = 
    (for (g <- glasses) yield Empty(g)) ++
    (for (g <- glasses) yield Fill(g)) ++
    (for (from <- glasses; to <- glasses if from!= to) yield Pour(from, to))
    
    
    // Paths:  sequences of moves
    // graphically, this looks like a foldRight
    class Path(history: List[Move]{
      def endState: State: trackState(history)
      private def trackState(xs: List[Move]): State = xs match {
        case Nil => initialState
        case move :: xs1 => move change trackState(xs1)
      }
      
    }
}
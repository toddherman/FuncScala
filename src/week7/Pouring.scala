package week7

// vector of all the glasses and their capacity
class Pouring(capacity: Vector[Int]) {

  //States
  // is a vector of integers.  Create type alias
  type State = Vector[Int]

  //initial state would consist of all empty glasses. 
  //a vector of the length of capacity consisting of all zeroes.
  // map, function that take a glass and returns zero
  val initialState = capacity map (x => 0)

  //Moves
  trait Move {
    def change(state: State): State
  }

  case class Empty(glass: Int) extends Move {
    //updated doesn't destroy the state, it will still be available
    def change(state: State) = state updated (glass, 0)
  }
  case class Fill(glass: Int) extends Move {
    //fill means brought to capacity
    def change(state: State) = state updated (glass, capacity(glass))
  }
  case class Pour(from: Int, to: Int) extends Move {
    //fill means brought to capacity
    def change(state: State) = {
      val amount = state(from) min (capacity(to) - state(to))
      // now that we know the amount to pour...
      state updated (from, state(from) - amount) updated (to, state(to) + amount)
    }
  }

  val glasses = 0 until capacity.length

  val moves =
    (for (g <- glasses) yield Empty(g)) ++
      (for (g <- glasses) yield Fill(g)) ++
      (for (from <- glasses; to <- glasses if from != to) yield Pour(from, to))

  // Paths:  sequences of moves
  // graphically, this looks like a foldRight
  // the new formulation is, without a doubt, much shorter and some would argue more elegant, 
  // than the recursive pattern matching solution. 
  // But for most people and particular beginners, it's also much harder to come up with and maybe to read.
  class Path(history: List[Move]) {
    def endState: State = (history foldRight initialState)(_ change _)
    def extend(move: Move) = new Path(move :: history)
    override def toString = (history.reverse mkString " ") + "--> " + endState
  }

  val initialPath = new Path(Nil) // path that contains the empty history

  // previously visited states excluded
  def from(paths: Set[Path], explored: Set[State]): Stream[Set[Path]] =
    if (paths.isEmpty) Stream.empty
    else {
      val more = for {
        path <- paths
        next <- moves map path.extend
        if !(explored contains next.endState)
      } yield next
      paths #:: from(more, explored ++ (more map (_.endState)))
    }

  val pathSets = from(Set(initialPath), Set(initialState))

  // takes a target volume that we want to see in one of the glasses
  // returns a stream of paths with the target volume as the end state
  def solutions(target: Int): Stream[Path] =
    //go through all, pick the paths that are solutions
    //concatenate into another stream
    for {
      pathSet <- pathSets // go through all
      path <- pathSet // go through each
      if path.endState contains target
    } yield path
}
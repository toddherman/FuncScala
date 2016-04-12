package week7

// takes: vector of all glasses and their capacity
class Pouring(capacity: Vector[Int]) {

  //States
  // is a vector of integers.  Create type alias
  type State = Vector[Int]

  //initial state would consist of all empty glasses. 
  //a vector of the length of capacity consisting of all zeroes.
  // map, function that take a glass (integer) and returns zero
  val initialState = capacity map (x => 0)

  //Moves (and the changes they have on states)
  trait Move {
    // how moves change states... implemented by each case class
    // takes a State and returns a new State
    def change(state: State): State
  }

  case class Empty(glass: Int) extends Move {
    // old state updated at the point of the glass where it's now empty
    //updated (purely functional method) doesn't destroy the state, it will still be available
    def change(state: State) = state updated (glass, 0)
  }
  case class Fill(glass: Int) extends Move {
    //fill means brought to capacity
    def change(state: State) = state updated (glass, capacity(glass))
  }
  case class Pour(from: Int, to: Int) extends Move {
    //pour from one to another
    def change(state: State) = {
      // how much gets poured? Fill the "to" glass to capacity
      // glass capacity minus how much is in there already.
      val amount = state(from) min (capacity(to) - state(to))
      // now that we know the amount to pour...
      // old state from glass updated removing amount
      // old state "to" plus the amount
      state updated (from, state(from) - amount) updated (to, state(to) + amount)
    }
  }

  val glasses = 0 until capacity.length

  //gerate the moves here.
  val moves =
    (for (g <- glasses) yield Empty(g)) ++
      (for (g <- glasses) yield Fill(g)) ++
      (for (from <- glasses; to <- glasses if from != to) yield Pour(from, to)) // can't pour into same glass

  // Paths:  sequences of moves (modeled as their own class)
  // history is in reverse: last move in path, comes first in history list
  // as paths get longer, end state becomes more complicated to compute
  // why recompute?  just store it:  Moved out of body and into args.
  class Path(history: List[Move], val endState: State) {
    // a new move preceeds the history
    def extend(move: Move) = new Path(move :: history, move change endState)
    // print object in intelligable manner.
    override def toString = (history.reverse mkString " ") + "--> " + endState
  }

  val initialPath = new Path(Nil, initialState) // path that contains the empty history

  // State Exploration
  // start from a set of paths, generate a stream of sets of paths
  // "explored" previously visited states excluded (prevent random walk)
  def from(paths: Set[Path], explored: Set[State]): Stream[Set[Path]] =
    if (paths.isEmpty) Stream.empty // boundary condition
    else {
      //gererate all the possible new paths
      val more = for {
        path <- paths // iterate over all paths in set
        next <- moves map path.extend // generate a next path by extending the current path 
        // don't include paths that have already been explored
        // explored is not allowed to contain the endState of next
        if !(explored contains next.endState)
      } yield next
      // followed by the evolution of my next generation more
      // in recursive call to from, must pass the old explored set AND more map endstate.
      paths #:: from(more, explored ++ (more map (_.endState)))
    }

  // use "from" to define the set of all possible paths
  val pathSets = from(Set(initialPath), Set(initialState))

  // takes a target which is the volume that we want to see in one of the glasses
  // returns a stream of paths with the target volume as the end state
  def solutions(target: Int): Stream[Path] =
    //go through all path sets, pick the paths that are solutions
    //concatenate into another stream
    for {
      pathSet <- pathSets // go through all
      path <- pathSet // go through each path
      if path.endState contains target // if it is a solution
    } yield path
}
package week3

trait List[T] {
  def isEmpty: Boolean
  def head: T
  def tail: List[T]
}

//parameterize classes with type parameters
class Cons[T](val head: T, val tail: List[T]) extends List[T] {
  def isEmpty = false // because they are never empty
  // no head or tail defs.  val means they are implemented in the base trait.
  //The difference between a val and a def concerns only the initialization. 
  //val is evaluated when the object is first initialized.
  //def is evaluated each time it is referenced. 
  //field definitions in classes are really special cases of methods
}
class Nil[T] extends List[T] {
  def isEmpty: Boolean = true
  // can't take the head or tail of an empty list
  def head: Nothing = throw new NoSuchElementException("Nil.head")
  def tail: Nothing = throw new NoSuchElementException("Nil.tail")
  // because Nothing is a subtype of any other type.
}

//  Looking at functions as objects
//  define an object List {
//  with 3 functions in it so that users can create lists of lengths 0-2
object List {
  // List(1,2) would expand to List.apply(1,2)

  // parameterize apply with T
  def apply[T](x1: T, x2: T): List[T] = new Cons(x1, new Cons(x2, new Nil))

  // for empty list
  def apply[T]() = new Nil

}

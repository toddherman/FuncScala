package week4

// added covariant type parameter to T here.
trait List[+T] {
  def isEmpty: Boolean
  def head: T
  def tail: List[T]
  def prepend[U >: T](elem: U): List[U] = new Cons(elem, this)
}

class Cons[T](val head: T, val tail: List[T]) extends List[T] {
  def isEmpty = false // because they are never empty
}

//Nil modeled as an object. 
// Because 'Nothing' is the bottom type, which is a subtype of every other type
object Nil extends List[Nothing] {
  def isEmpty: Boolean = true
  def head: Nothing = throw new NoSuchElementException("Nil.head")
  def tail: Nothing = throw new NoSuchElementException("Nil.tail")
}
object test {
  val x: List[String] = Nil // should be the empty list.

  // attempt at proving appropriate type inference
  def f(xs: List[NonEmpty], x: Empty) = xs prepend x
}
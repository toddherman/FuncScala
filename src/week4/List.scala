package week4

trait List[T] {
  def isEmpty: Boolean
  def head: T
  def tail: List[T]
}

class Cons[T](val head: T, val tail: List[T]) extends List[T] {
  def isEmpty = false // because they are never empty
}

//One shortcoming there that was that nil was modeled as a class. 
//Whereas we would prefer it to be an object, after all there's only one empty list.
// Can we change that?  Yes, because we can make List covariant.
class Nil[T] extends List[T] {
  def isEmpty: Boolean = true
  def head: Nothing = throw new NoSuchElementException("Nil.head")
  def tail: Nothing = throw new NoSuchElementException("Nil.tail")
}

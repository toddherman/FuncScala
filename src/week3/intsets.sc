package week3

//   core concept: dynamic binding

//     abstract class / extension:
//       can have members without implementation
//       cannot be instanciated
//       example: binary trees with purely functional data structure (no mutation), aka persistent data structure

//       superclass / subclass / base class
//       in Scala, all classes extend java.lang.Object
       
object intsets {
  val t1 = new NonEmpty(3, Empty, Empty)          //> t1  : week3.NonEmpty = {.3.}
  val t2 = t1 incl 4                              //> t2  : week3.IntSet = {.3{.4.}}
}

// added a method for forming the union of two sets
abstract class IntSet {
  def incl(x: Int): IntSet
  def contains(x: Int): Boolean
  def union(other: IntSet): IntSet //signature of union
}

// updated as a singleton object named Empty instead of a class
//overriding:
//       subclasses can redefine a non-abstract definition via 'override' modifier (mandatory)
       
object Empty extends IntSet {
  def contains(x: Int): Boolean = false
  def incl(x: Int): IntSet = new NonEmpty(x, Empty, Empty)
  override def toString = "."
  // the union of an empty set and another set...
  def union(other: IntSet): IntSet = other
}

class NonEmpty(elem: Int, left: IntSet, right: IntSet) extends IntSet {
  def contains(x: Int): Boolean =
    if (x < elem) left contains x
    else if (x > elem) right contains x
    else true
  def incl(x: Int): IntSet =
    if (x < elem) new NonEmpty(elem, left incl x, right)
    else if (x > elem) new NonEmpty(elem, left, right incl x)
    else this
  override def toString = "{" + left + elem + right + "}"
  
  // lots of iteration, how do we know the recursion will terminate?
  // every call on union is on a set that is smaller than what we started with
  // therefor at some point we have to reach zero.
  // Then we fall back to the Class Empty where "other " is returned
  // see Dynamic Method Dispatch:  code that is invoked by a method depends on the
  // runtime type of the object that contains the method.
  def union(other: IntSet): IntSet =
  	((left union right) union other) incl elem

}
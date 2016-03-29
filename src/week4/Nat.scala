package week4

//Is Scala a pure OO language?
//Yes, we can implement all primitive types and operators (i.e. boolean, int) 
//via classes (Boolean, Int, etc...).

//Peano numbers
abstract class Nat {
  def isZero: Boolean
  def predecessor: Nat
  def successor = new Succ(this) // refactored
  def +(that: Nat): Nat
  def -(that: Nat): Nat
}

object Zero extends Nat {
  def isZero = true
  def predecessor = throw new Error("0.predecessor")
  def +(that: Nat) = that // zero plus a number is the number
  def -(that: Nat) = if (that.isZero) this else throw new Error("negative number")
}

class Succ(n: Nat) extends Nat {
  def isZero = false
  def predecessor = n
  def +(that: Nat) = new Succ(n + that) // recursive
  def -(that: Nat) = if (that.isZero) this else n - that.predecessor
}
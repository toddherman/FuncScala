package week4

abstract class Nat {
  def isZero: Boolean
  def predecessor: Nat
  def successor: Nat
  def +(that: Nat): Nat
  def -(that: Nat): Nat
}

object Zero extends Nat {
  def isZero = true
  def predecessor = throw new Error("0.predecessor")
  def successor = new Succ(this)
  def +(that: Nat)
  dev - (that: Nat)
}

class Succ(N: Nat) extends Nat {
  def isZero = false
  def predecessor = n
  def successor = new Succ(this) // refactor this as it is duplicated in Zero above
}
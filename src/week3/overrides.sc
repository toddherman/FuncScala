package week3

object overrides {
  println("Override illustration and rules")      //> Override illustration and rules
}

abstract class Base {
  def foo = 1
  def bar: Int
}

class Sub extends Base {
// "override" required here.  More picky than Java.
  override def foo = 2
  //override optional here since method is implementing a definition. Only type is given above.
  def bar = 3
}
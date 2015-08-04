package scala.pickling.generator

import org.scalatest.FunSuite

class WillRobinsonGeneratorTest extends FunSuite {
  import scala.pickling.Defaults._
  import scala.pickling.json._
  import scala.pickling.PicklerUnpickler
  test("empty non-final class") {
    val x = new EmptyNonFinal
    val p = PicklerUnpickler.generate[EmptyNonFinal]
    val y = x.pickle.unpickle[EmptyNonFinal]
    assert(x != y)
    assert(x.getClass == y.getClass)
  }
  test("private-var non-final class") {
    val x = new PrivateValNonFinal(5)
    val p = PicklerUnpickler.generate[PrivateValNonFinal]
    val y = x.pickle.unpickle[PrivateValNonFinal]
    assert(x != y)
    assert(x.toString == y.toString)
  }
}

class EmptyNonFinal {}
class PrivateValNonFinal(y: Int) {
  private var x: Int = y
  override def toString = x.toString
}
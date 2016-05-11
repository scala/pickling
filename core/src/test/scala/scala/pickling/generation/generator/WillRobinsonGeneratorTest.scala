package scala.pickling.generator

import org.scalatest.FunSuite

class WillRobinsonGeneratorTest extends FunSuite {
  import scala.pickling.Defaults._
  import scala.pickling.json._
  import scala.pickling.PicklerUnpickler
  test("empty non-final class") {
    val x = new EmptyNonFinal
    implicit val p = PicklerUnpickler.generate[EmptyNonFinal]
    val y = x.pickle.unpickle[EmptyNonFinal]
    assert(x != y)
    assert(x.getClass == y.getClass)
  }
  test("private-var non-final class") {
    val x = new PrivateVarNonFinal(5)
    implicit val p = PicklerUnpickler.generate[PrivateVarNonFinal]
    val y = x.pickle.unpickle[PrivateVarNonFinal]
    assert(x != y)
    assert(x.toString == y.toString)
  }
  test("private-val non-final class") {
    val x = new PrivateValNonFinal(5)
    implicit val p = PicklerUnpickler.generate[PrivateValNonFinal]
    val y = x.pickle.unpickle[PrivateValNonFinal]
    assert(x != y)
    assert(x.toString == y.toString)
  }
  test("private-this-val non-final class") {
    val x = new PrivateThisVal(5)
    implicit val p = PicklerUnpickler.generate[PrivateThisVal]
    val y = x.pickle.unpickle[PrivateThisVal]
    assert(x != y)
    assert(x.toString == y.toString)
  }
}

class EmptyNonFinal {}
class PrivateValNonFinal(y: Int) {
  private val x: Int = y
  override def toString = x.toString
}

class PrivateVarNonFinal(y: Int) {
  private val x: Int = y
  override def toString = x.toString
}

class PrivateThisVal(y: Int) {
  private[this] val x: Int = y
  override def toString = x.toString
}
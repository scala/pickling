package scala.pickling.binary.list.obj

import org.scalatest.FunSuite

import scala.pickling._, scala.pickling.Defaults._, binary._

case class Person(name: String)

class BinaryListObjectTest extends FunSuite {
  test("main") {
    val lst = List(Person("A"), Person("B"), Person("C"))
    val pickle = lst.pickle
    assert(pickle.unpickle[List[Person]] == List(Person("A"), Person("B"), Person("C")))
  }
}

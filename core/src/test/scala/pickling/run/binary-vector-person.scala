package scala.pickling.binary.vector.person

import org.scalatest.FunSuite
import scala.pickling._
import binary._

case class Person(name: String)

class BinaryVectorPerson extends FunSuite {
  test("main") {
    val pickle = Vector(Person("A"), Person("B"), Person("C")).pickle
    assert(pickle.unpickle[Vector[Person]] === Vector(Person("A"), Person("B"), Person("C")))
  }
}

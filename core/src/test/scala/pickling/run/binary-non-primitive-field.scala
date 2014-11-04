package scala.pickling.binary.non.primitive.field

import org.scalatest.FunSuite
import scala.pickling._
import binary._

case class Person(name: String, age: Int)
case class Philipp(mother: Person)

class BinaryNonPrimitiveFieldTest extends FunSuite {
  test("main") {
    val gudrun = Person("Gudrun", 62)
    val p = Philipp(gudrun)
    val pckl = p.pickle

    assert(pckl.value.mkString("[", ",", "]") === "[2,0,0,0,49,115,99,97,108,97,46,112,105,99,107,108,105,110,103,46,98,105,110,97,114,121,46,110,111,110,46,112,114,105,109,105,116,105,118,101,46,102,105,101,108,100,46,80,104,105,108,105,112,112,-5,0,0,0,6,71,117,100,114,117,110,0,0,0,62]")
    assert(pckl.unpickle[Philipp] === p)
  }
}

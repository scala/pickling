package scala.pickling.binary.non.primitive.fields

import org.scalatest.FunSuite
import scala.pickling._, scala.pickling.Defaults._, binary._

case class Person(val name: String, val age: Int)
case class Philipp(val nationality: String, val weird: Boolean, val mother: Person)

class BinaryNonPrimitiveFieldsTest extends FunSuite {
  test("main") {
    val ph = new Philipp("German", true, new Person("Gudrun", 62))
    val pckl = ph.pickle
    // Note: We do not check the binary value because field orders are not guaranteed to be the same.
    //assert(pckl.value.mkString("[", ",", "]") === "[0,0,0,50,115,99,97,108,97,46,112,105,99,107,108,105,110,103,46,98,105,110,97,114,121,46,110,111,110,46,112,114,105,109,105,116,105,118,101,46,102,105,101,108,100,115,46,80,104,105,108,105,112,112,0,0,0,6,71,101,114,109,97,110,1,-5,0,0,0,6,71,117,100,114,117,110,0,0,0,62]")
    assert(pckl.unpickle[Philipp] == ph)
  }
}

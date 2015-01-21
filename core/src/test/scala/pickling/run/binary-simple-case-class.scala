package scala.pickling.binary.simple.`case`.`class`

import org.scalatest.FunSuite
import scala.pickling._, scala.pickling.Defaults._, binary._
import reflect.runtime.{universe => ru}
import ru._

case class Person(age: Int)

class BinarySimpleCaseClassTest extends FunSuite {
  test("main") {
    val p = Person(43)
    val pickle = p.pickle
    assert(pickle.value.mkString("[", ",", "]") === "[0,0,0,46,115,99,97,108,97,46,112,105,99,107,108,105,110,103,46,98,105,110,97,114,121,46,115,105,109,112,108,101,46,99,97,115,101,46,99,108,97,115,115,46,80,101,114,115,111,110,0,0,0,43]")
    assert(pickle.unpickle[Person] === p)
  }
}

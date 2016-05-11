package scala.pickling.json.non.primitive.field

import org.scalatest.FunSuite
import scala.pickling._, scala.pickling.Defaults._, json._

final case class Person(name: String, age: Int)
case class Philipp(mother: Person)

class JSONNonPrimitiveFieldTest extends FunSuite {
  test("main") {
    val gudrun = Person("Gudrun", 62)
    val p = Philipp(gudrun)
    val pckl = p.pickle

    assert(pckl.value === """{
  "$type": "scala.pickling.json.non.primitive.field.Philipp",
  "mother": {
    "name": "Gudrun",
    "age": 62
  }
}""")
    assert(pckl.unpickle[Philipp] === p)
  }
}

package scala.pickling.non.primitive.field

import org.scalatest.FunSuite
import scala.pickling._, scala.pickling.Defaults._, json._

case class Person(name: String, age: Int)
case class Philipp(mother: Person)

class NonPrimitiveFieldTest extends FunSuite {
  test("main") {
    val gudrun = Person("Gudrun", 62)
    val pckl = Philipp(gudrun).pickle
    assert(pckl.value === """
      |{
      |  "tpe": "scala.pickling.non.primitive.field.Philipp",
      |  "mother": {
      |    "name": "Gudrun",
      |    "age": 62
      |  }
      |}
    """.stripMargin.trim)
    assert(pckl.unpickle[Philipp] === Philipp(gudrun))
  }
}

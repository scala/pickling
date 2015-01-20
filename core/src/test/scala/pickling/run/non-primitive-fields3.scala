package scala.pickling.non.primitive.fields3

import org.scalatest.FunSuite
import scala.pickling._, scala.pickling.Defaults._, json._

case class Person(name: String, age: Int)
case class Philipp(mother: Person, father: Person)

class NonPrimitiveFields3Test extends FunSuite {
  test("main") {
    val gudrun = Person("Gudrun", 62)
    val rudolf = Person("Rudolf", 70)
    val pckl = Philipp(gudrun, rudolf).pickle
    assert(pckl.value === """
      |{
      |  "tpe": "scala.pickling.non.primitive.fields3.Philipp",
      |  "mother": {
      |    "name": "Gudrun",
      |    "age": 62
      |  },
      |  "father": {
      |    "name": "Rudolf",
      |    "age": 70
      |  }
      |}
    """.stripMargin.trim)
    assert(pckl.unpickle[Philipp] === Philipp(gudrun, rudolf))
  }
}

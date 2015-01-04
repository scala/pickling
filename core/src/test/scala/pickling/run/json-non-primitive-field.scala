package scala.pickling.json.non.primitive.field

import org.scalatest.FunSuite
import scala.pickling._
import json._
import AllPicklers._

case class Person(name: String, age: Int)
case class Philipp(mother: Person)

class JSONNonPrimitiveFieldTest extends FunSuite {
  test("main") {
    val gudrun = Person("Gudrun", 62)
    val p = Philipp(gudrun)
    val pckl = p.pickle

    assert(pckl.value === """{
  "tpe": "scala.pickling.json.non.primitive.field.Philipp",
  "mother": {
    "name": "Gudrun",
    "age": 62
  }
}""")
    assert(pckl.unpickle[Philipp] === p)
  }
}

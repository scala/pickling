package scala.pickling.nested.non.primitive.fields

import org.scalatest.FunSuite
import scala.pickling._, scala.pickling.Defaults._, json._

case class Job(title: String)
case class Person(name: String, age: Int, job: Job)
case class Philipp(mother: Person)

class NestedNonPrimitiveFieldsTest extends FunSuite {
  test("main") {
    val gudrun = Person("gudrun", 62, Job("Teacher"))
    val pckl = Philipp(gudrun).pickle
    assert(pckl.value.toString === """
      |{
      |  "tpe": "scala.pickling.nested.non.primitive.fields.Philipp",
      |  "mother": {
      |    "name": "gudrun",
      |    "age": 62,
      |    "job": {
      |      "title": "Teacher"
      |    }
      |  }
      |}
    """.stripMargin.trim)
    assert(pckl.unpickle[Philipp] === Philipp(gudrun))
  }
}

package scala.pickling.nested.non.primitive.fields

import org.scalatest.FunSuite
import scala.pickling._, scala.pickling.Defaults._, json._

final case class Job(title: String)
final case class Person(name: String, age: Int, job: Job)
case class Philipp(mother: Person)

class NestedNonPrimitiveFieldsTest extends FunSuite {
  test("main") {
    val gudrun = Person("gudrun", 62, Job("Teacher"))
    val pckl = Philipp(gudrun).pickle
    // Note:  Previously pickling would consider non-final case classes as final, and would elide the types.
    //        We no longer do this for correctness, so this test ensures the case classes are final.
    assert(pckl.value.toString === """
      |{
      |  "$type": "scala.pickling.nested.non.primitive.fields.Philipp",
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

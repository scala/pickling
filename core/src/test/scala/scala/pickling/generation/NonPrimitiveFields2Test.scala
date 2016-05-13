package scala.pickling.non.primitive.fields2

import org.scalatest.FunSuite
import scala.pickling._, scala.pickling.Defaults._, json._

// Note: Previously pickling would not consider finality when deciding to statically elid tags.
final case class Person(val name: String, val age: Int)
case class Philipp(val nationality: String, val weird: Boolean, val mother: Person)

class NonPrimitiveFields2Test extends FunSuite {
  test("main") {
    val p = new Philipp("German", true, new Person("Gudrun", 62))
    val pickle = p.pickle
    assert(pickle.value.toString === """
      |{
      |  "$type": "scala.pickling.non.primitive.fields2.Philipp",
      |  "nationality": "German",
      |  "weird": true,
      |  "mother": {
      |    "name": "Gudrun",
      |    "age": 62
      |  }
      |}
    """.stripMargin.trim)
    assert(pickle.unpickle[Philipp] === p)
  }
}

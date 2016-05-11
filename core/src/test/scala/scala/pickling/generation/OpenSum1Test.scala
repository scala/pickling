package scala.pickling.open.sum1

import org.scalatest.FunSuite
import scala.pickling._, scala.pickling.Defaults._, json._

package outer {
  abstract class Person {
    val name: String
    val age: Int
  }

  case class Firefighter(val name: String, val age: Int, val since: Int) extends Person

  package inner {
    case class Employee(val name: String, val age: Int, val salary: Int) extends Person
  }
}

class OpenSum1Test extends FunSuite {
  test("main") {
    import outer._

    val f: Person = new Firefighter(
      name = "Jeff",
      age = 45,
      since = 1990
    )

    val pickle = f.pickle
    assert(pickle.value.toString === """
      |{
      |  "$type": "scala.pickling.open.sum1.outer.Firefighter",
      |  "name": "Jeff",
      |  "age": 45,
      |  "since": 1990
      |}
    """.stripMargin.trim)
    assert(pickle.unpickle[Person] === f)
  }
}

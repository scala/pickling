package scala.pickling.`private`.`var`

import org.scalatest.FunSuite
import scala.pickling._, scala.pickling.Defaults._, json._

case class Person(val name: String, val age: Int) {
  private var ssn: Int = 0
}

class PrivateVarTest extends FunSuite {
  test("main") {
    val p = new Person("Bob", 42)
    val pickle = p.pickle
    assert(pickle.value === """
      |{
      |  "$type": "scala.pickling.private.var.Person",
      |  "name": "Bob",
      |  "age": 42,
      |  "ssn": 0
      |}
    """.stripMargin.trim)
    assert(pickle.unpickle[Person] === p)
  }
}

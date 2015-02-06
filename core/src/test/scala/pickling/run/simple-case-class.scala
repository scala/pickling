package scala.pickling.simple.`case`.`class`

import org.scalatest.FunSuite
import scala.pickling._, scala.pickling.Defaults._, json._

case class Person(name: String, age: Int)

class SimpleCaseClassTest extends FunSuite {
  test("main") {
    val expectedPickle = """
    |{
    |  "$type": "scala.pickling.simple.case.class.Person",
    |  "name": "Bob",
    |  "age": 83
    |}
    """.stripMargin.trim

    val person = new Person("Bob", 83)
    val pickle = person.pickle
    assert(pickle.value === expectedPickle)
    assert(pickle.unpickle[Person] === person)
  }
}
package scala.pickling.simplecaseclass

import org.scalatest.FunSuite
import scala.pickling._
import json._

case class Person(name: String, age: Int)

class SimpleCaseClassTest extends FunSuite {
  test("main") {
    val expectedPickle = """
    |{
    |  "tpe": "scala.pickling.simplecaseclass.Person",
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
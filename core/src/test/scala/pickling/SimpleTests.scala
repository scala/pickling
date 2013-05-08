package scala.pickling

import org.scalatest.FunSuite
import scala.pickling._
import scala.pickling.json._

case class Person(name: String, age: Int)

class SimpleTests extends FunSuite {
  test("pickle/unpickle a simple case class") {
    val pickle = new Person("Bob", 83).pickle
    println(pickle.value)
    println(pickle.unpickle[Person])
  }
}
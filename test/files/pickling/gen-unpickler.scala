import scala.pickling._
import json._

case class Person(val name: String, val age: Int)

object Test extends App {
  val pickle = Person("Bob",83).pickle
  println(pickle.value)
  println(pickle.value.unpickle[Person])
}

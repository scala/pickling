import scala.pickling._
import json._

case class Person(name: String, age: Int)

object Test extends App {
  val pickler = genPickler[Person]
  val pickle = pickler.pickle(new Person("Bob",83))
  println(pickle.value)
}

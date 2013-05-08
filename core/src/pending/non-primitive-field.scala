import scala.pickling._
import json._

case class Person(name: String, age: Int)
case class Philipp(mother: Person)

object Test extends App {
  val gudrun = Person("Gudrun", 62)
  val pckl = Philipp(gudrun).pickle
  println(pckl.value)
  println(pckl.unpickle[Philipp])
}

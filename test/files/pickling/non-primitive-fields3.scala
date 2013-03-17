import scala.pickling._
import json._

case class Person(name: String, age: Int)
case class Philipp(mother: Person, father: Person)

object Test extends App {
  val gudrun = Person("Gudrun", 62)
  val rudolf = Person("Rudolf", 70)
  val pckl = Philipp(gudrun, rudolf).pickle
  println(pckl.value)
  println(pckl.unpickle[Philipp])
}

import scala.pickling._
import json._

class Person(val name: String, val age: Int)
class Philipp(val nationality: String, val weird: Boolean, val mother: Person)

object Test extends App {
  val p = new Philipp("German", true, new Person("Gudrun", 62))
  val pickle = p.pickle
  println(pickle.value)
}
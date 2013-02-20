/*
  Test to directly generate a pickler for a *nested* arbitrary type (that is,
  a type that has at least one field with an arbitrary non-primitive type),
  and then use the generated pickler to pickle an instance of that type, by
  going through the `pickle` extension method.
*/

import scala.pickling._
import json._

class Person(val name: String, val age: Int)
class Philipp(val nationality: String, val weird: Boolean, val mother: Person)

object Test extends App {
  val p = new Philipp("German", true, new Person("Gudrun", 62))
  val pickle = p.pickle
  println(pickle.value)
}
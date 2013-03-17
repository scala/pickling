/*
  Test to generate a pickler for an arbitrary type, and then use the generated
  pickler to pickle an instance of that type by going through the `pickle`
  extension method.
*/

import scala.pickling._
import json._

class Person(val name: String, val age: Int)

object Test extends App {

  val p = new Person("Bob", 83)
  val pickle = p.pickle
  println(pickle.value)
}

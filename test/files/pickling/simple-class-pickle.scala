/*
  Test to directly generate a pickler for an arbitrary type, and then use the
  generated pickler to pickle an instance of that type.

  This test directly invokes the implicit macro which generates picklers (it
  doesn't use the implicit class that adds the `pickle` extension method)
*/

import scala.pickling._
import json._

class Person(val name: String, val age: Int)

object Test extends App {
  val pickle = new Person("Bob", 83).pickle
  println(pickle.value)
}

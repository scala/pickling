/*
  Test to directly generate a pickler for an arbitrary type implemented using
  a *case class*, and then use the generated pickler to pickle an instance of
  that type.

  This test directly invokes the implicit macro which generates picklers (it
  doesn't use the implicit class that adds the `pickle` extension method)
*/

import scala.pickling._
import json._

case class Person(name: String, age: Int)

object Test extends App {
  val pickler = Pickler.genPickler[Person]
  val pickle = pickler.pickle(new Person("Bob",83))
  println(pickle.value)
}

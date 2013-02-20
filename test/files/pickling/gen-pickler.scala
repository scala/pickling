/*
  Test to directly generate a pickler for an arbitrary type. Do nothing with
  it.

  This should just test the generation of a pickler for an arbitrary
  type, in this case Person. This test directly invokes the implicit
  macro which generates picklers (it doesn't use the implicit class that
  adds the pickle extension method)
*/

import scala.pickling._
import json._

class Person(val name: String, val age: Int)

object Test extends App {

  val pickler = genPickler[Person]

}
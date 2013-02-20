/*
  Test to indirectly generate a pickler for a type that extends another type
  by going through the `pickle` extension method. Note that any non-
  constructor vals are assumed to be able to be initialized upon unpickling,
  and thus are not pickled.
*/

import scala.pickling._
import json._

abstract class Person {
  val name: String
  val age: Int
}
abstract class Firefighter extends Person {
  val since: Int
}

object Test extends App {
  val f = new Firefighter {
    val name = "Jeff"
    val age = 45
    val since = 1990
  }

  val pickle = f.pickle
  println(pickle.value)
}

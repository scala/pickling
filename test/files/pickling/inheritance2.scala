/*
  Test to indirectly generate a pickler for a type that extends another type
  by going through the `pickle` extension method.

  All public state. But with one field that's a var. Reassignable fields must
  be pickled. (note that any non- constructor vals are typically assumed to be
  able to be initialized upon unpickling, and thus are not pickled.)
*/

import scala.pickling._
import json._

abstract class Creature {
  var species: String
}

abstract class Person extends Creature {
  var species = "human"
  val name: String
  val age: Int
}

class Firefighter(val name: String, val age: Int, val salary: Int) extends Person

object Test extends App {
  val f = new Firefighter("Josephine", 48, 40000)
  val pickle = f.pickle
  println(pickle.value)
}

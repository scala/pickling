/*
  Test to indirectly generate a pickler for a type that extends another type
  *and* which has default arguments, by going through the `pickle` extension
  method.

  No private state. All fields vals. (Note that any non-constructor vals are
  typically assumed to be able to be initialized upon unpickling, and thus are
  not pickled.)
*/

import scala.pickling._
import json._

abstract class Creature {
  val species: String
}

abstract class Person extends Creature {
  val name: String
  val age: Int
}

class Firefighter(val name: String, val age: Int, val salary: Int, val species: String = "human") extends Person

object Test extends App {
  val f = new Firefighter("Muriel", 41, 30000)
  val pickle = f.pickle
  println(pickle.value)
}

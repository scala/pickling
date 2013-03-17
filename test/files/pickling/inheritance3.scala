/*
  Test to indirectly generate a pickler for a type that extends another type
  by going through the `pickle` extension method.

  All public state. No vals that are not constructors (note that any non-
  constructor vals are typically assumed to be able to be initialized upon
  unpickling, and thus are not pickled.)
*/

import scala.pickling._
import json._

trait Person {
  val name: String
  val age: Int
}

abstract class Employee {
  val salary: Int
}

class Firefighter(val name: String, val age: Int, val salary: Int, val since: Int) extends Employee with Person

object Test extends App {
  val f = new Firefighter(
    name = "Joey",
    age = 32,
    salary = 30000,
    since = 1999
  )

  val pickle = f.pickle
  println(pickle.value)
}

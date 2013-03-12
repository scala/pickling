/*
  Test to indirectly generate a pickler for a type that extends another type
  by going through the `pickle` extension method. Note that any non-
  constructor vals are assumed to be able to be initialized upon unpickling,
  and thus are not pickled.
*/

import scala.pickling._
import json._

package outer {
  abstract class Person {
    val name: String
    val age: Int
  }

  class Firefighter(val name: String, val age: Int, val since: Int) extends Person

  package inner {
    class Employee(val name: String, val age: Int, val salary: Int) extends Person
  }
}

object Test extends App {
  import outer._

  val f: Person = new Firefighter(
    name = "Jeff",
    age = 45,
    since = 1990
  )

  val pickle = f.pickle
  println(pickle.value)
}

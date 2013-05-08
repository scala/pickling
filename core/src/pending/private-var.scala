/*
  Test to indirectly generate a pickler for a type that contains private state
  by going through the `pickle` extension method.

  Some private state. Private field a var. Reassignable fields must be
  pickled, and because it's private, an accessor must be generated for this
  field. (Note that any non-constructor vals are typically assumed to be able
  to be initialized upon unpickling, and thus are not pickled.)
*/


import scala.pickling._
import json._

case class Person(val name: String, val age: Int) {
  private var ssn: Int = 0
}

object Test extends App {
  val p = new Person("Bob", 42)
  val pickle = p.pickle
  println(pickle.value)
  println(pickle.unpickle[Person])
}

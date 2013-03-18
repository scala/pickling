/*
  Test to indirectly generate a pickler for a type that extends another type
  *and* which has private state, by going through the `pickle` extension
  method.

  Some private state. Private field a var. Reassignable fields must be
  pickled, and because it's private, an accessor must be generated for this
  field. (Note that any non-constructor vals are typically assumed to be able
  to be initialized upon unpickling, and thus are not pickled.)
*/


import scala.pickling._
import json._

trait Person {
  val name: String
  val age: Int
}

class Employee {
  private var salary: Int = 0
  def setSalary(newSalary: Int) = salary = newSalary
}

class Firefighter(val name: String, val age: Int, val since: Int) extends Employee with Person

object Test extends App {
  val f = new Firefighter("Jeff", 45, 1990)
  f.setSalary(30000)

  val pickle = f.pickle
  println(pickle.value)
}

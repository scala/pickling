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

case class Firefighter(val name: String, val age: Int, val salary: Int, val since: Int) extends Employee with Person

object Test extends App {
  val f = new Firefighter(
    name = "Joey",
    age = 32,
    salary = 30000,
    since = 1999
  )

  val pickleF = (f: Firefighter).pickle
  println(pickleF.value)
  println(pickleF.unpickle[Firefighter])

  val pickleE = (f: Employee).pickle
  println(pickleE.value)
  println(pickleE.unpickle[Employee])

  val pickleP = (f: Person).pickle
  println(pickleP.value)
  println(pickleP.unpickle[Person])
}

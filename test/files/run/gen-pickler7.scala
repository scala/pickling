import scala.pickling._
import json._

trait Person {
  val name: String
  val age: Int
}

abstract class Employee {
  val salary: Int
}

abstract class Firefighter extends Employee with Person {
  val since: Int
}

object Test extends App {
  val f = new Firefighter {
    val name = "Jeff"
    val age = 45
    val salary = 30000
    val since = 1990
  }

  val pickle = f.pickle
  println(pickle.value)
}

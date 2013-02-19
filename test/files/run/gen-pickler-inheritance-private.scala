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

abstract class Firefighter extends Employee with Person {
  val since: Int
}

object Test extends App {
  val f = new Firefighter {
    val name = "Jeff"
    val age = 45
    setSalary(30000)
    val since = 1990
  }

  val pickle = f.pickle
  println(pickle.value)
}

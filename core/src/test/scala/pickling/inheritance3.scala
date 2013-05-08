package scala.pickling.inheritance3

import org.scalatest.FunSuite
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

class Inheritance3Test extends FunSuite {
  test("main") {
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
}

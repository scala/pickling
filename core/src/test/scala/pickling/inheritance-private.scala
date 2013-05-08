package scala.pickling.inheritance.`private`

import org.scalatest.FunSuite
import scala.pickling._
import json._

trait Person {
  val name: String
  val age: Int
}

class Employee {
  private var salary: Int = 0
  def setSalary(newSalary: Int) = salary = newSalary
  def exposeSalary = salary
}

case class Firefighter(val name: String, val age: Int, val since: Int) extends Employee with Person

class InheritancePrivateTest extends FunSuite {
  test("main") {
    val f = new Firefighter("Jeff", 45, 1990)
    f.setSalary(30000)

    val pickleF = (f: Firefighter).pickle
    println(pickleF.value)
    println(pickleF.unpickle[Firefighter])
    println(pickleF.unpickle[Firefighter].exposeSalary)

    val pickleE = (f: Employee).pickle
    println(pickleE.value)
    println(pickleE.unpickle[Employee])
    println(pickleE.unpickle[Employee].exposeSalary)

    val pickleP = (f: Person).pickle
    println(pickleP.value)
    println(pickleP.unpickle[Person])
  }
}

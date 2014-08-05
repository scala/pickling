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
    assert(pickleF.value === """
      |{
      |  "$type": "scala.pickling.inheritance.private.Firefighter",
      |  "name": "Jeff",
      |  "age": 45,
      |  "since": 1990,
      |  "salary": 30000
      |}
    """.trim.stripMargin)
    assert(pickleF.unpickle[Firefighter] === f)
    assert(pickleF.unpickle[Firefighter].exposeSalary === 30000)

    val pickleE = (f: Employee).pickle
    assert(pickleE.value === """
      |{
      |  "$type": "scala.pickling.inheritance.private.Firefighter",
      |  "name": "Jeff",
      |  "age": 45,
      |  "since": 1990,
      |  "salary": 30000
      |}
    """.trim.stripMargin)
    assert(pickleE.unpickle[Employee] === f)
    assert(pickleF.unpickle[Employee].exposeSalary === 30000)

    val pickleP = (f: Person).pickle
    assert(pickleP.value === """
      |{
      |  "$type": "scala.pickling.inheritance.private.Firefighter",
      |  "name": "Jeff",
      |  "age": 45,
      |  "since": 1990,
      |  "salary": 30000
      |}
    """.trim.stripMargin)
    assert(pickleP.unpickle[Person] === f)
  }
}

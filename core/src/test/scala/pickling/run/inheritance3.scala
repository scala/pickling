package scala.pickling.inheritance3

import org.scalatest.FunSuite
import scala.pickling._, scala.pickling.Defaults._, json._

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
    assert(pickleF.value === """
      |{
      |  "$type": "scala.pickling.inheritance3.Firefighter",
      |  "name": "Joey",
      |  "age": 32,
      |  "salary": 30000,
      |  "since": 1999
      |}
    """.trim.stripMargin)
    assert(pickleF.unpickle[Firefighter] === f)

    val pickleE = (f: Employee).pickle
    assert(pickleE.value === """
      |{
      |  "$type": "scala.pickling.inheritance3.Firefighter",
      |  "name": "Joey",
      |  "age": 32,
      |  "salary": 30000,
      |  "since": 1999
      |}
    """.trim.stripMargin)
    assert(pickleE.unpickle[Employee] === f)

    val pickleP = (f: Person).pickle
    assert(pickleP.value === """
      |{
      |  "$type": "scala.pickling.inheritance3.Firefighter",
      |  "name": "Joey",
      |  "age": 32,
      |  "salary": 30000,
      |  "since": 1999
      |}
    """.trim.stripMargin)
    assert(pickleP.unpickle[Person] === f)
  }
}

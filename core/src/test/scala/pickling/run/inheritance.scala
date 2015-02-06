package scala.pickling.inheritance

import org.scalatest.FunSuite
import scala.pickling._, scala.pickling.Defaults._, json._

abstract class Creature {
  val species: String
}

abstract class Person extends Creature {
  val species = "human"
  val name: String
  val age: Int
}

case class Firefighter(val name: String, val age: Int, val salary: Int) extends Person

class InheritanceTest extends FunSuite {
  test("main") {
    val f = new Firefighter("Josephine", 48, 40000)

    val pickleF = (f: Firefighter).pickle
    assert(pickleF.value === """
      |{
      |  "$type": "scala.pickling.inheritance.Firefighter",
      |  "name": "Josephine",
      |  "age": 48,
      |  "salary": 40000
      |}
    """.trim.stripMargin)
    assert(pickleF.unpickle[Firefighter] === f)

    val pickleP = (f: Person).pickle
    assert(pickleP.value === """
      |{
      |  "$type": "scala.pickling.inheritance.Firefighter",
      |  "name": "Josephine",
      |  "age": 48,
      |  "salary": 40000
      |}
    """.trim.stripMargin)
    assert(pickleP.unpickle[Person] === f)

    val pickleC = (f: Creature).pickle
    assert(pickleC.value === """
      |{
      |  "$type": "scala.pickling.inheritance.Firefighter",
      |  "name": "Josephine",
      |  "age": 48,
      |  "salary": 40000
      |}
    """.trim.stripMargin)
    assert(pickleC.unpickle[Creature] === f)
  }
}

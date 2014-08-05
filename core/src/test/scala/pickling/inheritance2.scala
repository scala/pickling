package scala.pickling.inheritance2

import org.scalatest.FunSuite
import scala.pickling._
import json._

abstract class Creature {
  var species: String
}

abstract class Person extends Creature {
  var species = "human"
  val name: String
  val age: Int
}

case class Firefighter(val name: String, val age: Int, val salary: Int) extends Person

class Inheritance2Test extends FunSuite {
  test("main") {
    val f = new Firefighter("Josephine", 48, 40000)

    val pickleF = (f: Firefighter).pickle
    assert(pickleF.value === """
      |{
      |  "$type": "scala.pickling.inheritance2.Firefighter",
      |  "name": "Josephine",
      |  "age": 48,
      |  "salary": 40000,
      |  "species": "human"
      |}
    """.trim.stripMargin)
    assert(pickleF.unpickle[Firefighter] === f)
    assert(pickleF.unpickle[Firefighter].species === "human")

    val pickleP = (f: Person).pickle
    assert(pickleP.value === """
      |{
      |  "$type": "scala.pickling.inheritance2.Firefighter",
      |  "name": "Josephine",
      |  "age": 48,
      |  "salary": 40000,
      |  "species": "human"
      |}
    """.trim.stripMargin)
    assert(pickleP.unpickle[Person] === f)
    assert(pickleP.unpickle[Person].species === "human")

    val pickleC = (f: Creature).pickle
    assert(pickleC.value === """
      |{
      |  "$type": "scala.pickling.inheritance2.Firefighter",
      |  "name": "Josephine",
      |  "age": 48,
      |  "salary": 40000,
      |  "species": "human"
      |}
    """.trim.stripMargin)
    assert(pickleC.unpickle[Creature] === f)
    assert(pickleC.unpickle[Creature].species === "human")
  }
}

package scala.pickling.inheritance2

import org.scalatest.FunSuite
import scala.pickling._, scala.pickling.Defaults._, json._

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
  test("case class") {
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
    val uf = pickleF.unpickle[Firefighter]
    assert(uf === f)
    assert(uf.species === "human")
  }

  test("abstract class") {
    val f = new Firefighter("Josephine", 48, 40000)

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
    val up = pickleP.unpickle[Person]
    assert(up === f)
    assert(up.species === "human")
  }

  test("abstract class 2") {
    val f = new Firefighter("Josephine", 48, 40000)

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
    val uc = pickleC.unpickle[Creature]
    assert(uc === f)
    assert(uc.species === "human")
  }
}

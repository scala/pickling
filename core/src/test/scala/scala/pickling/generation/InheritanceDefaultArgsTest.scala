package scala.pickling.inheritance.defaultargs

import org.scalatest.FunSuite
import scala.pickling._, scala.pickling.Defaults._, json._

abstract class Creature {
  val species: String
}

abstract class Person extends Creature {
  val name: String
  val age: Int
}

case class Firefighter(val name: String, val age: Int, val salary: Int, val species: String = "human") extends Person

class InheritanceDefaultArgsTest extends FunSuite {
  test("main") {
    val f = new Firefighter("Muriel", 41, 30000)

    val pickleF = (f: Firefighter).pickle
    assert(pickleF.value === """
      |{
      |  "$type": "scala.pickling.inheritance.defaultargs.Firefighter",
      |  "name": "Muriel",
      |  "age": 41,
      |  "salary": 30000,
      |  "species": "human"
      |}
    """.trim.stripMargin)
    assert(pickleF.unpickle[Firefighter] === f)

    val pickleP = (f: Person).pickle
    assert(pickleP.value === """
      |{
      |  "$type": "scala.pickling.inheritance.defaultargs.Firefighter",
      |  "name": "Muriel",
      |  "age": 41,
      |  "salary": 30000,
      |  "species": "human"
      |}
    """.trim.stripMargin)
    assert(pickleP.unpickle[Person] === f)

    val pickleC = (f: Creature).pickle
    assert(pickleC.value === """
      |{
      |  "$type": "scala.pickling.inheritance.defaultargs.Firefighter",
      |  "name": "Muriel",
      |  "age": 41,
      |  "salary": 30000,
      |  "species": "human"
      |}
    """.trim.stripMargin)
    assert(pickleC.unpickle[Creature] === f)
  }
}

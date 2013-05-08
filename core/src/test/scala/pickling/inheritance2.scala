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
    println(pickleF.value)
    println(pickleF.unpickle[Firefighter])
    println(pickleF.unpickle[Firefighter].species)

    val pickleP = (f: Person).pickle
    println(pickleP.value)
    println(pickleP.unpickle[Person])
    println(pickleP.unpickle[Person].species)

    val pickleC = (f: Creature).pickle
    println(pickleC.value)
    println(pickleC.unpickle[Creature])
    println(pickleC.unpickle[Creature].species)
  }
}

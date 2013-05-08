package scala.pickling.inheritance

import org.scalatest.FunSuite
import scala.pickling._
import json._

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
    println(pickleF.value)
    println(pickleF.unpickle[Firefighter])

    val pickleP = (f: Person).pickle
    println(pickleP.value)
    println(pickleP.unpickle[Person])

    val pickleC = (f: Creature).pickle
    println(pickleC.value)
    println(pickleC.unpickle[Creature])
  }
}

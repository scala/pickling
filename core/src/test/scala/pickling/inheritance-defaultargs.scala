package scala.pickling.inheritance.defaultargs

import org.scalatest.FunSuite
import scala.pickling._
import json._

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

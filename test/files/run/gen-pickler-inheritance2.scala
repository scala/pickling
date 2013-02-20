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

class Firefighter(val name: String, val age: Int, val salary: Int) extends Person

object Test extends App {
  val f = new Firefighter("Josephine", 48, 40000)
  val pickle = f.pickle
  println(pickle.value)
}

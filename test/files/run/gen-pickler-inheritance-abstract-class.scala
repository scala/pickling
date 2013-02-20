import scala.pickling._
import json._

abstract class Creature {
  val species: String
}

abstract class Person extends Creature {
  val name: String
  val age: Int
}

class Firefighter(val name: String, val age: Int, val salary: Int, val species: String = "human") extends Person

object Test extends App {
  val f = new Firefighter("Muriel", 41, 30000)
  val pickle = f.pickle
  println(pickle.value)
}

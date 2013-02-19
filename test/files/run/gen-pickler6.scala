import scala.pickling._
import json._

abstract class Person {
  val name: String
  val age: Int
}
abstract class Firefighter extends Person {
  val since: Int
}

object Test extends App {
  val f = new Firefighter {
    val name = "Jeff"
    val age = 45
    val since = 1990
  }

  val pickle = f.pickle
  println(pickle.value)
}

import scala.pickling._
import json._

class Person(private val name: String, age: Int, val hobby: Hobby) {
  // NOTE: be careful not to reference age anywhere, so that it's elided by the "constructors" phase
  override def toString = s"Person(name = $name, hobby = $hobby)"
}
class Hobby(var name: String, private var notes: String, private val attitude: String) {
  override def toString = s"Hobby(name = $name, notes = $notes, attitude = $attitude)"
}

object Test extends App {
  val pickle = (new Person("Eugene", 25, new Hobby("hacking", "mostly Scala", "loving it")): Any).pickle
  println(pickle)
  println(pickle.unpickle[Person])
}

import scala.pickling._
import json._

class Person(private val name: String, age: Int, val hobby: Hobby)
class Hobby(var name: String, private var notes: String, private val attitude: String)

object Test extends App {
  val pickle = new Person("Eugene", 25, new Hobby("hacking", "mostly Scala", "loving it")).pickle
  println(pickle)
  // println(pickle.unpickle[Person])
}

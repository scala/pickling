import scala.pickling._
import json._

class Person(val name: String, val age: Int)

object Test extends App {

  val pickler = genPickler[Person]

}
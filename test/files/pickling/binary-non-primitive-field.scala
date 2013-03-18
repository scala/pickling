import scala.pickling._
import binary._

case class Person(name: String, age: Int)
case class Philipp(mother: Person)

object Test extends App {
  val gudrun = Person("Gudrun", 62)
  val p = Philipp(gudrun)
  val pckl = p.pickle
  
  println(pckl.value.mkString("[", ",", "]"))
  println(pckl.unpickle[Philipp])
}

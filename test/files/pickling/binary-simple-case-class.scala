import scala.pickling._
import binary._
import reflect.runtime.{universe => ru}
import ru._

case class Person(age: Int)

object Test extends App {

  val p = Person(43)

  val pickle = p.pickle

  println("ARRAY:")
  println(pickle.value.mkString("[", ",", "]"))

  val up = pickle.unpickle[Person]
  println(s"unpickled: $up")
}

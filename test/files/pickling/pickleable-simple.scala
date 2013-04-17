import scala.pickling._
import binary._

@pickleable case class Person(name: String)

object Test extends App {
  val p = new Person("joe")
  val pickle = p.pickle
  println(pickle.value.asInstanceOf[Array[Byte]].mkString("[", ",", "]"))
  val ff = pickle.unpickle[Person]
  println(ff)
}

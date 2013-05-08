import scala.pickling._
import binary._

@pickleable class Person(val name: String)

case class Firefigher(override val name: String, salary: Int) extends Person(name)

object Test extends App {
  val p = new Firefigher("joe", 30000)
  val pickl = p.pickle
  println(pickl.value.asInstanceOf[Array[Byte]].mkString("[", ",", "]"))
  val ff = pickl.unpickle[Person]
  println(ff)
}

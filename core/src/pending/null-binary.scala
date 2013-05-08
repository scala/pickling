import scala.pickling._
import binary._

class D
final class E
case class C(val x: String, val y: Int, val d: D, val e: E)

object Test extends App {
  val c = C(null, 0, null, null)
  val pickle = c.pickle
  println(pickle.value.mkString("[", ",", "]"))
  println(pickle.unpickle[C])
}
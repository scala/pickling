import scala.pickling._
import binary._

object Test extends App {
  val pickle = Vector(1, 2, 3).pickle
  println(pickle)
  println(pickle.unpickle[Vector[Int]])
}

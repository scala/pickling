import scala.pickling._
import binary._

object Test extends App {
  val pickle = List(1, 2, 3).pickle
  println(pickle)
  println(pickle.unpickle[List[Int]])
}

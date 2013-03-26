import scala.pickling._
import binary._

case class C(arr: Array[Int]) { override def toString = s"""C(${arr.mkString("[", ",", "]")})""" }

object Test extends App {
  val pickle = C(Array(1, 2, 3)).pickle
  println(pickle.value)
  println(pickle.unpickle[C])
}

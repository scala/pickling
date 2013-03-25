import scala.pickling._
import binary._

object Test extends App {
  val tup2 = ("hewrow", 2)
  val pckl = tup2.pickle
  println(pckl.unpickle[(String, Int)])
}
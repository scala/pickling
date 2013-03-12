import scala.pickling._
import json._

object Test extends App {
  val pickler = implicitly[Pickler[Int]]
  val pickle = pickler.pickle(12)
  println(pickle.value)
}
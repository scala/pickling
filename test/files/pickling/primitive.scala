import scala.pickling._
import json._

object Test extends App {
  val pickle = 12.pickle
  println(pickle.value)
}
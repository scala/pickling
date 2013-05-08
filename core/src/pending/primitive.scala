import scala.pickling._
import json._

object Test extends App {
  println(12.pickle.value)
  println(12.pickle.unpickle[Int])
  println(12.pickle.unpickle[Any])

  println("12".pickle.value)
  println("12".pickle.unpickle[String])
  println("12".pickle.unpickle[Any])

  println(true.pickle.value)
  println(true.pickle.unpickle[Boolean])
  println(true.pickle.unpickle[Any])
}
import scala.pickling._
import json._

object D {
  val shouldntSerializeMe = 42
}

object Test extends App {
  val pickle = D.pickle
  println(pickle)
  println(pickle.unpickle[D.type] eq D)
}

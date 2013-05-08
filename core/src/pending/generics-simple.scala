import scala.pickling._
import json._

case class C[T](x: T)

object Test extends App {
  val p = C(2).pickle
  println(p)
  println(p.unpickle[C[Int]])
}

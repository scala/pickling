import scala.pickling._
import json._

case class C(x: Any)

object Test extends App {
  val pckl = new C(2).pickle
  println(pckl)
  println(pckl.unpickle[C])
}

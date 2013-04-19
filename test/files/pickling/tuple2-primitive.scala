import scala.reflect.runtime.universe._

import scala.pickling._
import binary._

object Test extends App {
  val tup2 = ("hewrow", 2)
  val pckl = tup2.pickle
  println(pckl.value.asInstanceOf[Array[Byte]].mkString("[", ",", "]"))
  println(pckl.unpickle[(String, Int)])

  val tup3 = ("hewrow", 2, "bye")
  val pckl3 = tup3.pickle
  println(pckl3.unpickle[(String, Int, String)])
}

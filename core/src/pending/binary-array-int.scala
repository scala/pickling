import scala.pickling._
import scala.pickling.binary._

import reflect.runtime.universe._

object Test extends App {

  val ia = Array[Int](30, 31)

  val pickle: BinaryPickle = ia.pickle
  println(pickle.value.mkString("[", ",", "]"))

  val readArr = pickle.unpickle[Array[Int]]
  println(readArr.mkString("[", ",", "]"))

}

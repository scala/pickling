package scala.pickling.binary.array.int

import org.scalatest.FunSuite
import scala.pickling._
import binary._
import scala.reflect.runtime.universe._

class BinaryArrayIntTest extends FunSuite {
  test("main") {
    val ia = Array[Int](30, 31)

    val pickle: BinaryPickle = ia.pickle
    assert(pickle.value.mkString("[", ",", "]") === "[0,0,0,2,30,0,0,0,31,0,0,0]")

    val readArr = pickle.unpickle[Array[Int]]
    assert(readArr.mkString("[", ",", "]") === "[30,31]")
  }
}

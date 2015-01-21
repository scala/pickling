package scala.pickling.binary.array.int

import org.scalatest.FunSuite
import scala.pickling._, scala.pickling.Defaults._, binary._

import scala.reflect.runtime.universe._

class BinaryArrayIntTest extends FunSuite {
  test("main") {
    val ia = Array[Int](30, 31)

    val pickle: BinaryPickle = ia.pickle
    assert(pickle.value.mkString("[", ",", "]") === "[0,0,0,22,115,99,97,108,97,46,65,114,114,97,121,91,115,99,97,108,97,46,73,110,116,93,0,0,0,2,30,0,0,0,31,0,0,0]")

    val readArr = pickle.unpickle[Array[Int]]
    assert(readArr.mkString("[", ",", "]") === "[30,31]")
  }
}

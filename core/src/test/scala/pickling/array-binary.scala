package scala.pickling.arraybinary

import org.scalatest.FunSuite
import scala.pickling._
import binary._

case class C(arr: Array[Int]) { override def toString = s"""C(${arr.mkString("[", ",", "]")})""" }

class ArrayBinaryTest extends FunSuite {
  test("main") {
    val expectedPickle = "BinaryPickle([0,0,0,28,115,99,97,108,97,46,112,105,99,107,108,105,110,103,46,97,114,114,97,121,98,105,110,97,114,121,46,67,0,0,0,3,1,0,0,0,2,0,0,0,3,0,0,0])"
    val expectedUnpickle = "C([1,2,3])"

    val pickle = C(Array(1, 2, 3)).pickle
    assert(pickle.toString === expectedPickle)
    assert(pickle.unpickle[C].toString === expectedUnpickle)
  }
}

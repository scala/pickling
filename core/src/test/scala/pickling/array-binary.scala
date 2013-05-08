package scala.pickling.array.binary

import org.scalatest.FunSuite
import scala.pickling._
import binary._

case class C(arr: Array[Int]) { override def toString = s"""C(${arr.mkString("[", ",", "]")})""" }

class ArrayBinaryTest extends FunSuite {
  test("main") {
    val pickle = C(Array(1, 2, 3)).pickle
    assert(pickle.toString === "BinaryPickle([0,0,0,29,115,99,97,108,97,46,112,105,99,107,108,105,110,103,46,97,114,114,97,121,46,98,105,110,97,114,121,46,67,0,0,0,3,1,0,0,0,2,0,0,0,3,0,0,0])")
    assert(pickle.unpickle[C].toString === "C([1,2,3])")
  }
}

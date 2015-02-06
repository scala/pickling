package scala.pickling.array.binary

import org.scalatest.FunSuite
import scala.pickling.Defaults._
import scala.pickling.binary._

case class C(arr: Array[Int]) { override def toString = s"""C(${arr.mkString("[", ",", "]")})""" }

case class D(x: Int)

class ArrayBinaryTest extends FunSuite {
  test("main") {
    val pickle = C(Array(1, 2, 3)).pickle
    assert(pickle.toString === "BinaryPickle([0,0,0,29,115,99,97,108,97,46,112,105,99,107,108,105,110,103,46,97,114,114,97,121,46,98,105,110,97,114,121,46,67,0,0,0,3,1,0,0,0,2,0,0,0,3,0,0,0])")
    assert(pickle.unpickle[C].toString === "C([1,2,3])")
  }

  test("nested") {
    val ai = Array(1, 2, 3)
    val aai = Array(ai, ai)
    val aaai = Array(aai, aai)

    val p = aaai.pickle

    val up = p.unpickle[Array[Array[Array[Int]]]]
    assert(up(0)(0)(0) == 1)
  }

  test("nested tuple") {
    def mkString(o: (Array[Array[Double]], D)): String =
      s"${o._2}, ${o._1(0).mkString(",")}, ${o._1(1).mkString(",")}"

    val a = Array[Double](0.3d, 0.2d)
    val o = (Array(a, a), D(10))
    val anyObj: Any = o

    val p = anyObj.pickle

    val up = p.unpickle[Any]

    val upT = up.asInstanceOf[(Array[Array[Double]], D)]
    assert(mkString(upT) == mkString(o))
  }
}

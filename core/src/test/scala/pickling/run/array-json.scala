package scala.pickling.array.json

import org.scalatest.FunSuite
import scala.pickling.Defaults._
import scala.pickling.json._

case class C(arr: Array[Int]) { override def toString = s"""C(${arr.mkString("[", ",", "]")})""" }

case class D(x: Int)

class ArrayJsonTest extends FunSuite {
  test("main") {
    val expectedPickle = """
    |JSONPickle({
    |  "$type": "scala.pickling.array.json.C",
    |  "arr": [
    |    1,
    |    2,
    |    3
    |  ]
    |})
    """.stripMargin.trim
    val expectedUnpickle = "C([1,2,3])"

    val pickle = C(Array(1, 2, 3)).pickle
    assert(pickle.toString === expectedPickle)
    assert(pickle.unpickle[C].toString === expectedUnpickle)
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

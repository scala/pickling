package scala.pickling.array.json

import org.scalatest.FunSuite
import scala.pickling._
import json._

case class C(arr: Array[Int]) { override def toString = s"""C(${arr.mkString("[", ",", "]")})""" }

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
}

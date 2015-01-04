package scala.pickling.c.x.any.json

import org.scalatest.FunSuite
import scala.pickling._
import json._
import AllPicklers._

case class C(x: Any)

class CXAnyJsonTest extends FunSuite {
  test("main") {
    val c = new C(2)
    val pckl = c.pickle
    assert(pckl.toString === """
      |JSONPickle({
      |  "tpe": "scala.pickling.c.x.any.json.C",
      |  "x": {
      |    "tpe": "scala.Int",
      |    "value": 2
      |  }
      |})
    """.trim.stripMargin)
    assert(pckl.unpickle[C] === c)
  }
}

package scala.pickling.c.x.any.json

import org.scalatest.FunSuite
import scala.pickling._, scala.pickling.Defaults._, json._

case class C(x: Any)

class CXAnyJsonTest extends FunSuite {
  test("main") {
    val c = new C(2)
    val pckl = c.pickle
    assert(pckl.toString === """
      |JSONPickle({
      |  "$type": "scala.pickling.c.x.any.json.C",
      |  "x": {
      |    "$type": "scala.Int",
      |    "value": 2
      |  }
      |})
    """.trim.stripMargin)
    assert(pckl.unpickle[C] === c)
  }
}

package scala.pickling.json.list.t.`new`

import org.scalatest.FunSuite
import scala.pickling._
import json._

class JsonListTNewTest extends FunSuite {
  test("main") {
    val pickle = List(1, 2, 3).pickle
    assert(pickle.toString === """
      |JSONPickle({
      |  "tpe": "scala.collection.immutable.$colon$colon[scala.Int]",
      |  "elems": [
      |    1,
      |    2,
      |    3
      |  ]
      |})
    """.stripMargin.trim)
    assert(pickle.unpickle[List[Int]] === List(1, 2, 3))
  }
}

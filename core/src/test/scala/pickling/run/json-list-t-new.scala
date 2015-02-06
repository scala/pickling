package scala.pickling.json.list.t.`new`

import org.scalatest.FunSuite
import scala.pickling._, scala.pickling.Defaults._, json._

class JsonListTNewTest extends FunSuite {
  test("main") {
    val pickle = List(1, 2, 3).pickle
    assert(pickle.toString === """
      |JSONPickle({
      |  "$type": "scala.collection.immutable.$colon$colon[scala.Int]",
      |  "head": 1,
      |  "tl": {
      |    "$type": "scala.collection.immutable.$colon$colon[scala.Int]",
      |    "head": 2,
      |    "tl": {
      |      "$type": "scala.collection.immutable.$colon$colon[scala.Int]",
      |      "head": 3,
      |      "tl": {
      |        "$type": "scala.collection.immutable.Nil.type"
      |      }
      |    }
      |  }
      |})
    """.stripMargin.trim)
    assert(pickle.unpickle[List[Int]] === List(1, 2, 3))
  }
}

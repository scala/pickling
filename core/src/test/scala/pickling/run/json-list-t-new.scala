package scala.pickling.json.list.t.`new`

import org.scalatest.FunSuite
import scala.pickling._, scala.pickling.Defaults._, json._

class JsonListTNewTest extends FunSuite {
  test("main") {
    val pickle = List(1, 2, 3).pickle
    // NOTE - Previously lists would encode as hd/tl lists, which was dramatically less efficient.
    assert(pickle.toString === """
      |JSONPickle({
      |  "$type": "scala.collection.immutable.List[scala.Int]",
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

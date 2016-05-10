package scala.pickling.`null`.json

import org.scalatest.FunSuite
import scala.pickling._, scala.pickling.Defaults._, json._

class D
final class E
case class C(val x: String, val y: Int, val d: D, val e: E)

class NullJsonTest extends FunSuite {
  test("main") {
    val c = C(null, 0, null, null)
    val pickle = c.pickle
    assert(pickle.value === """
      |{
      |  "$type": "scala.pickling.null.json.C",
      |  "x": null,
      |  "y": 0,
      |  "d": null,
      |  "e": null
      |}
    """.stripMargin.trim)
    assert(pickle.unpickle[C].toString === c.toString)
  }
}

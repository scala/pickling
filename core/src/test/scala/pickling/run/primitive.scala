package scala.pickling.primitive

import org.scalatest.FunSuite
import scala.pickling._, scala.pickling.Defaults._, json._

class PrimitiveTest extends FunSuite {
  test("main") {
    assert(12.pickle.value === """
      |{
      |  "$type": "scala.Int",
      |  "value": 12
      |}
    """.stripMargin.trim)
    assert(12.pickle.unpickle[Int] === 12)
    assert(12.pickle.unpickle[Any] === 12)

    assert("12".pickle.value === """
      |{
      |  "$type": "java.lang.String",
      |  "value": "12"
      |}
    """.stripMargin.trim)
    assert("12".pickle.unpickle[String] === "12")
    assert("12".pickle.unpickle[Any] === "12")

    assert(true.pickle.value === """
      |{
      |  "$type": "scala.Boolean",
      |  "value": true
      |}
    """.stripMargin.trim)
    assert(true.pickle.unpickle[Boolean] === true)
    assert(true.pickle.unpickle[Any] === true)
  }
}

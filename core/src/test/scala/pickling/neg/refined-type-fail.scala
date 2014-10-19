package scala.pickling.refinedtypefail

import scala.pickling._
import NegativeCompilation._
import org.scalatest.FunSuite

class RefinedTypeFailTest extends FunSuite {
  test("main") {
    expectError("could not find implicit pickler for refined type") {
      """import _root_.scala.pickling._
        |import _root_.scala.pickling.json._
        |
        |class C { type Cap }
        |
        |object Test {
        |  type T = C { type Cap = Int }
        |  val x: T = null
        |  val p = x.pickle
        |}""".stripMargin
    }
  }
}

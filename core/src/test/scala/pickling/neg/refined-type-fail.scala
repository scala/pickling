package scala.pickling.refinedtypefail

import scala.pickling._
import NegativeCompilation._
import org.scalatest.FunSuite

class RefinedTypeFailTest extends FunSuite {
  test("main") {
    expectError("Cannot generate") {
      """import _root_.scala.pickling._
        |import _root_.scala.pickling.Defaults.{ pickleOps, unpickleOps }
        |import _root_.scala.pickling.json._
        |
        |class C { type Cap }
        |
        |val x: C { type Cap = Int } = new C { type Cap = Int }
        |val p = x.pickle""".stripMargin
    }
  }
}

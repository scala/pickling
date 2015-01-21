package scala.pickling.staticonlyfail1

import scala.pickling._
import NegativeCompilation._
import org.scalatest.FunSuite

class StaticOnlyFail1Test extends FunSuite {
  test("main") {
    expectError("Cannot generate") {
      """
        | import _root_.scala.pickling._
        | import _root_.scala.pickling.Defaults.{ pickleOps, unpickleOps }
        | import _root_.scala.pickling.json._
        | import _root_.scala.pickling.static.StaticOnly
        |
        | class C(val fld: Any)
        |
        | val x: C = new C(1)
        | val pickle: JSONPickle = x.pickle
      """.stripMargin
    }
  }
}

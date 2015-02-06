package scala.pickling.staticonlyfail2

import scala.pickling._
import NegativeCompilation._
import org.scalatest.FunSuite

class StaticOnlyFail2Test extends FunSuite {
  test("main") {
    expectError("Cannot generate") {
      """
        | import _root_.scala.pickling._
        | import _root_.scala.pickling.Defaults.{ pickleOps, unpickleOps }
        | import _root_.scala.pickling.json._
        | import _root_.scala.pickling.static.StaticOnly
        |
        | // this case is with an unsealed class
        | // as part of a complicated hierarchy
        | sealed trait C
        | sealed trait D extends C
        | final case class E(fld: Int) extends C
        | case class F(bar: String) extends C
        | // this is the class that makes it fail
        | class G(foo: Double) extends D
        |
        | val x: C = E(1)
        | val pickle: JSONPickle = x.pickle
      """.stripMargin
    }
  }
}

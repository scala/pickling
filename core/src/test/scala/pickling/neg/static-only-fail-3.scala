package scala.pickling.staticonlyfail3

import scala.pickling._
import NegativeCompilation._
import org.scalatest.FunSuite

class StaticOnlyFail3Test extends FunSuite {
  test("main") {
    expectError("Cannot generate") {
      """
        | import _root_.scala.pickling._
        | import _root_.scala.pickling.Defaults.{ pickleOps, unpickleOps }
        | import _root_.scala.pickling.json._
        | import _root_.scala.pickling.static.StaticOnly
        |
        | // this case has a type parameter on a
        | // subtype in the hierarchy
        | sealed trait C
        | final case class D[T](fld: T) extends C
        |
        | val x: C = D(1)
        | val pickle: JSONPickle = x.pickle
      """.stripMargin
    }
  }
}

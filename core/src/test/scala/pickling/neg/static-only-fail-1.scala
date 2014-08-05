package scala.pickling.staticonlyfail1

import scala.pickling._
import NegativeCompilation._
import org.scalatest.FunSuite
import org.junit.Test

class StaticOnlyFail1Test {
  @Test
  def `Any is never statically serializable`() {
    expectError("cannot generate fully static pickler") {
      """
        | import _root_.scala.pickling._
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

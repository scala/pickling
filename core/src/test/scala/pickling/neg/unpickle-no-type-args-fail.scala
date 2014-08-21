package scala.pickling.unpicklenotypeargsfail

import scala.pickling._
import NegativeCompilation._
import org.scalatest.FunSuite

class UnpickleNoTypeArgsFailTest extends FunSuite {
  test("main") {
    expectError("cannot unpickle because the (inferred) type argument to unpickle is abstract. Typically, this is caused by omitting an explicit type argument for unpickle.") {
      """
        | import _root_.scala.pickling._
        | import _root_.scala.pickling.json._
        |
        | Array(1, 2, 3).pickle.unpickle
      """.stripMargin
    }
  }
}

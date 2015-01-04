package scala.pickling.unpicklenotypeargsfail

import scala.pickling._
import AllPicklers._
import NegativeCompilation._
import org.scalatest.FunSuite

class UnpickleNoTypeArgsFailTest extends FunSuite {
  test("main") {
    expectError("ambiguous implicit values") {
      """
        | import _root_.scala.pickling._
        | import _root_.scala.pickling.json._
        | import _root_.scala.pickling.AllPicklers._
        |
        | Array(1, 2, 3).pickle.unpickle
      """.stripMargin
    }
  }
}

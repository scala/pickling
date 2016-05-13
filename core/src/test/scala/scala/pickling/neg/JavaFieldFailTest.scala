package scala.pickling.javafieldfail

import scala.pickling._
import NegativeCompilation._
import org.scalatest.FunSuite

class JavaFieldFailTest extends FunSuite {
  test("x.pickle does not compile for FakeByte") {
    expectError("Cannot generate") {
      """import _root_.scala.pickling._
        |import _root_.scala.pickling.Defaults._
        |import _root_.scala.pickling.json._
        |import _root_.scala.pickling.static._
        |import scala.pickling.javafieldfail.FakeByte
        |
        |val x: FakeByte = new FakeByte(10)
        |val pkl = x.pickle""".stripMargin
    }
  }
}

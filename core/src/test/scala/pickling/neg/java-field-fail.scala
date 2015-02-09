package scala.pickling.javafieldfail

import scala.pickling._
import NegativeCompilation._
import org.scalatest.FunSuite

class JavaFieldFailTest extends FunSuite {
  test("main") {
    expectError("Cannot generate") {
      """import _root_.scala.pickling._
        |import _root_.scala.pickling.Defaults._
        |import _root_.scala.pickling.json._
        |import _root_.scala.pickling.static._
        |
        |val x: java.lang.Byte = 10.toByte
        |
        |val p = x.pickle""".stripMargin
    }
  }
}

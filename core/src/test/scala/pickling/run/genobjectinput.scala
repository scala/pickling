package scala.pickling.test

import org.scalatest.FunSuite
import scala.pickling.util.GenObjectInput
import java.io.ObjectInput

class GenObjectInputTest extends FunSuite {
  test("two bytes") {
    val byte0: Byte = 5
    val byte1: Byte = 10
    val in = GenObjectInput.genInstance[(Byte, Byte)]((byte0, byte1))
    val res = List[Any](in.readByte, in.readByte)
    assert(res === List[Any](byte0, byte1))
  }

  test("three things") {
    val byte: Byte = 7
    val boolean = true
    val char = 'c'
    val in2 = GenObjectInput.genInstance[(Byte, Boolean, Char)]((byte, true, char))
    val res2 = List[Any](in2.readByte, in2.readBoolean, in2.readChar)
    assert(res2 === List[Any](byte, boolean, char))
  }
}

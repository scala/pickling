package scala.pickling.test

import org.scalatest.FunSuite
import scala.pickling.util.Externalizables

class GenObjectInputTest extends FunSuite {
  test("Byte, Byte") {
    val byte0: Byte = 5
    val byte1: Byte = 10
    val in = Externalizables.genInput[(Byte, Byte)]((byte0, byte1))
    val res = List[Any](in.readByte, in.readByte)
    assert(res === List[Any](byte0, byte1))
  }

  test("Byte, Boolean, Char") {
    val byte: Byte = 7
    val boolean = true
    val char = 'c'
    val in2 = Externalizables.genInput[(Byte, Boolean, Char)]((byte, true, char))
    val res2 = List[Any](in2.readByte, in2.readBoolean, in2.readChar)
    assert(res2 === List[Any](byte, boolean, char))
  }
}

package scala.pickling.c.x.any.binary

import org.scalatest.FunSuite
import scala.pickling._, scala.pickling.Defaults._, binary._

case class C(x: Any)

class CXAnyBinaryTest extends FunSuite {
  test("main") {
    val c = new C(2)
    val pckl = c.pickle
    assert(pckl.toString === "BinaryPickle([0,0,0,31,115,99,97,108,97,46,112,105,99,107,108,105,110,103,46,99,46,120,46,97,110,121,46,98,105,110,97,114,121,46,67,0,0,0,9,115,99,97,108,97,46,73,110,116,0,0,0,2])")
    assert(pckl.unpickle[C] === c)
  }
}

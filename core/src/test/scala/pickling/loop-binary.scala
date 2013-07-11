package scala.pickling.loop.binary

import org.scalatest.FunSuite
import scala.pickling._
import binary._

class C(val name: String, var c: C)

class LoopBinaryTest extends FunSuite {
  test("main") {
    val c1 = new C("c1", null)
    val c2 = new C("c2", c1)
    val c3 = new C("c3", c2)
    c1.c = c3

    val pickle = c1.pickle
    assert(pickle.toString === "")

    val c11 = pickle.unpickle[C]
    val c13 = c11.c
    val c12 = c13.c
    assert(c11.name === "c1")
    assert(c12.name === "c2")
    assert(c13.name === "c3")
    assert(c12.c === c11)
  }
}

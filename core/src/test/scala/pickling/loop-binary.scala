package scala.pickling.loop.binary

import org.scalatest.FunSuite
import scala.pickling._
import binary._

class C(val name: String, var c: C)

class LoopBinaryTest extends FunSuite {
  val c1 = new C("c1", null)
  val c2 = new C("c2", c1)
  val c3 = new C("c3", c2)

  test("loop-ref") {
    c1.c = c3
    val pickle = c1.pickle
    assert(pickle.toString === "BinaryPickle([0,0,0,28,115,99,97,108,97,46,112,105,99,107,108,105,110,103,46,108,111,111,112,46,98,105,110,97,114,121,46,67,0,0,0,2,99,49,-1,0,0,0,2,99,51,-1,0,0,0,2,99,50,-3,0,0,0,0])")

    val c11 = pickle.unpickle[C]
    val c13 = c11.c
    val c12 = c13.c
    assert(c11.name === "c1")
    assert(c12.name === "c2")
    assert(c13.name === "c3")
    assert(c12.c === c11)
  }

  test("loop-noref") {
    intercept[StackOverflowError] {
      import norefs._
      c1.c = c3
      c2.pickle
    }
  }

  test("noloop-noref") {
    import norefs._
    c1.c = null
    val pickle = c3.pickle
    assert(pickle.toString === "BinaryPickle([0,0,0,28,115,99,97,108,97,46,112,105,99,107,108,105,110,103,46,108,111,111,112,46,98,105,110,97,114,121,46,67,0,0,0,2,99,51,-1,0,0,0,2,99,50,-1,0,0,0,2,99,49,-2])")

    val c23 = pickle.unpickle[C]
    val c22 = c23.c
    val c21 = c22.c
    assert(c23.name === "c3")
    assert(c22.name === "c2")
    assert(c21.name === "c1")
  }
}

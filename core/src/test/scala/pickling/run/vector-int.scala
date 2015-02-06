package scala.pickling.vector.int

import org.scalatest.FunSuite
import scala.pickling._, scala.pickling.Defaults._, binary._

class VectorIntTest extends FunSuite {
  test("main") {
    val v = Vector(1, 2, 3)
    val pickle = v.pickle
    val expected0 = "BinaryPickle([0,0,0,44,115,99,97,108,97,46,99,111,108,108,101,99,116,105,111,110,46,105,109,109,117,116,97,98,108,101,46,86,101,99,116,111,114,91,115,99,97,108,97,46,73,110,116,93,0,0,0,3,0,0,0,1,0,0,0,2,0,0,0,3,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0])"
    val expected = "BinaryPickle([0,0,0,44,115,99,97,108,97,46,99,111,108,108,101,99,116,105,111,110,46,105,109,109,117,116,97,98,108,101,46,86,101,99,116,111,114,91,115,99,97,108,97,46,73,110,116,93,0,0,0,3,0,0,0,1,0,0,0,2,0,0,0,3])"
    assert(pickle.toString === expected0)
    assert(pickle.unpickle[Vector[Int]] === v)
  }
}

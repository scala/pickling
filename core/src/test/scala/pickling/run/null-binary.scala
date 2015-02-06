package scala.pickling.`null`.binary

import org.scalatest.FunSuite
import scala.pickling._, scala.pickling.Defaults._, binary._

class D
final class E
case class C(val x: String, val y: Int, val d: D, val e: E)

class NullBinaryTest extends FunSuite {
  test("main") {
    val c = C(null, 0, null, null)
    val pickle = c.pickle
    assert(pickle.value.mkString("[", ",", "]") === "[0,0,0,28,115,99,97,108,97,46,112,105,99,107,108,105,110,103,46,110,117,108,108,46,98,105,110,97,114,121,46,67,-2,0,0,0,0,-2,-2]")
    assert(pickle.unpickle[C].toString === c.toString)
  }
}

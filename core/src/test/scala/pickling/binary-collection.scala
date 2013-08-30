package scala.pickling.binary.collection

import org.scalatest.FunSuite
import scala.pickling._
import binary._

class BinaryCollectionTest extends FunSuite {
  test("main") {
    val pickle = Seq(1, 2, 3).pickle
    assert(pickle.toString === "BinaryPickle([0,0,0,31,115,99,97,108,97,46,99,111,108,108,101,99,116,105,111,110,46,83,101,113,91,115,99,97,108,97,46,73,110,116,93,0,0,0,3,0,0,0,1,0,0,0,2,0,0,0,3,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0])")
    assert(pickle.unpickle[Seq[Int]] === Seq(1, 2, 3))
  }
}

package scala.pickling.binary.list.t.`new`

import org.scalatest.FunSuite
import scala.pickling._
import binary._

class BinaryListTNewTest extends FunSuite {
  test("main") {
    val pickle = List(1, 2, 3).pickle
    assert(pickle.toString === "BinaryPickle([2,0,0,0,50,115,99,97,108,97,46,99,111,108,108,101,99,116,105,111,110,46,105,109,109,117,116,97,98,108,101,46,36,99,111,108,111,110,36,99,111,108,111,110,91,115,99,97,108,97,46,73,110,116,93,0,0,0,3,0,0,0,1,0,0,0,2,0,0,0,3,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0])")
    assert(pickle.unpickle[List[Int]] === List(1, 2, 3))
  }
}

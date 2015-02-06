package scala.pickling.tuple2.primitive

import org.scalatest.FunSuite
import scala.pickling._, scala.pickling.Defaults._, binary._

class Tuple2PrimitiveTest extends FunSuite {
  test("main") {
    val tup2 = ("hewrow", 2)
    val pckl = tup2.pickle
    assert(pckl.value.mkString("[", ",", "]") === "[0,0,0,40,115,99,97,108,97,46,84,117,112,108,101,50,91,106,97,118,97,46,108,97,110,103,46,83,116,114,105,110,103,44,115,99,97,108,97,46,73,110,116,93,0,0,0,6,104,101,119,114,111,119,0,0,0,2]")
    assert(pckl.unpickle[(String, Int)] === tup2)

    val tup3 = ("hewrow", 2, "bye")
    val pckl3 = tup3.pickle
    assert(pckl3.unpickle[(String, Int, String)] === tup3)
  }
}

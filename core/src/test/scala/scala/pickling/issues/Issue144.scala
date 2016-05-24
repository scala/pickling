package scala.pickling.issues

import org.scalatest.FunSuite

import scala.pickling._
import Defaults._
import binary._
import static._

class Issue144 extends FunSuite {

  test("pickle/unpickle right-nested tuple as Any should work") {
    val t1: Any = ((1, 2), 1)
    val sA: Array[Byte] = t1.pickle.value
    val t2: Any = sA.unpickle[Any]
    assert(t1 === t2)
  }

  test("pickle/unpickle left-nested tuple as Any should work") {
    val t1: Any = (1, (1, 2))
    val sB: Array[Byte] = t1.pickle.value
    val t2: Any = sB.unpickle[Any]
    assert(t1 === t2)
  }
}

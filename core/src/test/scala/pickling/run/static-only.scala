package scala.pickling.staticonly

import org.scalatest.FunSuite
import scala.pickling._
import json._
import static.StaticOnly

sealed abstract class C { val fld: Int }

final class D extends C { val fld = 1 }

final class E extends C {
  val fld = 2
  def incr(x: Int) = x + 1
}

class StaticOnlyTest extends FunSuite {
  test("main") {
    val x: C = new D
    val pickle: JSONPickle = x.pickle
    assert(pickle.unpickle[C].fld == 1)
  }
}

package scala.pickling.pickler

import org.scalatest.FunSuite
import scala.pickling._, Defaults._, json._

class FastTypeTagTest extends FunSuite {
  test("simpleTag") {
    val x = FastTypeTag[String]
    val y = x.pickle.unpickle[FastTypeTag[String]]
    assert(x == y)
  }
  test("unpickleAny") {
    val x = FastTypeTag[String]
    val y = x.pickle.unpickle[Any]
    assert(x == y)
  }
}

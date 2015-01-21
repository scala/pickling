package scala.pickling.runtimenulltest

import org.scalatest.FunSuite

class NullRuntimeTest extends FunSuite {
  test("json") {
    import scala.pickling._, scala.pickling.Defaults._, json._
    val n: Any = null
    val p: JSONPickle = n.pickle
    val up = p.unpickle[Any]
    assert(n === up)
  }

  test("binary") {
    import scala.pickling._, scala.pickling.Defaults._, binary._
    val n: Any = null
    val p: BinaryPickle = n.pickle
    val up = p.unpickle[Any]
    assert(n === up)
  }
}

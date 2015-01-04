package scala.pickling.runtimenulltest

import org.scalatest.FunSuite

class NullRuntimeTest extends FunSuite {
  test("json") {
    import scala.pickling._
    import json._
    import AllPicklers._
    val n: Any = null
    val p: JSONPickle = n.pickle
    val up = p.unpickle[Any]
    assert(n === up)
  }

  test("binary") {
    import scala.pickling._
    import binary._
    import AllPicklers._
    val n: Any = null
    val p: BinaryPickle = n.pickle
    val up = p.unpickle[Any]
    assert(n === up)
  }
}

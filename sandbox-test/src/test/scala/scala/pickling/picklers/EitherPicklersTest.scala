package scala.pickling.picklers

import org.scalatest.FunSuite

import scala.pickling.JsonPicklingProtocol

class EitherPicklersTest extends FunSuite {

  import JsonPicklingProtocol._

  test("pickle `left` when type is unknown") {
    val l: Any = Left[Int, String](1)
    val up = l.pickle.unpickle[Any]
    assert(l == up)
  }

  test("pickle `right` when type is unknown") {
    val r: Any = Right[Int, String]("hello")
    val up = r.pickle.unpickle[Any]
    assert(r == up)
  }

  test("pickle `either` when type is unknown") {

    val l: Any = Left[Int, String](1)
    val up1 = l.pickle.unpickle[Any]
    assert(l == up1)

    val r: Any = Right[Int, String]("hello")
    val up2 = r.pickle.unpickle[Any]
    assert(r == up2)

  }

}

package scala.pickling.test.issue50

import scala.pickling._
import scala.pickling.json._

import org.scalatest.FunSuite

case class TestA(x: Option[Int])
case class TestB(x: Option[String])

class Issue50Test extends FunSuite {
  test("Issue #50") {
    val a = TestA(Some(1))
    val pa = a.pickle
    assert(pa.unpickle[TestA] == a)

    val b = TestB(Some("hello"))
    val pb = b.pickle
    assert(pb.unpickle[TestB] == b)
  }
}
